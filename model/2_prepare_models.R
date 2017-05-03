 # y = vector of values for the observed trait
 # X = X matrix (samples x markers (-1, 0, 1))
 # pop.split = random permutations of samples
 # model= c( "BayesA", "BayesB", "BayesC", "BayesRR", "BLasso", "BLassof", "FIXED", "RKHS", "GBLUP" )#,"rrBLUP")
runBGLR <- function(y, X, pop.split, model)
{
	       if (model=="BayesA"){
		ETA = list(list(model="BayesA", X=X))
	} else if (model=="BayesB") {
		ETA = list(list(model="BayesB", X=X))
	} else if (model=="BayesC") {
		ETA = list(list(model="BayesC", X=X))
	} else if (model=="BayesRR") {
		ETA = list(list(model="BRR",    X=X))
	} else if (model=="BLasso") {
		ETA = list(list(model="BL",     X=X))
	} else if (model=="FIXED") {
		ETA = list(list(model="FIXED",  X=X))
	} else if (model=="BLassof") {
		Z = scale(X)
		G = tcrossprod(Z) / ncol(Z)
		L = svd(G)
		Lm = L$u %*% diag(L$d)^(1/2)
		ETA = list(list(model="BL",     X=Lm))
	} else if (model=="RKHS") {
		D = as.matrix( dist( X, method="euclidean")) ^2
		D = D / mean(D)
		h = 0.5
		K = exp(-h * D)
		ETA = list(list(model="RKHS",   K=K))
	} else if (model=="GBLUP") {
		Z = scale(X)
		G = tcrossprod(Z) / ncol(Z)
		ETA = list(list(model="RKHS",   K=G))
	} else {
		print(paste("ERROR: model",model,"is not available. Continuing..."))
		return(list( result=NA, cor=NA))
	}

	yNA = y
	iNA = which(pop.split == 1)
	yNA[iNA] = NA
	
	fm = BGLR( y=yNA, ETA=ETA, 
		nIter=1000, burnIn=200, thin=10, 
		verbose=FALSE, saveAt="../../mod/results/")
	
	return( list( 
		result=fm, 
		cor=cor(fm$yHat[iNA], y[iNA], 
			use="complete.obs", method="pearson") 
		) )
}
