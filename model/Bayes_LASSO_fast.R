runBayesLassoFast <- function(y, X, combinations)
{
	correlations = rep(NA,100)
	L = svd(G)
	Lm = L$u %*% diag(L$d)^(1/2)
#	dim(Lm)
#	plot(L$d)
	ETA = list(list(model="BL", X=Lm))

	for( i in 1:100 ){	
		yNA = y
		indexNA = combinations[,i]==0
		yNA[indexNA] = NA
		fm = BGLR(y=yNA, ETA=ETA, nIter=30000, burnIn=5000, verbose=FALSE)
		correlations[i] = cor(fm$yHat[indexNA], y[indexNA])
	}
	return(correlations)
}

corre.13.YDHA$BLf = runBayesLassoFast(phenotypes13$YDHA$BLUP[G.P.13,1], X, combinations13)
corre.14.YDHA$BLf = runBayesLassoFast(phenotypes14$YDHA$BLUP[G.P.14,1], X, combinations14)
