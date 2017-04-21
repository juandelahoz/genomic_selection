runRKHS <- function(y, X, combinations)
{
	correlations = rep(NA,100)
	D = as.matrix( dist( X, method="euclidean")) ^2
	D = D / mean(D)
	h = 0.5
	K = exp(-h * D)
	ETA = list(list(model="RKHS", K=K))

	for( i in 1:100 ){
		yNA = y
		indexNA = combinations[,i]==0
		yNA[indexNA] = NA
		fm = BGLR(y=yNA, ETA=ETA, nIter= 30000, burnIn= 5000)
		correlations[i] = cor(fm$yHat[indexNA], y[indexNA])
	}
	return(correlations)
}

corre.13.YDHA$RKHS = runRKHS(phenotypes13$YDHA$BLUP[G.P.13,1], X, combinations13)
corre.14.YDHA$RKHS = runRKHS(phenotypes14$YDHA$BLUP[G.P.14,1], X, combinations14)
corre.13.YDHA$BLf = runBayesLassoFast(phenotypes13$YDHA$BLUP[G.P.13,1], X, combinations13)
corre.14.YDHA$BLf = runBayesLassoFast(phenotypes14$YDHA$BLUP[G.P.14,1], X, combinations14)
corre.13.YDHA$GBLUP = runGBLUP(phenotypes13$YDHA$BLUP[G.P.13,1], X, combinations13)
corre.14.YDHA$GBLUP = runGBLUP(phenotypes14$YDHA$BLUP[G.P.14,1], X, combinations14)
