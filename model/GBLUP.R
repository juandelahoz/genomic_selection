runGBLUP <- function(y, X, combinations)
{
	correlations = rep(NA,100)
	ETA = list(list(model="RKHS", K=G))

 	for( i in 1:100 ){	
 		yNA = y
		indexNA = combinations[,i]==0
		yNA[indexNA] = NA
		fm = BGLR(y=yNA, ETA=ETA, nIter= 30000, burnIn= 5000, verbose=FALSE)
		correlations[i] = cor(fm$yHat[indexNA], y[indexNA])
	}
	return(correlations)
}

corre.13.YDHA$GBLUP = runGBLUP(phenotypes13$YDHA$BLUP[G.P.13,1], X, combinations13)
corre.14.YDHA$GBLUP = runGBLUP(phenotypes14$YDHA$BLUP[G.P.14,1], X, combinations14)
