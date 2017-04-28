runRKHS <- function(y, X, pop.split)
{
	D = as.matrix( dist( X, method="euclidean")) ^2
	D = D / mean(D)
	h = 0.5
	K = exp(-h * D)
	ETA = list(list(model="RKHS", K=K))
	yNA = y
	iNA = which(pop.split == 1)
	yNA[iNA] = NA
	
	fm = BGLR( y=yNA, ETA=ETA, 
		nIter=30000, burnIn=5000, thin=10, 
		verbose=FALSE, saveAt="../../mod/results/")
	
	return( list( 
		result=fm, 
		cor=cor(fm$yHat[iNA], y[iNA]) 
		) )
}
 # y = vector of values for the observed trait
 # X = X matrix (samples x markers (-1, 0, 1))
 # pop.split = random permutations of samples
