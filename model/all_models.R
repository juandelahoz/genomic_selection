 # y = vector of values for the observed trait
 # X = X matrix (samples x markers (-1, 0, 1))
 # pop.split = random permutations of samples
runBayesA <- function(y, X, pop.split)
{
	ETA = list(list(model="BayesA", X=X))
	yNA = y
	iNA = which(pop.split == 1)
	yNA[iNA] = NA
	
	fm = BGLR( y=yNA, ETA=ETA, 
		nIter=30000, burnIn=5000, thin=10, 
		verbose=FALSE, saveAt="results/")
	
	return( list( 
		result=fm, 
		cor=cor(fm$yHat[iNA], y[iNA]) 
		) )
}

runBayesB <- function(y, X, pop.split)
{
	ETA = list(list(model="BayesB", X=X))
	yNA = y
	iNA = which(pop.split == 1)
	yNA[iNA] = NA
	
	fm = BGLR( y=yNA, ETA=ETA, 
		nIter=30000, burnIn=5000, thin=10, 
		verbose=FALSE, saveAt="results/")
	
	return( list( 
		result=fm, 
		cor=cor(fm$yHat[iNA], y[iNA]) 
		) )
}

runBayesC <- function(y, X, pop.split)
{
	ETA = list(list(model="BayesC", X=X))
	yNA = y
	iNA = which(pop.split == 1)
	yNA[iNA] = NA
	
	fm = BGLR( y=yNA, ETA=ETA, 
		nIter=30000, burnIn=5000, thin=10, 
		verbose=FALSE, saveAt="results/")
	
	return( list( 
		result=fm, 
		cor=cor(fm$yHat[iNA], y[iNA]) 
		) )
}

runBayesLASSO <- function(y, X, pop.split)
{
	ETA = list(list(model="BL", X=X))
	yNA = y
	iNA = which(pop.split == 1)
	yNA[iNA] = NA
	
	fm = BGLR( y=yNA, ETA=ETA, 
		nIter=30000, burnIn=5000, thin=10, 
		verbose=FALSE, saveAt="results/")
	
	return( list( 
		result=fm, 
		cor=cor(fm$yHat[iNA], y[iNA]) 
		) )
}
