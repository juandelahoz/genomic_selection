# load models
	library(BGLR)
	source("1_prepare_data.R")
	source("2_prepare_models.R")
	model= c( "BayesA", "BayesB", "BayesC", "BLasso", "BLassof", "RKHS", "GBLUP" )

# run all combinations
	for (i in 1:100){
		for (prior in model){
			print(paste("log: running model",prior,"on random population",i,"- Started:",Sys.time()))
			set.seed(1234)
			cors$YDHA.13.o[i,prior]  = round( runBGLR( y3$YDHA.13.old , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$YDHA.13.o  , "../../mod/results/cors_13_YDHA_o.txt", sep="\t", row.names=FALSE)
		}
	}
