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
			cors$DPM.14[i,prior]     = round( runBGLR( y4$DPM.14.P   , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$DPM.14     , "../../mod/results/cors_14_DPM.txt",     sep="\t", row.names=FALSE)
		}
	}
