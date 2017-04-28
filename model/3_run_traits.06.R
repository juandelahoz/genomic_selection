#!/usr/bin/Rscript

## RUN YDHA c ##
source("1_prepare_models.R")
library(BGLR)
set.seed(1234)

# load models
	source("Bayes_A.R")
	source("Bayes_B.R")
	source("Bayes_C.R")
	source("Bayes_RidgeRegression.R")
	source("Bayes_LASSO.R")
	source("Bayes_LASSO_fast.R")
	source("FIXED.R")
	source("RKHS.R")
	source("GBLUP.R")
#	source("rrBLUP.R")

# run all combinations
	for (i in 1:100){

		model= "BayesA"
		cors$DPM.14[i,"BayesA"]  = round( runBayesA(       y4$DPM.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$DPM.14 , "../../mod/results/cors_14_DPM.txt",   sep="\t", row.names=FALSE)

		model= "BayesB"
		cors$DPM.14[i,"BayesB"]  = round( runBayesB(       y4$DPM.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$DPM.14 , "../../mod/results/cors_14_DPM.txt",   sep="\t", row.names=FALSE)

		model= "BayesC"
		cors$DPM.14[i,"BayesC"]  = round( runBayesC(       y4$DPM.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$DPM.14 , "../../mod/results/cors_14_DPM.txt",   sep="\t", row.names=FALSE)

		model= "BayesRR"
		cors$DPM.14[i,"BayesRR"] = round( runBayesRR(      y4$DPM.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$DPM.14 , "../../mod/results/cors_14_DPM.txt",   sep="\t", row.names=FALSE)

		model= "BLasso"
		cors$DPM.14[i,"BLasso"]  = round( runBayesLASSO(   y4$DPM.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$DPM.14 , "../../mod/results/cors_14_DPM.txt",   sep="\t", row.names=FALSE)

		model= "BLassof"
		cors$DPM.14[i,"BLassof"] = round( runBLassoFast(   y4$DPM.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$DPM.14 , "../../mod/results/cors_14_DPM.txt",   sep="\t", row.names=FALSE)

		model= "FIXED"
		cors$DPM.14[i,"FIXED"]   = round( runFIXED(        y4$DPM.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$DPM.14 , "../../mod/results/cors_14_DPM.txt",   sep="\t", row.names=FALSE)

		model= "RKHS"
		cors$DPM.14[i,"RKHS"]    = round( runRKHS(         y4$DPM.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$DPM.14 , "../../mod/results/cors_14_DPM.txt",   sep="\t", row.names=FALSE)

		model= "GBLUP"
		cors$DPM.14[i,"GBLUP"]   = round( runGBLUP(        y4$DPM.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$DPM.14 , "../../mod/results/cors_14_DPM.txt",   sep="\t", row.names=FALSE)

#		model= "rrBLUP"
#		cors$DPM.14[i,"rrBLUP"]  = round( runrrBLUP(       y4$DPM.14.P , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$DPM.14 , "../../mod/results/cors_14_DPM.txt",   sep="\t", row.names=FALSE)

	}
