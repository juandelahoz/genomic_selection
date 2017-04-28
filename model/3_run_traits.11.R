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
		cors$DPM.13[i,"BayesA"]  = round( runBayesA(       y3$DPM.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DPM.13 , "../../mod/results/cors_13_DPM.txt",   sep="\t", row.names=FALSE)

		model= "BayesB"
		cors$DPM.13[i,"BayesB"]  = round( runBayesB(       y3$DPM.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DPM.13 , "../../mod/results/cors_13_DPM.txt",   sep="\t", row.names=FALSE)

		model= "BayesC"
		cors$DPM.13[i,"BayesC"]  = round( runBayesC(       y3$DPM.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DPM.13 , "../../mod/results/cors_13_DPM.txt",   sep="\t", row.names=FALSE)

		model= "BayesRR"
		cors$DPM.13[i,"BayesRR"] = round( runBayesRR(      y3$DPM.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DPM.13 , "../../mod/results/cors_13_DPM.txt",   sep="\t", row.names=FALSE)

		model= "BLasso"
		cors$DPM.13[i,"BLasso"]  = round( runBayesLASSO(   y3$DPM.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DPM.13 , "../../mod/results/cors_13_DPM.txt",   sep="\t", row.names=FALSE)

		model= "BLassof"
		cors$DPM.13[i,"BLassof"] = round( runBLassoFast(   y3$DPM.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DPM.13 , "../../mod/results/cors_13_DPM.txt",   sep="\t", row.names=FALSE)

		model= "FIXED"
		cors$DPM.13[i,"FIXED"]   = round( runFIXED(        y3$DPM.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DPM.13 , "../../mod/results/cors_13_DPM.txt",   sep="\t", row.names=FALSE)

		model= "RKHS"
		cors$DPM.13[i,"RKHS"]    = round( runRKHS(         y3$DPM.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DPM.13 , "../../mod/results/cors_13_DPM.txt",   sep="\t", row.names=FALSE)

		model= "GBLUP"
		cors$DPM.13[i,"GBLUP"]   = round( runGBLUP(        y3$DPM.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DPM.13 , "../../mod/results/cors_13_DPM.txt",   sep="\t", row.names=FALSE)

#		model= "rrBLUP"
#		cors$DPM.13[i,"rrBLUP"]  = round( runrrBLUP(       y3$DPM.13.P , X3 , combinat_13[,i] )$cor ,5)
#		write.table( cors$DPM.13 , "../../mod/results/cors_13_DPM.txt",   sep="\t", row.names=FALSE)

	}
