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
		cors$DF.13[i,"BayesA"]  = round( runBayesA(       y3$DF.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DF.13 , "../../mod/results/cors_13_DF.txt",   sep="\t", row.names=FALSE)

		model= "BayesB"
		cors$DF.13[i,"BayesB"]  = round( runBayesB(       y3$DF.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DF.13 , "../../mod/results/cors_13_DF.txt",   sep="\t", row.names=FALSE)

		model= "BayesC"
		cors$DF.13[i,"BayesC"]  = round( runBayesC(       y3$DF.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DF.13 , "../../mod/results/cors_13_DF.txt",   sep="\t", row.names=FALSE)

		model= "BayesRR"
		cors$DF.13[i,"BayesRR"] = round( runBayesRR(      y3$DF.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DF.13 , "../../mod/results/cors_13_DF.txt",   sep="\t", row.names=FALSE)

		model= "BLasso"
		cors$DF.13[i,"BLasso"]  = round( runBayesLASSO(   y3$DF.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DF.13 , "../../mod/results/cors_13_DF.txt",   sep="\t", row.names=FALSE)

		model= "BLassof"
		cors$DF.13[i,"BLassof"] = round( runBLassoFast(   y3$DF.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DF.13 , "../../mod/results/cors_13_DF.txt",   sep="\t", row.names=FALSE)

		model= "FIXED"
		cors$DF.13[i,"FIXED"]   = round( runFIXED(        y3$DF.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DF.13 , "../../mod/results/cors_13_DF.txt",   sep="\t", row.names=FALSE)

		model= "RKHS"
		cors$DF.13[i,"RKHS"]    = round( runRKHS(         y3$DF.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DF.13 , "../../mod/results/cors_13_DF.txt",   sep="\t", row.names=FALSE)

		model= "GBLUP"
		cors$DF.13[i,"GBLUP"]   = round( runGBLUP(        y3$DF.13.P , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$DF.13 , "../../mod/results/cors_13_DF.txt",   sep="\t", row.names=FALSE)

#		model= "rrBLUP"
#		cors$DF.13[i,"rrBLUP"]  = round( runrrBLUP(       y3$DF.13.P , X3 , combinat_13[,i] )$cor ,5)
#		write.table( cors$DF.13 , "../../mod/results/cors_13_DF.txt",   sep="\t", row.names=FALSE)

	}
