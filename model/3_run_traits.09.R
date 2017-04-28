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
		cors$Zn.14[i,"BayesA"]  = round( runBayesA(       y4$Zn.14.s , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$Zn.14 , "../../mod/results/cors_14_Zn.txt",   sep="\t", row.names=FALSE)

		model= "BayesB"
		cors$Zn.14[i,"BayesB"]  = round( runBayesB(       y4$Zn.14.s , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$Zn.14 , "../../mod/results/cors_14_Zn.txt",   sep="\t", row.names=FALSE)

		model= "BayesC"
		cors$Zn.14[i,"BayesC"]  = round( runBayesC(       y4$Zn.14.s , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$Zn.14 , "../../mod/results/cors_14_Zn.txt",   sep="\t", row.names=FALSE)

		model= "BayesRR"
		cors$Zn.14[i,"BayesRR"] = round( runBayesRR(      y4$Zn.14.s , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$Zn.14 , "../../mod/results/cors_14_Zn.txt",   sep="\t", row.names=FALSE)

		model= "BLasso"
		cors$Zn.14[i,"BLasso"]  = round( runBayesLASSO(   y4$Zn.14.s , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$Zn.14 , "../../mod/results/cors_14_Zn.txt",   sep="\t", row.names=FALSE)

		model= "BLassof"
		cors$Zn.14[i,"BLassof"] = round( runBLassoFast(   y4$Zn.14.s , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$Zn.14 , "../../mod/results/cors_14_Zn.txt",   sep="\t", row.names=FALSE)

		model= "FIXED"
		cors$Zn.14[i,"FIXED"]   = round( runFIXED(        y4$Zn.14.s , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$Zn.14 , "../../mod/results/cors_14_Zn.txt",   sep="\t", row.names=FALSE)

		model= "RKHS"
		cors$Zn.14[i,"RKHS"]    = round( runRKHS(         y4$Zn.14.s , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$Zn.14 , "../../mod/results/cors_14_Zn.txt",   sep="\t", row.names=FALSE)

		model= "GBLUP"
		cors$Zn.14[i,"GBLUP"]   = round( runGBLUP(        y4$Zn.14.s , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$Zn.14 , "../../mod/results/cors_14_Zn.txt",   sep="\t", row.names=FALSE)

#		model= "rrBLUP"
#		cors$Zn.14[i,"rrBLUP"]  = round( runrrBLUP(       y4$Zn.14.s , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$Zn.14 , "../../mod/results/cors_14_Zn.txt",   sep="\t", row.names=FALSE)

	}
