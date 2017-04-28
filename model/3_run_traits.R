
## RUN YDHA c ##

library(BGLR)
set.seed(1234)

# load models
	source("Bayes_A.R")
	source("Bayes_B.R")
	source("Bayes_C.R")
	source("Bayes_LASSO.R")
	source("Bayes_LASSO_fast.R")
	source("Bayes_RidgeRegression.R")
	source("GBLUP.R")
	source("RKHS.R")
	source("FIXED.R")
#	source("rrBLUP.R")

# run all combinations
	for (i in 1:100){

		cors$YDHAc.14[i,"BayesA"]  = round( runBayesA(       y4$YDHAc.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHAc.14 , "../../mod/results/cors_14_YDHAc.txt",   sep="\t", row.names=FALSE)

		cors$YDHAc.14[i,"BayesB"]  = round( runBayesB(       y4$YDHAc.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHAc.14 , "../../mod/results/cors_14_YDHAc.txt",   sep="\t", row.names=FALSE)

		cors$YDHAc.14[i,"BayesC"]  = round( runBayesC(       y4$YDHAc.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHAc.14 , "../../mod/results/cors_14_YDHAc.txt",   sep="\t", row.names=FALSE)

		cors$YDHAc.14[i,"BayesRR"] = round( runBayesRR(      y4$YDHAc.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHAc.14 , "../../mod/results/cors_14_YDHAc.txt",   sep="\t", row.names=FALSE)

		cors$YDHAc.14[i,"BLasso"]  = round( runBayesLASSO(   y4$YDHAc.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHAc.14 , "../../mod/results/cors_14_YDHAc.txt",   sep="\t", row.names=FALSE)

		cors$YDHAc.14[i,"BLassof"] = round( runBLassoFast(   y4$YDHAc.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHAc.14 , "../../mod/results/cors_14_YDHAc.txt",   sep="\t", row.names=FALSE)

		cors$YDHAc.14[i,"FIXED"]   = round( runFIXED(        y4$YDHAc.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHAc.14 , "../../mod/results/cors_14_YDHAc.txt",   sep="\t", row.names=FALSE)

		cors$YDHAc.14[i,"RKHS"]    = round( runRKHS(         y4$YDHAc.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHAc.14 , "../../mod/results/cors_14_YDHAc.txt",   sep="\t", row.names=FALSE)

		cors$YDHAc.14[i,"GBLUP"]   = round( runGBLUP(        y4$YDHAc.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHAc.14 , "../../mod/results/cors_14_YDHAc.txt",   sep="\t", row.names=FALSE)

#		cors$YDHAc.14[i,"rrBLUP"]  = round( runrrBLUP(       y4$YDHAc.14.P , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$YDHAc.14 , "../../mod/results/cors_14_YDHAc.txt",   sep="\t", row.names=FALSE)

	}
