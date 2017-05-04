# load models
	library(BGLR)
	source("1_prepare_data.R")
	source("2_prepare_models.R")
	model= c( "BayesA", "BayesB", "BayesC", "BayesRR", "BLasso", "BLassof", "RKHS", "GBLUP", "FIXED" )

# run all combinations
	for (i in 1:100){
		for (prior in model){
			print(paste("log: running model",prior,"on random population",i,"- Started:",Sys.time()))
			set.seed(1234)
			
	# 2013
			cors$DF.13.o[i,prior]    = round( runBGLR( y3$DF.13.old   , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$DF.13.o   , "../../mod/results/cors_13_DF_o.txt",   sep="\t", row.names=FALSE)

			cors$DPM.13.o[i,prior]   = round( runBGLR( y3$DPM.13.old  , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$DPM.13.o  , "../../mod/results/cors_13_DPM_o.txt",  sep="\t", row.names=FALSE)

			cors$HSW.13.o[i,prior]   = round( runBGLR( y3$HSW.13.old  , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$HSW.13.o  , "../../mod/results/cors_13_HSW_o.txt",  sep="\t", row.names=FALSE)

			cors$YDHA.13.o[i,prior]  = round( runBGLR( y3$YDHA.13.old , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$YDHA.13.o , "../../mod/results/cors_13_YDHA_o.txt", sep="\t", row.names=FALSE)

	# new
			cors$DF.13[i,prior]      = round( runBGLR( y3$DF.13.P     , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$DF.13     , "../../mod/results/cors_13_DF.txt",     sep="\t", row.names=FALSE)

			cors$DPM.13[i,prior]     = round( runBGLR( y3$DPM.13.P    , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$DPM.13    , "../../mod/results/cors_13_DPM.txt",    sep="\t", row.names=FALSE)

			cors$HSW.13[i,prior]     = round( runBGLR( y3$HSW.13.P    , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$HSW.13    , "../../mod/results/cors_13_HSW.txt",    sep="\t", row.names=FALSE)

			cors$YDHA.13[i,prior]    = round( runBGLR( y3$YDHA.13.P   , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$YDHA.13   , "../../mod/results/cors_13_YDHA.txt",   sep="\t", row.names=FALSE)

	# 2014
			cors$DF.14.o[i,prior]    = round( runBGLR( y4$DF.14.old   , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$DF.14.o    , "../../mod/results/cors_14_DF_o.txt",    sep="\t", row.names=FALSE)

			cors$DPM.14.o[i,prior]   = round( runBGLR( y4$DPM.14.old  , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$DPM.14.o   , "../../mod/results/cors_14_DPM_o.txt",   sep="\t", row.names=FALSE)

			cors$HSW.14.o[i,prior]   = round( runBGLR( y4$HSW.14.old  , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$HSW.14.o   , "../../mod/results/cors_14_HSW_o.txt",   sep="\t", row.names=FALSE)

			cors$YDHA.14.o[i,prior]  = round( runBGLR( y4$YDHA.14.old , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHA.14.o  , "../../mod/results/cors_14_YDHA_o.txt",  sep="\t", row.names=FALSE)

	# new
			cors$DF.14[i,prior]      = round( runBGLR( y4$DF.14.P    , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$DF.14      , "../../mod/results/cors_14_DF.txt",      sep="\t", row.names=FALSE)

			cors$DPM.14[i,prior]     = round( runBGLR( y4$DPM.14.P   , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$DPM.14     , "../../mod/results/cors_14_DPM.txt",     sep="\t", row.names=FALSE)

			cors$HSW.14[i,prior]     = round( runBGLR( y4$HSW.14.P   , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$HSW.14     , "../../mod/results/cors_14_HSW.txt",     sep="\t", row.names=FALSE)

			cors$Fe.14[i,prior]      = round( runBGLR( y4$Fe.14.s    , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$Fe.14      , "../../mod/results/cors_14_Fe.txt",      sep="\t", row.names=FALSE)

			cors$Zn.14[i,prior]      = round( runBGLR( y4$Zn.14.s    , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$Zn.14      , "../../mod/results/cors_14_Zn.txt",      sep="\t", row.names=FALSE)

			cors$YDHAa.14[i,prior]   = round( runBGLR( y4$YDHAa.14.P , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHAa.14   , "../../mod/results/cors_14_YDHAa.txt",   sep="\t", row.names=FALSE)

			cors$YDHAb.14[i,prior]   = round( runBGLR( y4$YDHAb.14.P , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHAb.14   , "../../mod/results/cors_14_YDHAb.txt",   sep="\t", row.names=FALSE)

			cors$YDHAc.14[i,prior]   = round( runBGLR( y4$YDHAc.14.P , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHAc.14   , "../../mod/results/cors_14_YDHAc.txt",   sep="\t", row.names=FALSE)

			cors$YDPLa.14[i,prior]   = round( runBGLR( y4$YDPLa.14.P , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDPLa.14   , "../../mod/results/cors_14_YDPLa.txt",   sep="\t", row.names=FALSE)

			cors$YDPLb.14[i,prior]   = round( runBGLR( y4$YDPLb.14.P , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDPLb.14   , "../../mod/results/cors_14_YDPLb.txt",   sep="\t", row.names=FALSE)

			cors$YDPLc.14[i,prior]   = round( runBGLR( y4$YDPLc.14.P , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDPLc.14   , "../../mod/results/cors_14_YDPLc.txt",   sep="\t", row.names=FALSE)

	# extreme values removed from filtered dataset
			cors$YDHAd.14[i,prior]   = round( runBGLR( y4$YDHAd.14.P_fe1pct , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHAd.14   , "../../mod/results/cors_14_YDHAd.txt",   sep="\t", row.names=FALSE)

			cors$YDHAd2p.14[i,prior] = round( runBGLR( y4$YDHAd.14.P_fe2pct , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHAd2p.14 , "../../mod/results/cors_14_YDHAd2p.txt", sep="\t", row.names=FALSE)

			cors$YDHAd3p.14[i,prior] = round( runBGLR( y4$YDHAd.14.P_fe3pct , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHAd3p.14 , "../../mod/results/cors_14_YDHAd3p.txt", sep="\t", row.names=FALSE)

			cors$YDHAd5p.14[i,prior] = round( runBGLR( y4$YDHAd.14.P_fe5pct , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHAd5p.14 , "../../mod/results/cors_14_YDHAd5p.txt", sep="\t", row.names=FALSE)

			cors$YDHAd10p.14[i,prior] = round( runBGLR( y4$YDHAd.14.P_fe10pct , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHAd10p.14 , "../../mod/results/cors_14_YDHAd10p.txt", sep="\t", row.names=FALSE)


			cors$YDPLd.14[i,prior]   = round( runBGLR( y4$YDPLd.14.P_fe1pct , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDPLd.14   , "../../mod/results/cors_14_YDPLd.txt",   sep="\t", row.names=FALSE)

			cors$YDPLd2p.14[i,prior] = round( runBGLR( y4$YDPLd.14.P_fe2pct , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDPLd2p.14 , "../../mod/results/cors_14_YDPLd2p.txt", sep="\t", row.names=FALSE)

			cors$YDPLd3p.14[i,prior] = round( runBGLR( y4$YDPLd.14.P_fe3pct , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDPLd3p.14 , "../../mod/results/cors_14_YDPLd3p.txt", sep="\t", row.names=FALSE)

			cors$YDPLd5p.14[i,prior] = round( runBGLR( y4$YDPLd.14.P_fe5pct , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDPLd5p.14 , "../../mod/results/cors_14_YDPLd5p.txt", sep="\t", row.names=FALSE)

			cors$YDPLd10p.14[i,prior] = round( runBGLR( y4$YDPLd.14.P_fe10pct , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDPLd10p.14 , "../../mod/results/cors_14_YDPLd10p.txt", sep="\t", row.names=FALSE)

	# extreme values removed from unfiltered dataset
			cors$YDHAe1p.13 [i,prior] = round( runBGLR( y3$YDHAe.13.P_fe1pct  , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$YDHAe1p.13  , "../../mod/results/cors_13_YDHAe1p.txt",  sep="\t", row.names=FALSE)

			cors$YDHAe2p.13 [i,prior] = round( runBGLR( y3$YDHAe.13.P_fe2pct  , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$YDHAe2p.13  , "../../mod/results/cors_13_YDHAe2p.txt",  sep="\t", row.names=FALSE)

			cors$YDHAe3p.13 [i,prior] = round( runBGLR( y3$YDHAe.13.P_fe3pct  , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$YDHAe3p.13  , "../../mod/results/cors_13_YDHAe3p.txt",  sep="\t", row.names=FALSE)

			cors$YDHAe5p.13 [i,prior] = round( runBGLR( y3$YDHAe.13.P_fe5pct  , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$YDHAe5p.13  , "../../mod/results/cors_13_YDHAe5p.txt",  sep="\t", row.names=FALSE)

			cors$YDHAe10p.13[i,prior] = round( runBGLR( y3$YDHAe.13.P_fe10pct , X3 , combinat_13[,i] , prior )$cor ,5)
			write.table( cors$YDHAe10p.13 , "../../mod/results/cors_13_YDHAe10p.txt", sep="\t", row.names=FALSE)


			cors$YDHAe1p.14 [i,prior] = round( runBGLR( y4$YDHAe.14.P_fe1pct  , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHAe1p.14  , "../../mod/results/cors_14_YDHAe1p.txt",  sep="\t", row.names=FALSE)

			cors$YDHAe2p.14 [i,prior] = round( runBGLR( y4$YDHAe.14.P_fe2pct  , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHAe2p.14  , "../../mod/results/cors_14_YDHAe2p.txt",  sep="\t", row.names=FALSE)

			cors$YDHAe3p.14 [i,prior] = round( runBGLR( y4$YDHAe.14.P_fe3pct  , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHAe3p.14  , "../../mod/results/cors_14_YDHAe3p.txt",  sep="\t", row.names=FALSE)

			cors$YDHAe5p.14 [i,prior] = round( runBGLR( y4$YDHAe.14.P_fe5pct  , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHAe5p.14  , "../../mod/results/cors_14_YDHAe5p.txt",  sep="\t", row.names=FALSE)

			cors$YDHAe10p.14[i,prior] = round( runBGLR( y4$YDHAe.14.P_fe10pct , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDHAe10p.14 , "../../mod/results/cors_14_YDHAe10p.txt", sep="\t", row.names=FALSE)


			cors$YDPLe1p.14 [i,prior] = round( runBGLR( y4$YDPLe.14.P_fe1pct  , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDPLe1p.14  , "../../mod/results/cors_14_YDPLe1p.txt",  sep="\t", row.names=FALSE)

			cors$YDPLe2p.14 [i,prior] = round( runBGLR( y4$YDPLe.14.P_fe2pct  , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDPLe2p.14  , "../../mod/results/cors_14_YDPLe2p.txt",  sep="\t", row.names=FALSE)

			cors$YDPLe3p.14 [i,prior] = round( runBGLR( y4$YDPLe.14.P_fe3pct  , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDPLe3p.14  , "../../mod/results/cors_14_YDPLe3p.txt",  sep="\t", row.names=FALSE)

			cors$YDPLe5p.14 [i,prior] = round( runBGLR( y4$YDPLe.14.P_fe5pct  , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDPLe5p.14  , "../../mod/results/cors_14_YDPLe5p.txt",  sep="\t", row.names=FALSE)

			cors$YDPLe10p.14[i,prior] = round( runBGLR( y4$YDPLe.14.P_fe10pct , X4 , combinat_14[,i] , prior )$cor ,5)
			write.table( cors$YDPLe10p.14 , "../../mod/results/cors_14_YDPLe10p.txt", sep="\t", row.names=FALSE)

		}
	}
