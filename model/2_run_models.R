library(BGLR)
set.seed(1234)
# run Bayes A
	source("Bayes_A.R")
	for (i in 1:100){
		
# 2013
#		cors$DF.13.o[i,"BayesA"]    = round( runBayesA( y3$DF.13.old   , X3 , combinat_13[,i] )$cor ,5)
#		write.table( cors$DF.13.o   , "../../mod/Bayes_A/cors_13_DF_o.txt",   sep="\t", row.names=FALSE)

#		cors$DPM.13.o[i,"BayesA"]   = round( runBayesA( y3$DPM.13.old  , X3 , combinat_13[,i] )$cor ,5)
#		write.table( cors$DPM.13.o  , "../../mod/Bayes_A/cors_13_DPM_o.txt",  sep="\t", row.names=FALSE)

		cors$HSW.13.o[i,"BayesA"]   = round( runBayesA( y3$HSW.13.old  , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$HSW.13.o  , "../../mod/Bayes_A/cors_13_HSW_o.txt",  sep="\t", row.names=FALSE)

		cors$YDHA.13.o[i,"BayesA"]  = round( runBayesA( y3$YDHA.13.old , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$YDHA.13.o , "../../mod/Bayes_A/cors_13_YDHA_o.txt", sep="\t", row.names=FALSE)

# new
#		cors$DF.13[i,"BayesA"]      = round( runBayesA( y3$DF.13.P     , X3 , combinat_13[,i] )$cor ,5)
#		write.table( cors$DF.13     , "../../mod/Bayes_A/cors_13_DF.txt",     sep="\t", row.names=FALSE)

#		cors$DPM.13[i,"BayesA"]     = round( runBayesA( y3$DPM.13.P    , X3 , combinat_13[,i] )$cor ,5)
#		write.table( cors$DPM.13    , "../../mod/Bayes_A/cors_13_DPM.txt",    sep="\t", row.names=FALSE)

		cors$HSW.13[i,"BayesA"]     = round( runBayesA( y3$HSW.13.P    , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$HSW.13    , "../../mod/Bayes_A/cors_13_HSW.txt",    sep="\t", row.names=FALSE)

		cors$YDHA.13[i,"BayesA"]    = round( runBayesA( y3$YDHA.13.P   , X3 , combinat_13[,i] )$cor ,5)
		write.table( cors$YDHA.13   , "../../mod/Bayes_A/cors_13_YDHA.txt",   sep="\t", row.names=FALSE)

# 2014
#		cors$DF.14.o[i,"BayesA"]    = round( runBayesA( y4$DF.14.old   , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$DF.14.o    , "../../mod/Bayes_A/cors_14_DF_o.txt",    sep="\t", row.names=FALSE)

#		cors$DPM.14.o[i,"BayesA"]   = round( runBayesA( y4$DPM.14.old  , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$DPM.14.o   , "../../mod/Bayes_A/cors_14_DPM_o.txt",   sep="\t", row.names=FALSE)

		cors$HSW.14.o[i,"BayesA"]   = round( runBayesA( y4$HSW.14.old  , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$HSW.14.o   , "../../mod/Bayes_A/cors_14_HSW_o.txt",   sep="\t", row.names=FALSE)

		cors$YDHA.14.o[i,"BayesA"]  = round( runBayesA( y4$YDHA.14.old , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHA.14.o  , "../../mod/Bayes_A/cors_14_YDHA_o.txt",  sep="\t", row.names=FALSE)

# new
#		cors$DF.14[i,"BayesA"]      = round( runBayesA( y4$DF.14.P    , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$DF.14      , "../../mod/Bayes_A/cors_14_DF.txt",      sep="\t", row.names=FALSE)

#		cors$DPM.14[i,"BayesA"]     = round( runBayesA( y4$DPM.14.P   , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$DPM.14     , "../../mod/Bayes_A/cors_14_DPM.txt",     sep="\t", row.names=FALSE)

		cors$HSW.14[i,"BayesA"]     = round( runBayesA( y4$HSW.14.P   , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$HSW.14     , "../../mod/Bayes_A/cors_14_HSW.txt",     sep="\t", row.names=FALSE)

#		cors$Fe.14[i,"BayesA"]      = round( runBayesA( y4$Fe.14.s    , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$Fe.14      , "../../mod/Bayes_A/cors_14_Fe.txt",      sep="\t", row.names=FALSE)

#		cors$Zn.14[i,"BayesA"]      = round( runBayesA( y4$Zn.14.s    , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$Zn.14      , "../../mod/Bayes_A/cors_14_Zn.txt",      sep="\t", row.names=FALSE)

		cors$YDHAa.14[i,"BayesA"]   = round( runBayesA( y4$YDHAa.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHAa.14   , "../../mod/Bayes_A/cors_14_YDHAa.txt",   sep="\t", row.names=FALSE)

		cors$YDHAb.14[i,"BayesA"]   = round( runBayesA( y4$YDHAb.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHAb.14   , "../../mod/Bayes_A/cors_14_YDHAb.txt",   sep="\t", row.names=FALSE)

		cors$YDHAc.14[i,"BayesA"]   = round( runBayesA( y4$YDHAc.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHAc.14   , "../../mod/Bayes_A/cors_14_YDHAc.txt",   sep="\t", row.names=FALSE)

		cors$YDHAd.14[i,"BayesA"]   = round( runBayesA( y4$YDHAd.14.P_fe1pct , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDHAd.14   , "../../mod/Bayes_A/cors_14_YDHAd.txt",   sep="\t", row.names=FALSE)

		cors$YDPLa.14[i,"BayesA"]   = round( runBayesA( y4$YDPLa.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDPLa.14   , "../../mod/Bayes_A/cors_14_YDPLa.txt",   sep="\t", row.names=FALSE)

		cors$YDPLb.14[i,"BayesA"]   = round( runBayesA( y4$YDPLb.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDPLb.14   , "../../mod/Bayes_A/cors_14_YDPLb.txt",   sep="\t", row.names=FALSE)

		cors$YDPLc.14[i,"BayesA"]   = round( runBayesA( y4$YDPLc.14.P , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDPLc.14   , "../../mod/Bayes_A/cors_14_YDPLc.txt",   sep="\t", row.names=FALSE)

		cors$YDPLd.14[i,"BayesA"]   = round( runBayesA( y4$YDPLd.14.P_fe1pct , X4 , combinat_14[,i] )$cor ,5)
		write.table( cors$YDPLd.14   , "../../mod/Bayes_A/cors_14_YDPLd.txt",   sep="\t", row.names=FALSE)

# extreme values removed
#		cors$YDHAd2p.14[i,"BayesA"] = round( runBayesA( y4$YDHAd.14.P_fe2pct , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$YDHAd2p.14 , "../../mod/Bayes_A/cors_14_YDHAd2p.txt", sep="\t", row.names=FALSE)

#		cors$YDHAd3p.14[i,"BayesA"] = round( runBayesA( y4$YDHAd.14.P_fe3pct , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$YDHAd3p.14 , "../../mod/Bayes_A/cors_14_YDHAd3p.txt", sep="\t", row.names=FALSE)

#		cors$YDHAd5p.14[i,"BayesA"] = round( runBayesA( y4$YDHAd.14.P_fe5pct , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$YDHAd5p.14 , "../../mod/Bayes_A/cors_14_YDHAd5p.txt", sep="\t", row.names=FALSE)

#		cors$YDPLd2p.14[i,"BayesA"] = round( runBayesA( y4$YDPLd.14.P_fe2pct , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$YDPLd2p.14 , "../../mod/Bayes_A/cors_14_YDPLd2p.txt", sep="\t", row.names=FALSE)

#		cors$YDPLd3p.14[i,"BayesA"] = round( runBayesA( y4$YDPLd.14.P_fe3pct , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$YDPLd3p.14 , "../../mod/Bayes_A/cors_14_YDPLd3p.txt", sep="\t", row.names=FALSE)

#		cors$YDPLd5p.14[i,"BayesA"] = round( runBayesA( y4$YDPLd.14.P_fe5pct , X4 , combinat_14[,i] )$cor ,5)
#		write.table( cors$YDPLd5p.14 , "../../mod/Bayes_A/cors_14_YDPLd5p.txt", sep="\t", row.names=FALSE)

	}
