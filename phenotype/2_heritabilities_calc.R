 # packages
library(lme4)
library(lsmeans)

 # calculation of heritability
heritability <- function (trait, line, rep, blk)
{
	model = lmer( trait ~ (1|line) + (1|blk) + rep)
	var.g = as.vector( VarCorr( model )$line)
	var.e = attr( VarCorr( model ),"sc")^2

	# heritabilities!
	return(var.g/(var.g+(var.e/3)))
}

h2 = data.frame(row.names="h2")

 # 2013
h2$yld13 = heritability(phe13$YDHA,phe13$line, phe13$rep, phe13$rep:phe13$blk)
h2$hsw13 = heritability(phe13$HSW, phe13$line, phe13$rep, phe13$rep:phe13$blk)
h2$dfl13 = heritability(phe13$DF,  phe13$line, phe13$rep, phe13$rep:phe13$blk)
h2$dpm13 = heritability(phe13$DPM, phe13$line, phe13$rep, phe13$rep:phe13$blk)
 # 2014
h2$yld14 = heritability(phe14$YDHA,   phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$dfl14 = heritability(phe14$DF,     phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$dpm14 = heritability(phe14$DPM,    phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$tsw14 = heritability(phe14$TSW,    phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$hsw14 = heritability(phe14$HSW,    phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$pln14 = heritability(phe14$PLN,    phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$ylp14 = heritability(phe14$YDHAPL, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
 # different yields
h2$yld14_b =    heritability( phe14$YD_basic, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$yld14_50g =  heritability(   phe14$YD_50g, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$yld14_100g = heritability(  phe14$YD_100g, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$yld14_1mh =  heritability(   phe14$YD_1mh, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$yld14_2mh =  heritability(   phe14$YD_2mh, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$yld14_10pl = heritability(  phe14$YD_10pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$yld14_20pl = heritability(  phe14$YD_20pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$yld14_50g_1mh_10pl =  heritability( phe14$YD_50g_1mh_10pl,   phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$yld14_100g_2mh_20pl = heritability(phe14$YD_100g_2mh_20pl,   phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
 # definitive yields
h2$yld14_final = heritability(phe14$YDHA_basic,   phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$ylp14_final = heritability(phe14$YDPL_basic,   phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$yld14_100g_1mh_15pl = heritability(phe14$YDHA_100g_1mh_15pl,   phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)
h2$ylp14_100g_1mh_15pl = heritability(phe14$YDPL_100g_1mh_15pl,   phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk)

h2 <- t(h2)
print(h2)

new_yields <- c(h2["yld14",], h2["yld14_b",], h2["yld14_50g",], h2["yld14_100g",], 	h2["yld14_1mh",], h2["yld14_2mh",], h2["yld14_10pl",], h2["yld14_20pl",], 	
	h2["yld14_50g_1mh_10pl",], h2["yld14_100g_2mh_20pl",], h2["yld14_final",], h2["yld14_100g_1mh_15pl",], h2["ylp14_final",], 	h2["ylp14_100g_1mh_15pl",])
yields <-	  c(  "original", 		 "POMc", 	   "> 50gSW", 	  ">100gSW", 			  ">1MH", 			">2MH", 		 ">10PLN", 			">20PLN", 	
	  "50gSW,1MH,10PLN",       "100gSW,2MH,20PLN", 		 "final_YDHA", 			"100g,1MH,15PLN_HA",     "final_YDHAPL", 		 "100gSW,1MH,15PLN_PL")

par(mfcol=c(2,1))
barplot(new_yields[1:7],names=yields[1:7],ylim=c(0,0.5),main="Different Yield Heritabilities")
abline(h=seq(0,0.5,0.05),lty="dotted")
barplot(new_yields[8:14],names=yields[8:14],ylim=c(0,0.5),col=c(rep(8,3),rep(2,4)))
abline(h=seq(0,0.5,0.05),lty="dotted")


#################################
#                            h2 #
# yld13               0.6510847 #
# hsw13               0.8926132 #
# dfl13               0.9422545 #
# dpm13               0.8905599 #
# yld14               0.2478150 #
# dfl14               0.7004765 #
# dpm14               0.7956781 #
# tsw14               0.5586127 #
# hsw14               0.8043470 #
# pln14               0.5701891 #
# ylp14               0.3477040 #
# yld14_b             0.3267215 #
# yld14_50g           0.3092380 #
# yld14_100g          0.2965194 #
# yld14_1mh           0.3544793 #
# yld14_2mh           0.3841102 #
# yld14_10pl          0.3354015 #
# yld14_20pl          0.2962291 #
# yld14_50g_1mh_10pl  0.3178015 #
# yld14_100g_2mh_20pl 0.3960989 #
# yld14_final         0.3270396 #
# ylp14_final         0.3955477 #
# yld14_100g_1mh_15pl 0.3632168 #
# ylp14_100g_1mh_15pl 0.4028210 #
#################################

rm(list= c("new_yields","yields"))
