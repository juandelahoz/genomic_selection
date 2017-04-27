#source("1_clean_raw_data.R")
#source("2_heritabilities_calc.R")
#source("3_BLUP_BLUE_calc.R")
phenotypes <- read.table("../../phe/line/phenotypes_full.txt", h=T)
smpl_all   <- phenotypes$line

# remove large residuals
filter_extreme <- function(q_value, name, trait, lne, rep, blk, env= rep(NA,length(trait)), method= "blup")
{
	observed  = get_phenotype(trait, lne, rep, blk, env, method)$oe
	posi_vals = abs( observed$res.o )
	threshold = quantile( posi_vals, (100-q_value)/100, na.rm=T )
	passing   = which( posi_vals < threshold )
	observed$filtered = NA
	observed$filtered[passing] = trait[passing]
	add_phenotype(paste(name,"_fe",q_value,"pct",sep=""), observed$filtered, lne, rep, blk, env, method)
	return(data.frame( id=observed$id, obs=trait, res=observed$res.e, pass=observed$filtered))
}


filter_extreme(1,  "YDHAd.14.P", phe14$YDHA_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl, method="blup")
filter_extreme(2,  "YDHAd.14.P", phe14$YDHA_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl, method="blup")
filter_extreme(3,  "YDHAd.14.P", phe14$YDHA_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl, method="blup")
filter_extreme(5,  "YDHAd.14.P", phe14$YDHA_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl, method="blup")
filter_extreme(10, "YDHAe.14.P", phe14$YDHA_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl, method="blup")

filter_extreme(1,  "YDPLd.14.P", phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl, method="blup")
filter_extreme(2,  "YDPLd.14.P", phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl, method="blup")
filter_extreme(3,  "YDPLd.14.P", phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl, method="blup")
filter_extreme(5,  "YDPLd.14.P", phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl, method="blup")
filter_extreme(10, "YDPLe.14.P", phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl, method="blup")

str(phenotypes)
#write.table(phenotypes, "../../phe/line/phenotypes_full.txt", sep="\t", row.names=FALSE)

# plots
last_filters.ha <- cbind(phenotypes$YDHAa.14.P, phenotypes$YDHAb.14.P, phenotypes$YDHAc.14.P, phenotypes$YDHAd.14.P_fe1pct, phenotypes$YDHAe.14.P_fe5pct, phenotypes$YDHAf.14.P_fe10pct)
par(mfcol=c(3,2))
plot(last_filters.ha[,c(1,2)])
plot(last_filters.ha[,c(1,3)])
plot(last_filters.ha[,c(1,4)])
plot(last_filters.ha[,c(3,4)])
plot(last_filters.ha[,c(3,5)])
plot(last_filters.ha[,c(3,6)])


last_filters.pl <- cbind(phenotypes$YDPLa.14.P, phenotypes$YDPLb.14.P, phenotypes$YDPLc.14.P, phenotypes$YDPLd.14.P_fe1pct, phenotypes$YDPLe.14.P_fe5pct, phenotypes$YDPLf.14.P_fe10pct)
par(mfcol=c(3,2))
plot(last_filters.pl[,c(1,2)])
plot(last_filters.pl[,c(1,3)])
plot(last_filters.pl[,c(1,4)])
plot(last_filters.pl[,c(3,4)])
plot(last_filters.pl[,c(3,5)])
plot(last_filters.pl[,c(3,6)])
