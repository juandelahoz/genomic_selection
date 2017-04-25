#source("1_clean_raw_data.R")
#source("2_heritabilities_calc.R")
#source("3_BLUP_BLUE_calc.R")
phenotypes <- read.table(phenotypes, "../../phe/line/phenotypes_full.txt", h=T)

blue.ydha_c = get_phenotype(phe14$YDHA_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$rep:phe14$trl, method="blue")
blue.ydpa_c = get_phenotype(phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$rep:phe14$trl, method="blue")

blup.ydha_c = get_phenotype(phe14$YDHA_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$rep:phe14$trl, method="blup")
blup.ydpa_c = get_phenotype(phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$rep:phe14$trl, method="blup")

# blup

plot(resid.yld$model.ha.p,resid.yld$manua.ha.p)

resid.yld$YDHA_100g_1mh_15pl_1200r <- NA ; resid.yld$YDHA_100g_1mh_15pl_1200r[which(abs(resid.yld$manua.ha.p)<=1200)] <- resid.yld$ydha.obs[which(abs(resid.yld$manua.ha.p)<=1200)]


blup.ydha_c.2 = get_phenotype(resid.yld$YDHA_100g_1mh_15pl_1200r, resid.yld$line, resid.yld$rep, resid.yld$blk, resid.yld$trl, method="blup")


# blup

# expectation
plot(resid.yld.2$model.ha.p,resid.yld.2$manua.ha.p)
hist(resid.yld.2$model.ha.p)
hist(resid.yld.2$manua.ha.p)


################################
blup.ydpa_c = BLUPc14(phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$rep:phe14$trl)

