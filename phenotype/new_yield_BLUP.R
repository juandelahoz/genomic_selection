# correlations between years for new yields
samples = as.character(sort(unique(phe14$line)))
n.sampl = length(samples)
blup.tb = data.frame(BLUP=rep(NA,n.sampl),BLUE=rep(NA,n.sampl),row.names=samples)
yields14 <- list( yld14=blup.tb, yld14_b=blup.tb, yld14_50g=blup.tb, yld14_100g=blup.tb, 
	yld14_1mh=blup.tb, yld14_2mh=blup.tb, yld14_10pl=blup.tb, yld14_20pl=blup.tb, 
	yld14_50g_1mh_10pl=blup.tb, yld14_100g_2mh_20pl=blup.tb)

	yields14$yld14$BLUE = BLUEc14(phe14$YDHA, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef
	yld14.BLUP = BLUPc14(phe14$YDHA, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)
	yields14$yld14$BLUP = yld14.BLUP$random_ef_l + mean( yields14$yld14$BLUE$lsmean, na.rm=TRUE)

	yields14$yld14_b$BLUE = BLUEc14(phe14$YD_basic, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef
	yld14_b.BLUP = BLUPc14(phe14$YD_basic, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)
	yields14$yld14_b$BLUP = yld14_b.BLUP$random_ef_l + mean( yields14$yld14_b$BLUE$lsmean, na.rm=TRUE)

	yields14$yld14_50g$BLUE = BLUEc14(phe14$YD_50g, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef
	yld14_50g.BLUP = BLUPc14(phe14$YD_50g, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)
	yields14$yld14_50g$BLUP = yld14_50g.BLUP$random_ef_l + mean( yields14$yld14_50g$BLUE$lsmean, na.rm=TRUE)

	yields14$yld14_100g$BLUE = BLUEc14(phe14$YD_100g, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef
	yld14_100g.BLUP = BLUPc14(phe14$YD_100g, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)
	yields14$yld14_100g$BLUP = yld14_100g.BLUP$random_ef_l + mean( yields14$yld14_100g$BLUE$lsmean, na.rm=TRUE)

	yields14$yld14_1mh$BLUE = BLUEc14(phe14$YD_1mh, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef
	yld14_1mh.BLUP = BLUPc14(phe14$YD_1mh, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)
	yields14$yld14_1mh$BLUP = yld14_1mh.BLUP$random_ef_l + mean( yields14$yld14_1mh$BLUE$lsmean, na.rm=TRUE)

	yields14$yld14_2mh$BLUE = BLUEc14(phe14$YD_2mh, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef
	yld14_2mh.BLUP = BLUPc14(phe14$YD_2mh, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)
	yields14$yld14_2mh$BLUP = yld14_2mh.BLUP$random_ef_l + mean( yields14$yld14_2mh$BLUE$lsmean, na.rm=TRUE)

	yields14$yld14_10pl$BLUE = BLUEc14(phe14$YD_10pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef
	yld14_10pl.BLUP = BLUPc14(phe14$YD_10pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)
	yields14$yld14_10pl$BLUP = yld14_10pl.BLUP$random_ef_l + mean( yields14$yld14_10pl$BLUE$lsmean, na.rm=TRUE)

	yields14$yld14_20pl$BLUE = BLUEc14(phe14$YD_20pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef
	yld14_20pl.BLUP = BLUPc14(phe14$YD_20pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)
	yields14$yld14_20pl$BLUP = yld14_20pl.BLUP$random_ef_l + mean( yields14$yld14_20pl$BLUE$lsmean, na.rm=TRUE)

	yields14$yld14_50g_1mh_10pl$BLUE = BLUEc14(phe14$YD_50g_1mh_10pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef
	yld14_50g_1mh_10pl.BLUP = BLUPc14(phe14$YD_50g_1mh_10pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)
	yields14$yld14_50g_1mh_10pl$BLUP = yld14_50g_1mh_10pl.BLUP$random_ef_l + mean( yields14$yld14_50g_1mh_10pl$BLUE$lsmean, na.rm=TRUE)

	yields14$yld14_100g_2mh_20pl$BLUE = BLUEc14(phe14$YD_100g_2mh_20pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef
	yld14_100g_2mh_20pl.BLUP = BLUPc14(phe14$YD_100g_2mh_20pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)
	yields14$yld14_100g_2mh_20pl$BLUP = yld14_100g_2mh_20pl.BLUP$random_ef_l + mean( yields14$yld14_100g_2mh_20pl$BLUE$lsmean, na.rm=TRUE)

yld14_50g_1mh_15pl.BLUP = BLUPc14(phe14$YDHA_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)
ylp14_50g_1mh_15pl.BLUP = BLUPc14(phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)
yld14_clean.BLUP = BLUPc14(phe14$YDHA_basic, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)
ylp14_clean.BLUP = BLUPc14(phe14$YDPL_basic, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)

######################################################################
yld.blp <- data.frame(lines.n = rep(NA,14), residual.var = rep(NA,14),
	row.names = c("yld14", "yld14_b", "yld14_50g", "yld14_100g", "yld14_1mh", 
		"yld14_2mh", "yld14_10pl", "yld14_20pl", "yld14_50g_1mh_10pl", "yld14_100g_2mh_20pl",
		"yld14_50g_1mh_15pl", "ylp14_50g_1mh_15pl","yld14_clean","ylp14_clean"))

yld.blp$lines.n[1] = nrow(yld14.BLUP$random_ef_l)
yld.blp$lines.n[2] = nrow(yld14_b.BLUP$random_ef_l)
yld.blp$lines.n[3] = nrow(yld14_50g.BLUP$random_ef_l)
yld.blp$lines.n[4] = nrow(yld14_100g.BLUP$random_ef_l)
yld.blp$lines.n[5] = nrow(yld14_1mh.BLUP$random_ef_l)
yld.blp$lines.n[6] = nrow(yld14_2mh.BLUP$random_ef_l)
yld.blp$lines.n[7] = nrow(yld14_10pl.BLUP$random_ef_l)
yld.blp$lines.n[8] = nrow(yld14_20pl.BLUP$random_ef_l)
yld.blp$lines.n[9] = nrow(yld14_50g_1mh_10pl.BLUP$random_ef_l)
yld.blp$lines.n[10] = nrow(yld14_100g_2mh_20pl.BLUP$random_ef_l)

yld.blp$lines.n[11] = nrow(yld14_50g_1mh_15pl.BLUP$random_ef_l)
yld.blp$lines.n[12] = nrow(ylp14_50g_1mh_15pl.BLUP$random_ef_l)
yld.blp$lines.n[13] = nrow(yld14_clean.BLUP$random_ef_l)
yld.blp$lines.n[14] = nrow(ylp14_clean.BLUP$random_ef_l)


yld.blp$residual.var[1] = attr(VarCorr(yld14.BLUP$model), "sc")
yld.blp$residual.var[2] = attr(VarCorr(yld14_b.BLUP$model), "sc")
yld.blp$residual.var[3] = attr(VarCorr(yld14_50g.BLUP$model), "sc")
yld.blp$residual.var[4] = attr(VarCorr(yld14_100g.BLUP$model), "sc")
yld.blp$residual.var[5] = attr(VarCorr(yld14_1mh.BLUP$model), "sc")
yld.blp$residual.var[6] = attr(VarCorr(yld14_2mh.BLUP$model), "sc")
yld.blp$residual.var[7] = attr(VarCorr(yld14_10pl.BLUP$model), "sc")
yld.blp$residual.var[8] = attr(VarCorr(yld14_20pl.BLUP$model), "sc")
yld.blp$residual.var[9] = attr(VarCorr(yld14_50g_1mh_10pl.BLUP$model), "sc")
yld.blp$residual.var[10] = attr(VarCorr(yld14_100g_2mh_20pl.BLUP$model), "sc")

yld.blp$residual.var[11] = attr(VarCorr(yld14_50g_1mh_15pl.BLUP$model), "sc")
yld.blp$residual.var[12] = attr(VarCorr(ylp14_50g_1mh_15pl.BLUP$model), "sc")
yld.blp$residual.var[13] = attr(VarCorr(yld14_clean.BLUP$model), "sc")
yld.blp$residual.var[14] = attr(VarCorr(ylp14_clean.BLUP$model), "sc")


par(mfcol=c(1,1))
plot(yld.blp$lines.n, yld.blp$residual.var, pch=19,col=2, ylab="Residual variance (stdev) in model",xlab="Number of lines with yield BLUP")
text(yld.blp$lines.n, yld.blp$residual.var, labels=row.names(yld.blp),cex=0.7,pos=3)
