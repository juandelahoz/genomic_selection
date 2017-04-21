source("1_clean_raw_data.R")
source("2_heritabilities_calc.R")
source("3_BLUP_BLUE_calc.R")

blue.ydha_c = BLUEc14(phe14$YDHA_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$rep:phe14$trl)
blue.ydpa_c = BLUEc14(phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$rep:phe14$trl)

blup.ydha_c = BLUPc14(phe14$YDHA_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$rep:phe14$trl)
blup.ydpa_c = BLUPc14(phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$rep:phe14$trl)


#####################################################################################
#	
#	BLUEc13 <- function( trait, line, rep, blk)
#	{
#		phen_mod <- lmer( trait ~ 0 + line + rep + (1|blk))
#		fixed_ef <- as.data.frame( summary( 
#			lsmeans( phen_mod, ~ line, 'revpairwise') )[,1:2] )
#		intercept <- mean( fixed_ef$lsmean, na.rm=TRUE)
#		return( list(model=phen_mod, fixed_ef=fixed_ef,intercept=intercept) )
#	}
#	BLUEc14 <- function( trait, line, rep, blk, trl )
#	{
#		phen_mod <- lmer( trait ~ 0 + line + rep + (1|blk) + (1|trl) )
#		fixed_ef <- as.data.frame( summary( 
#			lsmeans( phen_mod, ~ line, 'revpairwise') )[,1:2] )
#		intercept <- mean( fixed_ef$lsmean, na.rm=TRUE)
#		return( list(model=phen_mod, fixed_ef=fixed_ef,intercept=intercept) )
#	}
#	
#	 # BLUP (genotipe as random effect)
#	BLUPc13 <- function( trait, line, rep, blk )
#	{
#		phen_mod <- lmer( trait ~ (1|line) + rep + (1|blk) )
#		random_ef <- ranef( phen_mod )$line
#		return(list(model=phen_mod, random_ef_l=random_ef))
#	}
#	BLUPc14 <- function( trait, line, rep, blk, trl )
#	{
#		phen_mod <- lmer( trait ~ (1|line) + rep + (1|blk) + (1|trl) )
#		random_ef <- ranef( phen_mod )$line
#		return(list(model=phen_mod, random_ef_l=random_ef))
#	}
#####################################################################################


resid.yld <- data.frame(id = paste(phe14$line,phe14$rep,phe14$trl,phe14$blk,sep="-"), line=phe14$line, rep=phe14$rep, trl=phe14$rep:phe14$trl, blk=phe14$rep:phe14$trl:phe14$blk, ydha.obs = phe14$YDHA_100g_1mh_15pl)
resid.yld$model.ha.p <- NA   ; resid.yld$model.ha.p[as.numeric(names(summary(blup.ydha_c$model)$residuals))] <- summary(blup.ydha_c$model)$residuals

# blup
intercept    <- as.numeric(fixef( blup.ydha_c$model )["(Intercept)"])

rep.ef       <- data.frame(rep=levels(phe14$rep),ef=fixef( blup.ydha_c$model ),row.names =NULL)
rep.ef$ef[1] <- 0

trl.ef       <- data.frame(trl= row.names(ranef(blup.ydha_c$model)$trl ),ef=ranef(blup.ydha_c$model)$trl [,1])

blk.ef       <- data.frame(blk= row.names(ranef(blup.ydha_c$model)$blk ),ef=ranef(blup.ydha_c$model)$blk [,1])

line.ef      <- data.frame(line=row.names(ranef(blup.ydha_c$model)$line),ef=ranef(blup.ydha_c$model)$line[,1])

# expectation
resid.yld$expec.ha.p.obs <- intercept + 
							rep.ef [match(resid.yld$rep ,rep.ef [,1]),2] +
							trl.ef [match(resid.yld$trl ,trl.ef [,1]),2] +
							blk.ef [match(resid.yld$blk ,blk.ef [,1]),2] +
							line.ef[match(resid.yld$line,line.ef[,1]),2] 

resid.yld$manua.ha.p <- resid.yld$ydha.obs - resid.yld$expec.ha.p.obs

str(resid.yld)

plot(resid.yld$model.ha.p,resid.yld$manua.ha.p)

resid.yld$YDHA_100g_1mh_15pl_1200r <- NA ; resid.yld$YDHA_100g_1mh_15pl_1200r[which(abs(resid.yld$manua.ha.p)<=1200)] <- resid.yld$ydha.obs[which(abs(resid.yld$manua.ha.p)<=1200)]


blup.ydha_c.2 = BLUPc14(resid.yld$YDHA_100g_1mh_15pl_1200r, resid.yld$line, resid.yld$rep, resid.yld$blk, resid.yld$trl)

resid.yld.2 <- data.frame(id = paste(phe14$line,phe14$rep,phe14$trl,phe14$blk,sep="-"), line=phe14$line, rep=phe14$rep, trl=phe14$rep:phe14$trl, blk=phe14$rep:phe14$trl:phe14$blk, ydha.obs = phe14$YDHA_100g_1mh_15pl)
resid.yld.2$model.ha.p <- NA   ; resid.yld.2$model.ha.p[as.numeric(names(summary(blup.ydha_c.2$model)$residuals))] <- summary(blup.ydha_c.2$model)$residuals

# blup
intercept.2    <- as.numeric(fixef( blup.ydha_c.2$model )["(Intercept)"])

rep.ef.2       <- data.frame(rep= levels                          phe14$rep),ef=fixef( blup.ydha_c.2$model ), row.names = NULL)
rep.ef.2$ef[1] <- 0

trl.ef.2       <- data.frame(trl= row.names(ranef(blup.ydha_c.2$model)$trl ),ef=ranef(blup.ydha_c.2$model)$trl [,1])

blk.ef.2       <- data.frame(blk= row.names(ranef(blup.ydha_c.2$model)$blk ),ef=ranef(blup.ydha_c.2$model)$blk [,1])

line.ef.2      <- data.frame(line=row.names(ranef(blup.ydha_c.2$model)$line),ef=ranef(blup.ydha_c.2$model)$line[,1])

# expectation
resid.yld.2$expec.ha.p.obs <- intercept.2 + 
							rep.ef.2 [match(resid.yld.2$rep ,rep.ef.2 [,1]),2] +
							trl.ef.2 [match(resid.yld.2$trl ,trl.ef.2 [,1]),2] +
							blk.ef.2 [match(resid.yld.2$blk ,blk.ef.2 [,1]),2] +
							line.ef.2[match(resid.yld.2$line,line.ef.2[,1]),2] 

resid.yld.2$manua.ha.p <- resid.yld.2$ydha.obs - resid.yld.2$expec.ha.p.obs

str(resid.yld.2)

plot(resid.yld.2$model.ha.p,resid.yld.2$manua.ha.p)
hist(resid.yld.2$model.ha.p)
hist(resid.yld.2$manua.ha.p)


################################
blup.ydpa_c = BLUPc14(phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$rep:phe14$trl)












## tarea: sacar mejore blups, simplificando la función BLUP... 
## round( ranef( blup.ydha_c$model )$line + mean(coef(blup.ydha_c$model)$line$"(Intercept)") ,3) == round( coef(blup.ydha_c$model)$line["(Intercept)"] ,3)
##        ranef( blup.ydha_c$model )$line                                                        ==             blup.ydha_c$random_ef_l
##        fixef( blup.ydha_c$model )["(Intercept)"]                                              ==  mean( coef(blup.ydha_c$model)$line$"(Intercept)")



