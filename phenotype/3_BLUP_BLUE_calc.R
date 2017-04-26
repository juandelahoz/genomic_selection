 # packages
library(lme4)
library(lsmeans)
########################################################
 # function to calculate BLUEs and BLUPs
get_phenotype <- function( trait, lne, rep, blk, env= rep(NA,length(trait)), method= "blup")
	# env can be any additional random variable (samples assigned randomly to levels)
{
	if (method == "blup") {
		
	# run the model using lines as random effects
		if( length(levels(env)) > 0 ) mod <- lmer( trait ~ rep + (1|blk) + (1|lne) + (1|env) )
		else mod <- lmer( trait ~ rep + (1|blk) + (1|lne) )

	# get the effects of each factor and level
		eff <- list( 
			intercept= fixef(mod)["(Intercept)"], 
			rep= data.frame( lev=    levels(            rep ), ef= fixef(mod)          ), 
			blk= data.frame( lev= row.names( ranef(mod)$blk ), ef= ranef(mod)$blk [,1] ), 
			lne= data.frame( lev= row.names( ranef(mod)$lne ), ef= ranef(mod)$lne [,1] ) 
			)
		eff$rep$ef[1] <- 0   # base effect is 0 (Intercept)

	} else if (method == "blue") {

	# run the model using lines as random effects
		if( length(levels(env)) > 0 ) mod <- lmer( trait ~ 0 + (1|env) + rep + (1|blk) + lne )
		else                          mod <- lmer( trait ~ 0           + rep + (1|blk) + lne )

	# get the effects of each factor and level
		eff <- list( 
			intercept= 0, 
			rep= data.frame( lev= sub("rep","",names(fixef(mod)[grep("rep",names(fixef(mod)))])), 
							  ef=                    fixef(mod)[grep("rep",names(fixef(mod)))] ), 
			blk= data.frame( lev= row.names( ranef(mod)$blk ), ef= ranef(mod)$blk [,1] ), 
			lne= data.frame( lev= c(levels(lne)[1], sub("lne","",names(fixef(mod)[grep("lne",names(fixef(mod)))]))), # (very complicated, but works!) 
							  ef= c(            0 ,                    fixef(mod)[grep("lne",names(fixef(mod)))]) )  # Adds the base fixed effect (0) of the first (alphabetic) line id
			)

	} else {

		stop(method, ' is not a valid method. Please use "blup" or "blue".')
	}

# build a table with observations and expectations
	oe <- data.frame( 
		id=    paste( lne, rep, blk, sep= "-"), 
		lne.o= lne,   rep.o= rep, blk.o= blk, 
		obs=   trait, res.o= NA, 
		lne.e= eff$lne[match( lne , eff$lne$lev ), "ef"], 
		rep.e= eff$rep[match( rep , eff$rep$lev ), "ef"], 
		blk.e= eff$blk[match( blk , eff$blk$lev ), "ef"]
		)
	oe$exp   <- eff$intercept + oe$lne.e + oe$rep.e + oe$blk.e
	oe$res.e <- oe$obs - oe$exp
	oe$res.o [as.numeric(names(summary(mod)$residuals))] <- summary(mod)$residuals

# fill objects if optional random variable was used
	if( length(levels(env)) > 0 ){
		eff$env=  data.frame( lev= row.names( ranef(mod)$env ), ef= ranef(mod)$env [,1] )
		oe$id=    paste( lne, env, rep, blk, sep= "-")
		oe$env.o= env
		oe$env.e= eff$env[match( env , eff$env$lev ), "ef"]
		oe$exp=   eff$intercept + oe$lne.e + oe$rep.e + oe$blk.e + oe$env.e
		oe$res.e= oe$obs - oe$exp
		oe <- oe[,c("id","lne.o","rep.o","blk.o","env.o","obs","res.o","lne.e","rep.e","blk.e","env.e","exp","res.e")]
	}

# extract raw means and standard deviations per line
	raw <- data.frame(
		lne=  levels( lne ),
		mean= tapply( trait, lne, mean, na.rm=TRUE),
		stdv= tapply( trait, lne,   sd, na.rm=TRUE)
		)
	raw <- raw[ which(!is.na( raw$mean )), ]

# get residual variance of the model

	var <- attr(VarCorr(mod), "sc")

# return the answer
	if(method=="blup"){
	# recover the adjusted means per line
		blup <- data.frame( line= eff$lne$lev, blup= eff$lne$ef + eff$intercept )
		return(list( mod= mod , eff= eff , oe= oe , blup= blup , raw= raw , var= var ))
	} 	else if(method=="blue"){
	# recover the adjusted means per line
		blue     <- data.frame( line= eff$lne$lev, blue= eff$lne$ef + mean(eff$rep$ef) )
		return(list( mod= mod , eff= eff , oe= oe , blue= blue , raw= raw , var= var ))
	}
}

########################################################
 # 2013

samples = as.character(sort(unique(phe13$line)))
n.sampl = length(samples)
blup.tb = data.frame(BLUP=rep(NA,n.sampl),BLUE=rep(NA,n.sampl),row.names=samples)

phenotypes.13  <- list( DF=blup.tb, DPM=blup.tb, YDHA=blup.tb, HSW=blup.tb )

	phenotypes.13$YDHA$BLUE = get_phenotype(phe13$YDHA, phe13$line, phe13$rep, phe13$rep:phe13$blk, method="blue")$blue$blue
	phenotypes.13$YDHA$BLUP = get_phenotype(phe13$YDHA, phe13$line, phe13$rep, phe13$rep:phe13$blk, method="blup")$blup$blup

	phenotypes.13$DF$BLUE = BLUEc13(phe13$DF, phe13$line, phe13$rep, phe13$rep:phe13$blk)$fixed_ef$lsmean
	phenotypes.13$DF$BLUP = BLUPc13(phe13$DF, phe13$line, phe13$rep, phe13$rep:phe13$blk)$random_ef_l[,1] + mean( phenotypes.13$DF$BLUE, na.rm=TRUE)

	phenotypes.13$DPM$BLUE = BLUEc13(phe13$DPM, phe13$line, phe13$rep, phe13$rep:phe13$blk)$fixed_ef$lsmean
	phenotypes.13$DPM$BLUP = BLUPc13(phe13$DPM, phe13$line, phe13$rep, phe13$rep:phe13$blk)$random_ef_l[,1] + mean( phenotypes.13$DPM$BLUE, na.rm=TRUE)

	phenotypes.13$HSW$BLUE = BLUEc13(phe13$HSW, phe13$line, phe13$rep, phe13$rep:phe13$blk)$fixed_ef$lsmean
	phenotypes.13$HSW$BLUP = BLUPc13(phe13$HSW, phe13$line, phe13$rep, phe13$rep:phe13$blk)$random_ef_l[,1] + mean( phenotypes.13$HSW$BLUE, na.rm=TRUE)

 # 2014

samples = as.character(sort(unique(phe14$line)))
n.sampl = length(samples)
blup.tb = data.frame(BLUP=rep(NA,n.sampl),BLUE=rep(NA,n.sampl),row.names=samples)

phenotypes.14  <- list( DF=blup.tb, DPM=blup.tb, TSW=blup.tb, HSW=blup.tb, YDHA_a=blup.tb, YDPL_a=blup.tb, YDHA_b=blup.tb, YDPL_b=blup.tb, YDHA_c=blup.tb, YDPL_c=blup.tb)

	phenotypes.14$DF$BLUE = get_phenotype(phe14$DF, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl,"blue")$blue$blue
	phenotypes.14$DF$BLUP = get_phenotype(phe14$DF, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl,"blup")$blup$blup

	phenotypes.14$DPM$BLUE = BLUEc14(phe14$DPM, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef$lsmean
	phenotypes.14$DPM$BLUP = BLUPc14(phe14$DPM, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$random_ef_l[,1] + mean( phenotypes.14$DPM$BLUE, na.rm=TRUE)

	phenotypes.14$TSW$BLUE = BLUEc14(phe14$TSW, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef$lsmean
	phenotypes.14$TSW$BLUP = BLUPc14(phe14$TSW, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$random_ef_l[,1] + mean( phenotypes.14$TSW$BLUE, na.rm=TRUE)

	phenotypes.14$HSW$BLUE = BLUEc14(phe14$HSW, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef$lsmean
	phenotypes.14$HSW$BLUP = BLUPc14(phe14$HSW, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$random_ef_l[,1] + mean( phenotypes.14$HSW$BLUE, na.rm=TRUE)

	phenotypes.14$YDHA_a$BLUE = BLUEc14(phe14$YDHA, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef$lsmean
	phenotypes.14$YDHA_a$BLUP = BLUPc14(phe14$YDHA, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$random_ef_l[,1] + mean( phenotypes.14$YDHA_a$BLUE, na.rm=TRUE)

	phenotypes.14$YDPL_a$BLUE = BLUEc14(phe14$YDHAPL, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef$lsmean
	phenotypes.14$YDPL_a$BLUP = BLUPc14(phe14$YDHAPL, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$random_ef_l[,1] + mean( phenotypes.14$YDPL_a$BLUE, na.rm=TRUE)

	phenotypes.14$YDHA_b$BLUE = BLUEc14(phe14$YDHA_basic, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef$lsmean
	phenotypes.14$YDHA_b$BLUP = BLUPc14(phe14$YDHA_basic, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$random_ef_l[,1] + mean( phenotypes.14$YDHA_b$BLUE, na.rm=TRUE)

	phenotypes.14$YDPL_b$BLUE = BLUEc14(phe14$YDPL_basic, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef$lsmean
	phenotypes.14$YDPL_b$BLUP = BLUPc14(phe14$YDPL_basic, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$random_ef_l[,1] + mean( phenotypes.14$YDPL_b$BLUE, na.rm=TRUE)

						behc = BLUEc14(phe14$YDHA_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef
	phenotypes.14$YDHA_c$BLUE[match(behc$line,rownames(phenotypes.14$YDHA_c))] = behc$lsmean
	phenotypes.14$YDHA_c$BLUP[match(behc$line,rownames(phenotypes.14$YDHA_c))] = BLUPc14(phe14$YDHA_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$random_ef_l[,1] + mean( phenotypes.14$YDHA_c$BLUE, na.rm=TRUE)

						bepc = BLUEc14(phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef
	phenotypes.14$YDPL_c$BLUE[match(bepc$line,rownames(phenotypes.14$YDPL_c))] = bepc$lsmean
	phenotypes.14$YDPL_c$BLUP[match(bepc$line,rownames(phenotypes.14$YDPL_c))] = BLUPc14(phe14$YDPL_100g_1mh_15pl, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$random_ef_l[,1] + mean( phenotypes.14$YDPL_c$BLUE, na.rm=TRUE)

str(phenotypes.13)
str(phenotypes.14)

########################################################

 # get old phenotypes
old = read.table("../../phe/raw/phenotype_data_old.txt",h=T)
smpl_shared = sort( as.factor( intersect( intersect( row.names(phenotypes.13$DF), row.names(phenotypes.14$DF ) ), old$id.old ) ) )
length(smpl_shared)
# 426
smpl_all    = sort( as.factor( union(     union(     row.names(phenotypes.13$DF), row.names(phenotypes.14$DF ) ), old$id.old ) ) )
length(smpl_all)
# 830

phenotypes = data.frame(line= smpl_all)

phenotypes$DF.13.P  	= NA  ;  	phenotypes$DF.13.P  [match( row.names(phenotypes.13$DF), smpl_all )]   <- round(phenotypes.13$DF$BLUP,2)
phenotypes$DF.13.E  	= NA  ;  	phenotypes$DF.13.E  [match( row.names(phenotypes.13$DF), smpl_all )]   <- round(phenotypes.13$DF$BLUE,2)
phenotypes$DF.14.P  	= NA  ;  	phenotypes$DF.14.P  [match( row.names(phenotypes.14$DF), smpl_all )]   <- round(phenotypes.14$DF$BLUP,2)
phenotypes$DF.14.E  	= NA  ;  	phenotypes$DF.14.E  [match( row.names(phenotypes.14$DF), smpl_all )]   <- round(phenotypes.14$DF$BLUE,2)

phenotypes$DPM.13.P  	= NA  ;  	phenotypes$DPM.13.P [match( row.names(phenotypes.13$DPM), smpl_all )]  <- round(phenotypes.13$DPM$BLUP,2)
phenotypes$DPM.13.E  	= NA  ;  	phenotypes$DPM.13.E [match( row.names(phenotypes.13$DPM), smpl_all )]  <- round(phenotypes.13$DPM$BLUE,2)
phenotypes$DPM.14.P  	= NA  ;  	phenotypes$DPM.14.P [match( row.names(phenotypes.14$DPM), smpl_all )]  <- round(phenotypes.14$DPM$BLUP,2)
phenotypes$DPM.14.E  	= NA  ;  	phenotypes$DPM.14.E [match( row.names(phenotypes.14$DPM), smpl_all )]  <- round(phenotypes.14$DPM$BLUE,2)

phenotypes$HSW.13.P  	= NA  ;  	phenotypes$HSW.13.P [match( row.names(phenotypes.13$HSW), smpl_all )]  <- round(phenotypes.13$HSW$BLUP,2)
phenotypes$HSW.13.E  	= NA  ;  	phenotypes$HSW.13.E [match( row.names(phenotypes.13$HSW), smpl_all )]  <- round(phenotypes.13$HSW$BLUE,2)
phenotypes$HSW.14.P  	= NA  ;  	phenotypes$HSW.14.P [match( row.names(phenotypes.14$HSW), smpl_all )]  <- round(phenotypes.14$HSW$BLUP,2)
phenotypes$HSW.14.E  	= NA  ;  	phenotypes$HSW.14.E [match( row.names(phenotypes.14$HSW), smpl_all )]  <- round(phenotypes.14$HSW$BLUE,2)

phenotypes$YDHA.13.P  	= NA  ;  	phenotypes$YDHA.13.P[match( row.names(phenotypes.13$YDHA), smpl_all )] <- round(phenotypes.13$YDHA$BLUP,2)
phenotypes$YDHA.13.E  	= NA  ;  	phenotypes$YDHA.13.E[match( row.names(phenotypes.13$YDHA), smpl_all )] <- round(phenotypes.13$YDHA$BLUE,2)
phenotypes$YDHAa.14.P  	= NA  ;  	phenotypes$YDHAa.14.P[match( row.names(phenotypes.14$YDHA_a), smpl_all )] <- round(phenotypes.14$YDHA_a$BLUP,2)
phenotypes$YDHAa.14.E  	= NA  ;  	phenotypes$YDHAa.14.E[match( row.names(phenotypes.14$YDHA_a), smpl_all )] <- round(phenotypes.14$YDHA_a$BLUE,2)
phenotypes$YDPLa.14.P  	= NA  ;  	phenotypes$YDPLa.14.P[match( row.names(phenotypes.14$YDPL_a), smpl_all )] <- round(phenotypes.14$YDPL_a$BLUP,2)
phenotypes$YDPLa.14.E  	= NA  ;  	phenotypes$YDPLa.14.E[match( row.names(phenotypes.14$YDPL_a), smpl_all )] <- round(phenotypes.14$YDPL_a$BLUE,2)

phenotypes$YDHAb.14.P  	= NA  ;  	phenotypes$YDHAb.14.P[match( row.names(phenotypes.14$YDHA_b), smpl_all )] <- round(phenotypes.14$YDHA_b$BLUP,2)
phenotypes$YDHAb.14.E  	= NA  ;  	phenotypes$YDHAb.14.E[match( row.names(phenotypes.14$YDHA_b), smpl_all )] <- round(phenotypes.14$YDHA_b$BLUE,2)
phenotypes$YDPLb.14.P  	= NA  ;  	phenotypes$YDPLb.14.P[match( row.names(phenotypes.14$YDPL_b), smpl_all )] <- round(phenotypes.14$YDPL_b$BLUP,2)
phenotypes$YDPLb.14.E  	= NA  ;  	phenotypes$YDPLb.14.E[match( row.names(phenotypes.14$YDPL_b), smpl_all )] <- round(phenotypes.14$YDPL_b$BLUE,2)

phenotypes$YDHAc.14.P  	= NA  ;  	phenotypes$YDHAc.14.P[match( row.names(phenotypes.14$YDHA_c), smpl_all )] <- round(phenotypes.14$YDHA_c$BLUP,2)
phenotypes$YDHAc.14.E  	= NA  ;  	phenotypes$YDHAc.14.E[match( row.names(phenotypes.14$YDHA_c), smpl_all )] <- round(phenotypes.14$YDHA_c$BLUE,2)
phenotypes$YDPLc.14.P  	= NA  ;  	phenotypes$YDPLc.14.P[match( row.names(phenotypes.14$YDPL_c), smpl_all )] <- round(phenotypes.14$YDPL_c$BLUP,2)
phenotypes$YDPLc.14.E  	= NA  ;  	phenotypes$YDPLc.14.E[match( row.names(phenotypes.14$YDPL_c), smpl_all )] <- round(phenotypes.14$YDPL_c$BLUE,2)

phenotypes$DF.13.old  	= NA  ;  	phenotypes$DF.13.old[match( old$id.old, smpl_all )] 	<-   old$DF.13
phenotypes$DF.14.old  	= NA  ;  	phenotypes$DF.14.old[match( old$id.old, smpl_all )] 	<-   old$DF.14
phenotypes$DPM.13.old  	= NA  ;  	phenotypes$DPM.13.old[match( old$id.old, smpl_all )] 	<-   old$DPM.13
phenotypes$DPM.14.old  	= NA  ;  	phenotypes$DPM.14.old[match( old$id.old, smpl_all )] 	<-   old$DPM.14
phenotypes$HSW.13.old  	= NA  ;  	phenotypes$HSW.13.old[match( old$id.old, smpl_all )] 	<-   old$HSW.13
phenotypes$HSW.14.old  	= NA  ;  	phenotypes$HSW.14.old[match( old$id.old, smpl_all )] 	<-   old$HSW.14
phenotypes$YDHA.13.old  = NA  ;  	phenotypes$YDHA.13.old[match( old$id.old, smpl_all )] 	<-   old$YDHA.13
phenotypes$YDHA.14.old  = NA  ;  	phenotypes$YDHA.14.old[match( old$id.old, smpl_all )] 	<-   old$YDHA.14

minerals <- subset(phe14,!is.na(phe14$Fe))
phenotypes$Fe.14.s  	= NA  ;  	phenotypes$Fe.14.s[match(minerals$line, smpl_all)] = minerals$Fe
phenotypes$Zn.14.s  	= NA  ;  	phenotypes$Zn.14.s[match(minerals$line, smpl_all)] = minerals$Zn

str(phenotypes)

 # save it
#write.table(phenotypes, "../../phe/line/phenotypes_full.txt", sep="\t", row.names=FALSE)
#phenotypes <- read.table(phenotypes, "../../phe/line/phenotypes_full.txt", h=T)

 # correlate BLUE and BLUP, and correlate between years
par(mfcol=c(1,2))
plot(phenotypes$YDHAc.14.P, phenotypes$YDHAc.14.E)
plot(phenotypes$YDHA.13.E, phenotypes$YDHAc.14.E)

 # compare distributions of BLUE and BLUP, and both years
par(mfcol=c(2,2))
hist(phenotypes$YDHA.13.P,30,col=8,xlim=c(150,1500),main="BLUP 13 YDHA")
hist(phenotypes$YDHA.13.E,30,col=8,xlim=c(150,1500),main="BLUE 13 YDHA")
hist(phenotypes$YDHAc.14.P,30,col=8,xlim=c(250,3000),main="BLUP 14 YDHA")
hist(phenotypes$YDHAc.14.E,30,col=8,xlim=c(250,3000),main="BLUE 14 YDHA")

 # calculate all corelations between every pair of variables
correlations <- round(cor(phenotypes[sapply(phenotypes,class)=="numeric"],use="complete.obs",method="pearson"),4)
 # retrieve the p-value of the correlations between every pair of variables
correlations.p <- matrix(nrow=nrow(correlations),ncol=ncol(correlations))
for( n in 1:nrow(correlations)){
	for( p in 1:nrow(correlations)){
		correlations.p[n,p] <- round(cor.test(phenotypes[sapply(phenotypes,class)=="numeric"][,n],phenotypes[sapply(phenotypes,class)=="numeric"][,p],method="pearson")$p.value,4)
	}
}
#write.table(correlations,  "../../phe/line/variables_correlations.txt",       sep="\t")
#write.table(correlations.p,"../../phe/line/variables_correlations_pvalue.txt",sep="\t")
par(mfcol=c(1,1))
heatmap(correlations)

rm(list=c("minerals","n","p","phenotypes.13","phenotypes.14","samples","n.sampl","blup.tb","bepc","behc"))
