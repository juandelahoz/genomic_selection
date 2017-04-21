 # packages
library(lme4)
library(lsmeans)
########################################################
 # functions to calculate BLUEs and BLUPs

 # BLUE (genotipe as fixed effect)
BLUEc13 <- function( trait, line, rep, blk)
{
	phen_mod <- lmer( trait ~ 0 + line + rep + (1|blk))
	fixed_ef <- as.data.frame( summary( 
		lsmeans( phen_mod, ~ line, 'revpairwise') )[,1:2] )
	intercept <- mean( fixed_ef$lsmean, na.rm=TRUE)
	return( list(model=phen_mod, fixed_ef=fixed_ef,intercept=intercept) )
}
BLUEc14 <- function( trait, line, rep, blk, trl )
{
	phen_mod <- lmer( trait ~ 0 + line + rep + (1|blk) + (1|trl) )
	fixed_ef <- as.data.frame( summary( 
		lsmeans( phen_mod, ~ line, 'revpairwise') )[,1:2] )
	intercept <- mean( fixed_ef$lsmean, na.rm=TRUE)
	return( list(model=phen_mod, fixed_ef=fixed_ef,intercept=intercept) )
}

 # BLUP (genotipe as random effect)
BLUPc13 <- function( trait, line, rep, blk )
{
	phen_mod <- lmer( trait ~ (1|line) + rep + (1|blk) )
	random_ef <- ranef( phen_mod )$line
	return(list(model=phen_mod, random_ef_l=random_ef))
}
BLUPc14 <- function( trait, line, rep, blk, trl )
{
	phen_mod <- lmer( trait ~ (1|line) + rep + (1|blk) + (1|trl) )
	random_ef <- ranef( phen_mod )$line
	return(list(model=phen_mod, random_ef_l=random_ef))
}

########################################################
 # 2013

samples = as.character(sort(unique(phe13$line)))
n.sampl = length(samples)
blup.tb = data.frame(BLUP=rep(NA,n.sampl),BLUE=rep(NA,n.sampl),row.names=samples)

phenotypes.13  <- list( DF=blup.tb, DPM=blup.tb, YDHA=blup.tb, HSW=blup.tb )

	phenotypes.13$YDHA$BLUE = BLUEc13(phe13$YDHA, phe13$line, phe13$rep, phe13$rep:phe13$blk)$fixed_ef$lsmean
	phenotypes.13$YDHA$BLUP = BLUPc13(phe13$YDHA, phe13$line, phe13$rep, phe13$rep:phe13$blk)$random_ef_l[,1] + mean( phenotypes.13$YDHA$BLUE, na.rm=TRUE)

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

	phenotypes.14$DF$BLUE = BLUEc14(phe14$DF, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$fixed_ef$lsmean
	phenotypes.14$DF$BLUP = BLUPc14(phe14$DF, phe14$line, phe14$rep, phe14$rep:phe14$trl:phe14$blk, phe14$trl)$random_ef_l[,1] + mean( phenotypes.14$DF$BLUE, na.rm=TRUE)

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
