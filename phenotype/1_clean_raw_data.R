rm(list=ls())
#############################
 # get raw phenotype data: 
	#		line_old	line	year	rep		blk			trl		treat	Frow	Fcol		lane	sector	chk	
classes = c("factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor",
	# 	Fe		Zn			DF		DPM			POM		POMc		TSW		HSW			PLN		Mhrl		Mhsq	PLN_Msq		seed	YDHA	YDHAPL	KG_dia_Ha	PHI_fisio NoVAIM_fisio SCMR_fisio CTD_fisio
	"numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")
pheno <- read.table("raw_data/phenotype_data_clean_sorted.txt",h=T, colClasses=classes)
head(pheno)
str(pheno)

################
 # clean 2013 #
phe13 <- subset(pheno,year=="2013")
phe13$line 	 <- factor(phe13$line)
phe13$year 	 <- factor(phe13$year)
phe13$rep 	 <- factor(phe13$rep)
phe13$blk 	 <- factor(phe13$blk)

phe13$lane 	 <- factor(phe13$lane)
phe13$sector <- factor(phe13$sector)
phe13$chk 	 <- factor(phe13$chk)
phe13[,c("line_old","trl","treat","Frow","Fcol","Fe","Zn","POM","POMc","TSW","PLN","Mhrl","Mhsq","YDHAPL")] <- NULL
str(phe13)

################
 # clean 2014 #
phe14 <- subset(pheno,year=="2014")
phe14$line <- factor(phe14$line)
phe14$year <- factor(phe14$year)
phe14$rep  <- factor(phe14$rep)
phe14$blk  <- factor(phe14$blk)

phe14$trl  <- factor(phe14$trl)
phe14$Frow <- factor(phe14$Frow)
phe14$Fcol <- factor(phe14$Fcol)
phe14[,c("line_old","treat","lane","sector","chk","KG_dia_Ha","PHI_fisio","NoVAIM_fisio","SCMR_fisio","CTD_fisio")] <- NULL
str(phe14)

######################################
 # improved yield measurements 2014 #
######################################

 # original yield calculation (per hectare)
which(round(phe14$YDHA) != round( (phe14$TSW / phe14$Mhsq) * ((100 - phe14$POM) /86) * 10) )
 # original yield calculation (per plant)
which(round(phe14$YDHAPL) != round( (phe14$TSW / phe14$PLN) * ((100 - phe14$POMc) /86) * 180) )

 # corrected Percentage of Moisture
mean_POM = mean(ifelse( phe14$TSW>=240 & phe14$POM>7 ,phe14$POM,NA),na.rm=TRUE) # 11.92085
phe14$POMc_2 = ifelse(phe14$TSW<240 | phe14$POM<7, mean_POM, phe14$POM)

 # filters								(phe14$TSW / phe14$Mhsq) * ((100 - phe14$POM)    /86) * 10
phe14$YD_basic = 						(phe14$TSW / phe14$Mhsq) * ((100 - phe14$POMc_2) /86) * 10
 # by weight
phe14$YD_50g   = ifelse( phe14$TSW>=50 ,(phe14$TSW / phe14$Mhsq) * ((100 - phe14$POMc_2) /86) * 10, NA)
phe14$YD_100g  = ifelse( phe14$TSW>=100,(phe14$TSW / phe14$Mhsq) * ((100 - phe14$POMc_2) /86) * 10, NA)
 # by area harvested
phe14$YD_1mh   = ifelse( phe14$Mhrl>=1 ,(phe14$TSW / phe14$Mhsq) * ((100 - phe14$POMc_2) /86) * 10, NA)
phe14$YD_2mh   = ifelse( phe14$Mhrl>=2 ,(phe14$TSW / phe14$Mhsq) * ((100 - phe14$POMc_2) /86) * 10, NA)
 # by number of plants
phe14$YD_10pl  = ifelse( phe14$PLN>=10 ,(phe14$TSW / phe14$Mhsq) * ((100 - phe14$POMc_2) /86) * 10, NA)
phe14$YD_20pl  = ifelse( phe14$PLN>=20 ,(phe14$TSW / phe14$Mhsq) * ((100 - phe14$POMc_2) /86) * 10, NA)
 # combined
phe14$YD_50g_1mh_10pl  = ifelse( phe14$TSW>=50  & phe14$Mhrl>=1 & phe14$PLN>=10 ,(phe14$TSW / phe14$Mhsq) * ((100 - phe14$POMc_2) /86) * 10, NA)
phe14$YD_100g_2mh_20pl = ifelse( phe14$TSW>=100 & phe14$Mhrl>=2 & phe14$PLN>=20 ,(phe14$TSW / phe14$Mhsq) * ((100 - phe14$POMc_2) /86) * 10, NA)
 # definitive filters
phe14$YDHA_basic = 																(phe14$TSW / (phe14$Mhrl*0.6)) * ((100 - phe14$POMc_2) /86) *  10
phe14$YDHA_100g_1mh_15pl = ifelse( phe14$TSW>100 & phe14$Mhrl>1 & phe14$PLN>15 ,(phe14$TSW / (phe14$Mhrl*0.6)) * ((100 - phe14$POMc_2) /86) *  10, NA)
phe14$YDPL_basic = 																(phe14$TSW /  phe14$PLN)       * ((100 - phe14$POMc_2) /86) * 180
phe14$YDPL_100g_1mh_15pl = ifelse( phe14$TSW>100 & phe14$Mhrl>1 & phe14$PLN>15 ,(phe14$TSW /  phe14$PLN)       * ((100 - phe14$POMc_2) /86) * 180, NA)

 # retrieve high yielding
YD_high <- phe14[which(phe14$YDHA_basic>3100),]
YD_high <- YD_high[order(YD_high$YDHA_basic),]
#write.table(YD_high, "raw_data/high_yielding_observations.txt", sep="\t", row.names=FALSE)

rm(mean_POM,classes)
