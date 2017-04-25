source("1_clean_raw_data.R")

# test1
par(mfcol=c(2,1))
hist(phe13$DF[order(summ13$means$DF)],10,col=8,xlim=c(28,45),main="Days to Flowering 2013",xlab="Days",freq=FALSE)
hist(phe14$DF,10,col=8,xlim=c(28,45),main="Days to Flowering 2014",xlab="Days",freq=FALSE)


	par(mfcol=c(2,1))
	plot(-1,-1,xlim=c(0,line.n),ylim=c(27,45),main="Samples Sorted by Mean",xlab="Samples",ylab=t)
	j=1
	for (i in lines[order(summ13$means[,t])]){
		boxplot(phe13[phe13$line==i,t],add=TRUE,at=j, names=i, xaxt="n", yaxt="n")
		j=j+1
	}
	plot(-1,-1,xlim=c(0,line.n),ylim=c(27,45),main="Samples Sorted by Standar Deviation",xlab="Samples",ylab=t)
	j=1
	for (i in lines[order(summ13$stdev[,t])]){
		boxplot(phe13[phe13$line==i,t],add=TRUE,at=j, names=i, xaxt="n", yaxt="n")
		j=j+1
	}

hist(summ13$means$DF)

############################################################################################################
 # SORTING

# sort all pheno
rm(list=ls())
	#		line_old	line	year	rep		blk			trl		treat	Frow	Fcol		lane	sector	chk	
classes = c("factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor",
	# 	Fe		Zn			DF		DPM			POM		POMc		TSW		HSW			PLN		Mhrl		Mhsq	PLN_Msq		seed	YDHA	YDHAPL	KG_dia_Ha	PHI_fisio NoVAIM_fisio SCMR_fisio CTD_fisio
	"numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")
pheno1 <- read.table("../../phe/raw/phenotype_data_clean.txt",h=T, colClasses=classes)
pheno1 <- pheno1[order(pheno1$line),]   # both
pheno1 <- pheno1[order(pheno1$sector),] # 2013
pheno1 <- pheno1[order(pheno1$lane),]   # 2013
pheno1 <- pheno1[order(pheno1$blk),]    # both
pheno1 <- pheno1[order(pheno1$trl),]    # 2014
pheno1 <- pheno1[order(pheno1$rep),]    # both
pheno1 <- pheno1[order(pheno1$year),]   # both
write.table(pheno1, "../../phe/raw/phenotype_data_clean_sorted.txt", sep="\t", row.names=FALSE)
	#		line_old	line	year	rep		blk			trl		treat	Frow	Fcol		lane	sector	chk	
classes = c("factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor",
	# 	Fe		Zn			DF		DPM			POM		POMc		TSW		HSW			PLN		Mhrl		Mhsq	PLN_Msq		seed	YDHA	YDHAPL	KG_dia_Ha	PHI_fisio NoVAIM_fisio SCMR_fisio CTD_fisio
	"numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")
pheno2 <- read.table("../../phe/raw/phenotype_data_clean_sorted.txt",h=T, colClasses=classes)
head(pheno2)
str(pheno2)

 # sort 2013
phe13s <- phe13c[order(phe13c$line),]
phe13s <- phe13s[order(phe13s$lane),]
phe13s <- phe13s[order(phe13s$sector),]
phe13s <- phe13s[order(phe13s$blk),]
phe13s <- phe13s[order(phe13s$rep),]
write.table(phe13s, "../../phe/raw/phenotype_data_clean_sorted.13.txt", sep="\t", row.names=FALSE)
	#		    ID 		line	year	rep		blk			lane	sector
classes = c("factor","factor","factor","factor","factor","factor","factor",
	# 	DF		DPM		 	YDHA	areaM2	wgt_row_gr	humidPCT 	chk
	"numeric","numeric","numeric","numeric","numeric","numeric","factor")
phe13s2 <- read.table("../../phe/raw/phenotype_data_clean_sorted.13.txt",h=T, colClasses=classes)
head(phe13s2)
str(phe13s2)

############################################################################################################
 # POM from different trials
# para entender por qué hay unos POM muy bajitos cuando se hace el filtro de las 240g de peso, y solo en el trial 11
pom_h <- phe14[which(!is.na(ifelse(phe14$trl==11,ifelse(phe14$TSW>=240,ifelse(phe14$POM>7,phe14$POM,NA),NA),NA))),]
pom_l <- phe14[which(!is.na(ifelse(phe14$trl==11,ifelse(phe14$TSW>=240,ifelse(phe14$POM<7,phe14$POM,NA),NA),NA))),]
plot(rbind(pom_l,pom_h)[,c(7,8,9,10,11,12,13,14,15,16,17,18,20,21)])
# buscar si los grupos separados en un eje por el POM, también se separan en el otro eje por alguna variable -> NO!
