#setwd("../model")
set.seed(1234)
sampl <- read.table("../../gen/raw/MGC_annt_repM_q40_s_maf05_oh06_I300_impt_rrBLUP_samples.txt",h=F)[,1]
genet <- read.table("../../gen/raw/MGC_annt_repM_q40_s_maf05_oh06_I300_impt_rrBLUP.in", row.names=as.character(sampl), h=F)
pheno <- read.table("../../phe/line/phenotypes_full.txt",h=T,row.names="line")

# obtain the number of lines with genotype and phenotype data per trait
	pheno_num <-numeric(0)
	for(i in 1:ncol(pheno)){
		lines_gp = intersect( sampl, rownames( pheno[which(!is.na(pheno[,i])),] ))
		pheno_num[i] <- length( which( !is.na( pheno[lines_gp, i] )))
		names(pheno_num)[i] <- names(pheno)[i]
	}
#	as.matrix(pheno_num[order(pheno_num)])

# select only samples from 2013 with genotype and phenotype data to use
	use_13 =	intersect(row.names(pheno)[which(!is.na(pheno$DF.13.old))],
				intersect(row.names(pheno)[which(!is.na(pheno$DPM.13.old))],
					intersect(row.names(pheno)[which(!is.na(pheno$HSW.13.old))],
					intersect(row.names(pheno)[which(!is.na(pheno$YDHA.13.old))],
						intersect(row.names(pheno)[which(!is.na(pheno$DF.13.P))],
						intersect(row.names(pheno)[which(!is.na(pheno$DF.13.E))],
							intersect(row.names(pheno)[which(!is.na(pheno$DPM.13.P))],
							intersect(row.names(pheno)[which(!is.na(pheno$DPM.13.E))],
								intersect(row.names(pheno)[which(!is.na(pheno$HSW.13.P))],
								intersect(row.names(pheno)[which(!is.na(pheno$HSW.13.E))],
									intersect(row.names(pheno)[which(!is.na(pheno$YDHA.13.P))],
									intersect(row.names(pheno)[which(!is.na(pheno$YDHA.13.E))],
										sampl))))))))))))
	sampl_n_13 = length(use_13)

# select only samples from 2014 with genotype and phenotype data to use
	use_14 = 	intersect(row.names(pheno)[which(!is.na(pheno$YDPLd.14.P_fe5pct))],
				intersect(row.names(pheno)[which(!is.na(pheno$YDHAd.14.P_fe5pct))],
					intersect(row.names(pheno)[which(!is.na(pheno$YDPLd.14.P_fe3pct))],
					intersect(row.names(pheno)[which(!is.na(pheno$YDHAd.14.P_fe3pct))],
						intersect(row.names(pheno)[which(!is.na(pheno$YDPLd.14.P_fe2pct))],
						intersect(row.names(pheno)[which(!is.na(pheno$YDHAd.14.P_fe2pct))],
							intersect(row.names(pheno)[which(!is.na(pheno$YDPLd.14.P_fe1pct))],
							intersect(row.names(pheno)[which(!is.na(pheno$YDHAd.14.P_fe1pct))],
							sampl))))))))
	sampl_n_14 = length(use_14)

# generate partitions (70%TP vs 30%VP)
	combinat_13 <- matrix(0, nrow=sampl_n_13, ncol=100, dimnames=list(row=use_13))
	combinat_14 <- matrix(0, nrow=sampl_n_14, ncol=100, dimnames=list(row=use_14))
	for( i in 1:100 ){
		# 0 = train ; 1 = test
#		combinat_13[ sample( 1:sampl_n_13, sampl_n_13*0.3 ) , i] = 1 
#		combinat_14[ sample( 1:sampl_n_14, sampl_n_14*0.3 ) , i] = 1 
	}
#	write.table( combinat_13, "../../mod/combinat_13.mtx", sep="\t", col.names=FALSE)
#	write.table( combinat_14, "../../mod/combinat_14.mtx", sep="\t", col.names=FALSE)
	combinat_13 <- as.matrix(read.table("../../mod/combinat_13.mtx", h=F, row.names=1))
	combinat_14 <- as.matrix(read.table("../../mod/combinat_14.mtx", h=F, row.names=1))

# object to fill with correlations
	trait.cors = matrix(numeric(0), nrow=100, ncol=10, dimnames=list(NULL,
		c("BayesA", "BayesB", "BayesC", "BayesRR", "BLasso", "BLassof", "FIXED", "RKHS", "GBLUP", "rrBLUP")))
	trait.cors = as.data.frame(trait.cors)
	cors = list(DF.13.o=trait.cors, DPM.13.o=trait.cors, HSW.13.o=trait.cors, YDHA.13.o=trait.cors,
				DF.13=trait.cors, DPM.13=trait.cors, HSW.13=trait.cors, YDHA.13=trait.cors,
				DF.14.o=trait.cors, DPM.14.o=trait.cors, HSW.14.o=trait.cors, YDHA.14.o=trait.cors,
				DF.14=trait.cors, DPM.14=trait.cors, HSW.14=trait.cors, Fe.14=trait.cors, Zn.14=trait.cors,
				YDHAa.14=trait.cors, YDHAb.14=trait.cors, YDHAc.14=trait.cors, YDHAd.14=trait.cors,
				YDPLa.14=trait.cors, YDPLb.14=trait.cors, YDPLc.14=trait.cors, YDPLd.14=trait.cors,
				YDHAd2p.14=trait.cors, YDHAd3p.14=trait.cors, YDHAd5p.14=trait.cors,
				YDPLd2p.14=trait.cors, YDPLd3p.14=trait.cors, YDPLd5p.14=trait.cors)
#	str(cors)

# get X, y, Z and G matrices. And mean from diagonal.
	y3 = pheno[use_13, c("DF.13.P","DPM.13.P","HSW.13.P","YDHA.13.P",
						 "DF.13.old","DPM.13.old","HSW.13.old","YDHA.13.old")]
	y4 = pheno[use_14, c("DF.14.P","DPM.14.P","HSW.14.P","Fe.14.s","Zn.14.s",
						 "DF.14.old","DPM.14.old","HSW.14.old","YDHA.14.old",
						 "YDHAa.14.P","YDHAb.14.P","YDHAc.14.P","YDHAd.14.P_fe1pct",
						 "YDPLa.14.P","YDPLb.14.P","YDPLc.14.P","YDPLd.14.P_fe1pct",
						 "YDHAd.14.P_fe2pct","YDHAd.14.P_fe3pct","YDHAd.14.P_fe5pct",
						 "YDPLd.14.P_fe2pct","YDPLd.14.P_fe3pct","YDPLd.14.P_fe5pct")]

	X = genet
	Z = scale(X)
	G = tcrossprod(Z) / ncol(Z)
#	mean(diag(G))

# check that the samples are well sorted
# 2013
	X3 = genet[use_13,]
	y3 = pheno[use_13,]
	combinat_13 = combinat_13[use_13,]

# 2014
	X4 = genet[use_14,]
	y4 = pheno[use_14,]
	combinat_14 = combinat_14[use_14,]

# samples distribution on the first components
	pc = princomp(G)
	variance = (pc$sdev)^2
	varexpl = variance / sum(variance)
	
#	par( mfcol=c(1,2))
#	plot( cumsum(varexpl), xlim=c(1,29.5), type="h", lwd=4, ylim=c(0,1))
#	abline( h=0.8, col=2)
#	biplot( pc, cex=c(0.7), ylabs=NULL)
#	par( mfcol=c(1,1))
	 #biplot(pc,cex=c(0.7),c(2,3),ylabs=NULL)
