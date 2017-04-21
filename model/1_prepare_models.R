#setwd("../models")
set.seed(1234)
sampl <- read.table("../../gen/raw/magic_annt_repM_q40_s_maf05_oh06_I350_impt_rrBLUP_samples.txt",h=F)[,1]
genet <- read.table("../../gen/raw/magic_annt_repM_q40_s_maf05_oh06_I350_impt_rrBLUP.in", row.names=as.character(sampl), h=F)
pheno <- read.table("../../phe/line/phenotypes_full.txt",h=T,row.names="id")

 # select only samples with genotype and phenotype data
g.p = intersect(sampl, row.names(pheno))
sampl.n = length(g.p)
 # generate partitions (70%TP vs 30%VP)
# combinations <- matrix(0, nrow=sampl.n, ncol=100, dimnames=list(row=g.p))
# for( i in 1:100 ){ combinations[sample(1:sampl.n, sampl.n*0.3),i] = 1 }  # 0 = train ; 1 = test
# write.table(combinations,"combinations.matrix",sep="\t",col.names=FALSE)
combinations <- as.matrix(read.table("../../mod/combinations.matrix",h=F,row.names=1))

trait.cors = matrix(numeric(0), nrow=100, ncol=9, dimnames=list(NULL,
	c("BayesA", "BayesB", "BayesC", "BRR", "BL", "BLf", "FIXED", "RKHS", "GBLUP")))
cor.13 = list(YDHA=trait.cors, HSW=trait.cors, DF=trait.cors, DPM=trait.cors)
cor.14 = list(YDHA=trait.cors, HSW=trait.cors, DF=trait.cors, DPM=trait.cors)

# get X, y, Z and G matrices. And mean from diagonal.
X = genet[g.p,]
y = pheno[g.p,]
Z = scale(X)

G = tcrossprod(Z) / ncol(Z)
mean(diag(G))
 
 # samples distribution on the first components
pc = princomp(G)
variance = (pc$sdev)^2
varexpl = variance / sum(variance)
par(mfcol=c(1,2))
plot(cumsum(varexpl),xlim=c(1,29.5),type="h",lwd=4)
abline(h=0.8,col=2)
biplot(pc,cex=c(0.7),ylabs=NULL)
par(mfcol=c(1,1))
 #biplot(pc,cex=c(0.7),c(2,3),ylabs=NULL)
