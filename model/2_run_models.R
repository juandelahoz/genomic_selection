library(BGLR)
# check that the samples are well sorted
X = genet[g.p,]
y = pheno[g.p,]
combinations = combinations[g.p,]

source("Bayes_A.R")
for (i in 9:100){
	
	cor.13$YDHA[i,"BayesA"] = runBayesA( y$YDHA.13.P , X , combinations[,i] )$cor
	write.table(cor.13$YDHA,"results/cor_13_YDHA.txt",sep="\t",row.names=FALSE)
	
	cor.14$YDHA[i,"BayesA"] = runBayesA( y$YDHA.14.P , X , combinations[,i] )$cor
	write.table(cor.14$YDHA,"results/cor_14_YDHA.txt",sep="\t",row.names=FALSE)
}

