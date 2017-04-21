y = phenotypes13$YDHA$BLUP[G.P.13,1]
combinations = combinations13

runBayesA
	ETA = list(list(model="BayesA", X=X))
	yNA = y
	indexNA = combinations[,1]==0
	yNA[indexNA] = NA
		fm.BA = BGLR(y=yNA, ETA=ETA, nIter= 30000, burnIn= 5000, verbose=FALSE)

runBayesLasso
	ETA = list(list(model="BL", X=X))
	yNA = y
	indexNA = combinations[,1]==0
	yNA[indexNA] = NA
		fm.BL = BGLR(y=yNA, ETA=ETA, nIter= 30000, burnIn= 5000, verbose=FALSE)

runBayesB
	ETA = list(list(model="BayesB", X=X))
	yNA = y
	indexNA = combinations[,1]==0
	yNA[indexNA] = NA
		fm.BB = BGLR(y=yNA, ETA=ETA, nIter= 30000, burnIn= 5000, verbose=FALSE)

runBayesC
	ETA = list(list(model="BayesC", X=X))
	yNA = y
	indexNA = combinations[,1]==0
	yNA[indexNA] = NA
		fm.BC = BGLR(y=yNA, ETA=ETA, nIter= 30000, burnIn= 5000, verbose=FALSE)

cor(fm.BA$yHat[indexNA], y[indexNA])
cor(fm.BC$yHat[indexNA], y[indexNA])
cor(fm.BB$yHat[indexNA], y[indexNA])
cor(fm.BL$yHat[indexNA], y[indexNA])

pos.map <- read.table("../../gen/raw/magic_annt_repM_q40_s_maf05_oh06_I350_impt.map")
names(pos.map) <- c("chr","id","pos")
chrcol=c("purple4","red","goldenrod4","midnightblue","brown2","cyan4","yellow","green3","orange","slateblue4","saddlebrown")
par(mfrow=c(2,2))

plot(fm.BA$ETA[[1]]$b, cex=0.5, pch=19, col=chrcol[pos.map$chr],main="Bayes A", xlab="Physical Order", xaxt="n", ylab="Beta Value")
abline(h=0)
hist(fm.BA$ETA[[1]]$b, breaks=300, col=8, main="Effects Distribution", xlab="Beta Value")
abline(v=0)

plot(fm.BB$ETA[[1]]$b, cex=0.5, pch=19, col=chrcol[pos.map$chr],main="Bayes B", xlab="Physical Order", xaxt="n", ylab="Beta Value")
abline(h=0)
hist(fm.BB$ETA[[1]]$b, breaks=300, col=8, main="Effects Distribution", xlab="Beta Value",xlim=c(-0.04,0.04))
abline(v=0)

plot(fm.BC$ETA[[1]]$b, cex=0.5, pch=19, col=chrcol[pos.map$chr],main="Bayes C", xlab="Physical Order", xaxt="n", ylab="Beta Value")
abline(h=0)
hist(fm.BC$ETA[[1]]$b, breaks=300, col=8, main="Effects Distribution", xlab="Beta Value",xlim=c(-0.04,0.04))
abline(v=0)

plot(fm.BL$ETA[[1]]$b, cex=0.5, pch=19, col=chrcol[pos.map$chr],main="Bayesian Lasso", xlab="Physical Order", xaxt="n", ylab="Beta Value",ylim=c(-0.5,0.5))
abline(h=0)
hist(fm.BL$ETA[[1]]$b, breaks=1000, col=8, main="Effects Distribution", xlab="Beta Value",xlim=c(-0.3,0.3))
abline(v=0)













runGBLUP
	ETA = list(list(model="RKHS", K=G))
 		yNA = y
		indexNA = combinations[,1]==0
		yNA[indexNA] = NA
		fm = BGLR(y=yNA, ETA=ETA, nIter= 30000, burnIn= 5000, verbose=FALSE)
		correlations[i] = cor(fm$yHat[indexNA], y[indexNA])

runRKHS
	ETA = list(list(model="RKHS", K=K))
	D = as.matrix( dist( X, method="euclidean")) ^2
	D = D / mean(D)
	h = 0.5
	K = exp(-h * D)

		yNA = y
		indexNA = combinations[,1]==0
		yNA[indexNA] = NA
		fm = BGLR(y=yNA, ETA=ETA, nIter= 30000, burnIn= 5000)
		correlations[i] = cor(fm$yHat[indexNA], y[indexNA])
