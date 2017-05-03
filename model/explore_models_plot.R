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
