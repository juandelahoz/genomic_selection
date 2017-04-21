rm(list=ls())
source("1_clean_raw_data.R")
######################################
# 2013 #

 # function to plot phenotype data from 2013
plot.13.field <- function(obs.vector, main="", ylab=""){
	plot(obs.vector,col=0, main=main, ylab=ylab, xlab="", xaxt="n")

	abline(v=seq(0,2160,10),col=c(1,rep("lightgray",71)),lty="dotted")
	text(x=seq(5,2155,10),y=0,labels=1:72,cex=0.35)

	axis(1,at=seq(360,1800,720),labels= paste("rep",1:3),tick=FALSE)
	axis(1,at=seq(0,2160,720),labels=rep("",4))
}

 # EXPLORE THE OBSERVATIONS TABLE #

# yield
#pdf("../../phe/raw/img/MAGIC_2013_RAW_ORIGINAL_yield.pdf",20,10)
	par(mfcol=c(2,1))
plot.13.field(phe13$YDHA, main="Yield per Hectare", ylab="Yield (Kg/Ha)")
	points(phe13$YDHA,pch=20,col=rainbow(12)[phe13$lane])
plot.13.field(phe13$HSW, main="Hundred (100) Seed Weight", ylab="Grams (g)")
	points(phe13$HSW,pch=20,col=rainbow(12)[phe13$lane])
#dev.off()

# time to maturity
#pdf("../../phe/raw/img/MAGIC_2013_RAW_ORIGINAL_days.pdf",20,10)
	par(mfcol=c(2,1))
plot.13.field(c(phe13$DF,max(phe13$DPM)), main="Time to Flowering and to Physiological Maturity", ylab="Days")
	points(phe13$DF,pch=20,col=rainbow(12)[phe13$lane])
	points(phe13$DPM,pch=17,col=rainbow(12)[phe13$lane])
hist(phe13$DPM - phe13$DF, 50, col=8, main="Time from Flowering to Maturity",xlab="Days")
#dev.off()

######################################
# 2014 #

 # function to plot phenotype data from 2014
plot.14.field <- function(obs.vector, main="", ylab=""){
	plot(obs.vector,col=0, main=main, ylab=ylab, xlab="", xaxt="n")
	abline(v=seq(0,1944,18),col=c(1,rep("lightgray",17)),lty="dotted")
	text(x=seq(9,1935,18),y=0,labels=1:18,cex=0.35)
	axis(1,at=seq(162,1782,324),labels=rep(paste("trial",10:11),3),tick=FALSE)
	axis(1,at=seq(324,1620,648),labels=    paste("rep",1:3),padj=3,tick=FALSE)
	axis(1,at=seq(0,1944,648),labels=rep("",4))
}

 # EXPLORE THE OBSERVATIONS TABLE #

 # How are the distributions of TotalSeedWeigh and PercentageOfMoisture?
	par(mfcol=c(2,1))
plot.14.field(phe14$TSW, main="Seed Weight Distribution and Moisture Measurement Limit", ylab="Total Seed Weight (g)")
	abline(h=240,col=3,lwd=5)
	points(phe14$TSW, col=phe14$trl, pch=ifelse(phe14$TSW>=240,"+","-"))
plot.14.field(phe14$POM, main="Percentage of Moisture and Seed Weight Classification", ylab="Percentage of Moisture (%)")
	mean_POM = mean(ifelse( phe14$TSW>=240 & phe14$POM>7 ,phe14$POM,NA),na.rm=TRUE) # 11.92085
	abline(h=mean_POM,col=4,lty=2,lwd=2)
	points(phe14$POM, col=ifelse(phe14$TSW>=240,4,5), pch=ifelse(phe14$TSW>=240,"+","-"))

 # How are the distributions of MetersHarvested and PlantNumber?
	par(mfcol=c(2,1))
plot.14.field(phe14$Mhrl, main="Meters Harvested", ylab="Meters Harvested (m)")
	points(phe14$Mhrl,col=phe14$trl,pch=20)
	#points(phe14$Mhrl,col=ifelse(phe14$TSW>=240,4,5), pch=ifelse(phe14$TSW>=240,"+","-"))
plot.14.field(phe14$PLN,main="Plant Number",ylab="Number of Plants Harvested")
	points(phe14$PLN,col=phe14$trl,pch=20)
	#points(phe14$PLN,col=ifelse(phe14$TSW>=240,4,5), pch=ifelse(phe14$TSW>=240,"+","-"))

 # new distribution of PercentageOfMoisture
	par(mfcol=c(2,1))
	phe14$POMc_2 = ifelse(phe14$TSW<240 | phe14$POM<7, mean_POM, phe14$POM)
hist(phe14$POMc_2,col=4,50,main="New distribution of Percentage of Moisture",xlab="Percentage of Moisture (%)")
plot.14.field(c(phe14$POMc_2,0), main="New Percentage of Moisture and Seed Weight Classification", ylab="Percentage of Moisture (%)")
	points(phe14$POMc_2,col=4,pch="+")

 # Field view of the improved yield measurements for 2014
	par(mfcol=c(4,1))
plot.14.field(phe14$YD_basic, main="Yield >50g (>100g) Seed", ylab="Yield per Hectare")
	points(phe14$YD_50g,         col=ifelse(phe14$TSW>=100,4,2), pch=20)
plot.14.field(phe14$YD_basic, main="Yield >1 (>2) Meters Harvested", ylab="Yield per Hectare")
	points(phe14$YD_1mh,         col=ifelse(phe14$Mhrl>=2,4,2),  pch=20)
plot.14.field(phe14$YD_basic, main="Yield >10 (>20) Plants Harvested", ylab="Yield per Hectare")
	points(phe14$YD_10pl,        col=ifelse(phe14$PLN>=20,4,2),  pch=20)
plot.14.field(phe14$YD_basic, main="Yield with combinations of thresholds", ylab="Yield per Hectare")
	points(phe14$YD_50g_1mh_10pl,col=ifelse(phe14$TSW>=100 & phe14$Mhrl>=2 & phe14$PLN>=20,4,2), pch=20)

 # Histogram view of the improved yield measurements for 2014
par(mfcol=c(2,4))
hist.14.yield <- function(yield.vector,breaks=50,main){
	hist(yield.vector,breaks,col=8,ylim=c(0,150),xlim=c(0,5500),
		xlab="Yield (Kg) per Hectare",main=main)
}
hist.14.yield(phe14$YD_50g  , main="> 50  gram Total Seed Weight")
hist.14.yield(phe14$YD_100g , main="> 100 gram Total Seed Weight")
hist.14.yield(phe14$YD_1mh     , main=">1 Meters Harvested")
hist.14.yield(phe14$YD_2mh  ,30, main=">2 Meters Harvested")
hist.14.yield(phe14$YD_10pl , main=">10 Plants Harvested")
hist.14.yield(phe14$YD_20pl , main=">20 Plants Harvested")
hist.14.yield(phe14$YD_50g_1mh_10pl     , main="50g  seeds, 1m harvested, 10 plants")
hist.14.yield(phe14$YD_100g_2mh_20pl ,30, main="100g seeds, 2m harvested, 20 plants")

 # Field view of the final yield measurements for 2014
par(mfcol=c(2,1))
plot.14.field(phe14$YDHA_basic, main="Yield with thresholds: TSW>100, MH>1, PLN>15", ylab="Yield per Hectare")
	points(phe14$YDHA_basic,col=ifelse(phe14$TSW>100 & phe14$Mhrl>1 & phe14$PLN>15, 4,2), pch=20)
plot.14.field(phe14$YDPL_basic, main="Yield with thresholds: TSW>100, MH>1, PLN>15", ylab="Yield per Plant")
	points(phe14$YDPL_basic,col=ifelse(phe14$TSW>100 & phe14$Mhrl>1 & phe14$PLN>15, 4,2), pch=20)

# quality control of high-yielding observations
par(mfcol=c(2,1))
hist(phe14$YDHA_basic,100,col=8,main="yield without thresholds",xlab="Yield per Hectare")
	abline(v=3100,col=2,lwd=3)
plot.14.field(phe14$YDHA_basic, main="Yield with thresholds: TSW>100, MH>1, PLN>15", ylab="Yield per Hectare")
	points(phe14$YD_basic,col=ifelse(phe14$TSW>100 & phe14$Mhrl>1 & phe14$PLN>15, 4,2), pch=20)
	abline(h=3100,col=3,lwd=1.5)

# check correlations of meters harvested, plant number and total seed weight # with high-yielding highlighted
par(mfcol=c(1,3))
plot(jitter(phe14$Mhrl),jitter(phe14$PLN),xlab="Meters Harvested",ylab="Plant Number",pch=20,col=ifelse(phe14$YDHA_basic<3100, 1,2),cex=ifelse(phe14$YDHA_basic<3100, 1,2),cex.lab=1.5)
	abline(0,10,col=2)
	legend("topleft",legend=paste("R:",round(cor(phe14$Mhrl,phe14$PLN,"complete","pearson"),3)),bty="n")
plot(jitter(phe14$Mhrl),jitter(phe14$TSW),ylab="Total Seed Weight",xlab="MetersHarvested",pch=20,col=ifelse(phe14$YDHA_basic<3100, 1,2),cex=ifelse(phe14$YDHA_basic<3100, 1,2),cex.lab=1.5)
	legend("topleft",legend=paste("R:",round(cor(phe14$Mhrl,phe14$TSW,"complete","pearson"),3)),bty="n")
plot(jitter(phe14$PLN),jitter(phe14$TSW),ylab="Total Seed Weight",xlab="Plant Number",pch=20,col=ifelse(phe14$YDHA_basic<3100, 1,2),cex=ifelse(phe14$YDHA_basic<3100, 1,2),cex.lab=1.5)
	legend("topleft",legend=paste("R:",round(cor(phe14$PLN,phe14$TSW,"complete","pearson"),3)),bty="n")
