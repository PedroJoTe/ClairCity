rm(list=ls())
ProjectWD <- "/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCityFinal"
setwd(ProjectWD)
Cleandata <- read.table(file ="/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCity/aarhus.csv", header=T, sep="," )
attach(Cleandata)
D15<-subset(Cleandata,Years==2015)
H15 <- (TotalHeating[Years==2015])
file1<- cbind(D15,H15)

## Finding 2015 pattern
TH15 <- sum(H15)
upto <- length(H15)
Patt <- seq(1,upto)
for (i in 1:upto){
  Patt[i] <- H15[i]/TH15
}
Pattern<- cbind(D15,Patt)

## Generate Selected Days Pattern
attach(Pattern)
PSD <- matrix(0,4,24)
for (i in 1:4){
tahun <- 2015
bulan <- c(2,2,8,8)
tanggal <- c(11,15,12,16)
PSD[i,]<- (Pattern$Patt[Years==tahun & Months==bulan[i] & Dates==tanggal[i]])
}
PatternSD <- PSD
#write.table(PatternSD, "GasPatternSD.csv", sep =";",row.names = FALSE) 
###>>>> data set 2 <<<<<####

data1 <- read.table(file ="/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCity/NL_ADAM/AdamClean1.csv", header=T, sep="," )
attach(data1)
#summing gas Consumption in 2015
TG <- sum(data1$gasv)
TGSD <- TG*PSD
#write.table(TGSD, "HHLGasConsumpSD.csv", sep =";",row.names = FALSE) 

## Finding Profile by category, categorized by total family member
#summing number of person in every hh 
xx <- length(hhid)
tfm <- seq(1,xx)
for (i in 1:xx){
  tfm[i] <-sum(N15_44[c(i)],N00_14[c(i)],N45_64[c(i)],N65_AO[c(i)])
}
tfm <-as.data.frame(tfm)
datax<- cbind(data1,tfm)
datax<- datax[c(-5,-6,-7,-8,-9,-12,-13)]


## Finding Profile by category, categorized by total family member
#summing gasv consumption by tfm, from 1 to 13 persons
GasC <- seq(1,13)
for (i in 1:13){
  GasC[i]<- sum(datax$gasv[tfm==i])
}

#Percentage of gas Consumption to overcome 5 profiles
PGasC <- seq(1,13)
for (i in 1:13){
  PGasC[i] <- (GasC[i]/TG)*100
}
Profile <- c(PGasC[1:4], sum(PGasC[5:13]))
#write.table(Profile, "GasConsumpPercentageSD.csv", sep =";",row.names = FALSE) 

##>>>>>>>> third Section, Combine Pattern and Profile <<<<<###

#Profile 1, which is household with one family member
#total gas X pattern selected days

Profile1 <- ((Profile[1]*TG)/100)*PatternSD  
Profile2 <- ((Profile[2]*TG)/100)*PatternSD
Profile3 <- ((Profile[3]*TG)/100)*PatternSD
Profile4 <- ((Profile[4]*TG)/100)*PatternSD
Profile5 <- ((Profile[5]*TG)/100)*PatternSD

All <- rbind(Profile1,Profile2,Profile3,Profile4,Profile5)
#write.table(All, "GasConsumpProfileSD.csv", sep =";",row.names = FALSE) 

#######>>>>>>>>>>>>>>>>>>>>

psHHL <- function(bulan,tanggal,sd){
tahun <- 2015
#bulan <- 8
#tanggal <- 16

plot(TGSD[sd,],  type="b", col="green", col.lab = "blue", #ylim =c(0.05, 0.15),
     xlab=paste("Hourly GAS Consumption on ",bulan,'/',tanggal,'/',tahun,' (mm/dd/yyyy)', sep=""), 
     ylab="Kilo Watt Hour (kWh)" )
title("Amsterdam HH Consumption Profile", #sub = "---------------",
      cex.main = 1,   font.main= 2, col.main= "black",
      cex.sub = 0.79, font.sub = 4, col.sub = "black" )
}

##multiple plot
par(mfrow=c(2,2))   ## figure 3
psHHL(2,11,1)
psHHL(2,15,2)
psHHL(8,12,3)
psHHL(8,16,4)
