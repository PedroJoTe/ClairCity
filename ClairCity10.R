rm(list=ls())
ProjectWD <- "/media/helle/Data/Pinrolinvic/Drive E/Latihan R"
setwd(ProjectWD)
Cleandata <- read.table(file ="/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCity/aarhus.csv", header=T, sep="," )
attach(Cleandata)
D15<-subset(Cleandata,Years==2015)
H15 <- (TotalHeating[Years==2015])
file1<- cbind(D15,H15)
detach(Cleandata)

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
#write.table(PatternSD, "PatternSD.csv", sep =";") 
detach(Pattern)

###>>>> data set 2 <<<<<####

data1 <- read.table(file ="/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCity/NL_ADAM/AdamClean3.csv", header=T, sep="," )
attach(data1)
#summing gas Consumption in 2015
TG <- sum(data1$gasv)   
uptox <- length(data1$hhid)

## Combine Pattern to Claircity adam data
PG211 <- matrix(0,uptox,24)
tictoc1 <- system.time(
  for (i in 1:uptox){
    PG211[i,]<- data1$gasv[i]*PSD[1,]
  })

uptox <- length(data1$hhid)
PG215 <- matrix(0,uptox,24)
tictoc2 <- system.time(
  for (i in 1:uptox){
    PG215[i,]<- data1$gasv[i]*PSD[2,]
  })

uptox <- length(data1$hhid)
PG812 <- matrix(0,uptox,24)
tictoc3 <- system.time(
  for (i in 1:uptox){
    PG812[i,]<- data1$gasv[i]*PSD[3,]
  })

uptox <- length(data1$hhid)
PG816 <- matrix(0,uptox,24)
tictoc4 <- system.time(
  for (i in 1:uptox){
    PG816[i,]<- data1$gasv[i]*PSD[4,]
  })

AGasCon<- cbind(data1[c(-3,-4,-5)],PG211,PG215,PG812,PG816) #Gas
write.table(AGasCon, "GasConsumpPerHouse.csv", sep =";", row.names = FALSE) 
detach(data1)

#####>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
write.table(Profile, "GasConsumpProfile.csv", sep =";") 
