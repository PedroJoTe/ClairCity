rm(list=ls())
ProjectWD <- "/media/helle/Data/Pinrolinvic/Drive E/Latihan R"
setwd(ProjectWD)
Cleandata <- read.table(file ="/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCity/NetherlandClean.csv", header=T, sep="," )
attach(Cleandata)
D15<-subset(Cleandata,Years==2015)
E15 <- (NL_load_[Years==2015])
file1<- cbind(D15,E15)
detach(Cleandata)

## Finding 2015 pattern
TE15 <- sum(E15)
upto <- length(E15)
Patt <- seq(1,upto)
for (i in 1:upto){
  Patt[i] <- E15[i]/TE15
}
Pattern<- cbind(D15,Patt)

## Generate Selected Days Pattern
attach(Pattern)
PSDE <- matrix(0,4,24)
for (i in 1:4){
  tahun <- 2015
  bulan <- c(2,2,8,8)
  tanggal <- c(11,15,12,16)
  PSDE[i,]<- (Pattern$Patt[Years==tahun & Months==bulan[i] & Dates==tanggal[i]])
}
PatternSDE <- PSDE
write.table(PatternSDE, "PatternSDE2.csv", sep =";") 
detach(Pattern)

###>>>> data set 2 <<<<<####

data1 <- read.table(file ="/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCity/NL_ADAM/AdamClean3.csv", header=T, sep="," )
attach(data1)
TE <- sum(data1$elecv)   
uptox <- length(data1$hhid)

## Combine Pattern to Claircity adam data

PE211 <- matrix(0,uptox,24)
tictoc1 <- system.time(
  for (i in 1:uptox){
    PE211[i,]<- data1$elecv[i]*PSDE[1,]
  })

PE215 <- matrix(0,uptox,24)
tictoc2 <- system.time(
  for (i in 1:uptox){
    PE215[i,]<- data1$elecv[i]*PSDE[2,]
  })

PE812 <- matrix(0,uptox,24)
tictoc3 <- system.time(
  for (i in 1:uptox){
    PE812[i,]<- data1$elecv[i]*PSDE[3,]
  })

PE816 <- matrix(0,uptox,24)
tictoc4 <- system.time(
  for (i in 1:uptox){
    PE816[i,]<- data1$elecv[i]*PSDE[4,]
  })

AEleCon<- cbind(data1[c(-3,-4,-6)],PE211,PE215,PE812,PE816) # electricity
write.table(AEleCon, "EleConsumpPerHouse.csv", sep =";", row.names = FALSE) 
detach(data1)

#####>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


## Finding Profile by category, categorized by total family member
#summing gasv consumption by tfm, from 1 to 13 persons
EleC <- seq(1,13)
for (i in 1:13){
  EleC[i]<- sum(data1$elecv[tfm==i])
}

#Percentage of gas Consumption to overcome 5 profiles
PEleC <- seq(1,13)
for (i in 1:13){
  PEleC[i] <- (EleC[i]/TE)*100
}
Profile <- c(PEleC[1:4], sum(PEleC[5:13]))
write.table(Profile, "EleConsumpProfile.csv", sep =";") 

