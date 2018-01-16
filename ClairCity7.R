rm(list=ls())
ProjectWD <- "/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCityFinal"
setwd(ProjectWD)
Cleandata <- read.table(file ="/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCity/NetherlandClean.csv", header=T, sep="," )
attach(Cleandata)
D15<-subset(Cleandata,Years==2015)
E15 <- (NL_load_[Years==2015])
file1<- cbind(D15,E15)
toKiloWatt <- 1000          # Convert to KW
toAdam <- 0.05              # adam takes 5% of national population
toHH <- 0.2                # hh takes 20 % from total Adam Consumption  
toperHH <- 440397   

AE15 <- (E15*toKiloWatt*toAdam*toHH)
Pattern<- cbind(D15,AE15)

## Generate Selected Days Pattern
attach(Pattern)
PSD <- matrix(0,4,24)
for (i in 1:4){
  tahun <- 2015
  bulan <- c(2,2,8,8)
  tanggal <- c(11,15,12,16)
  PSD[i,]<- (Pattern$AE15[Years==tahun & Months==bulan[i] & Dates==tanggal[i]])
}
PatternSD <- PSD
#write.table(PSD, "HHLEleConsumpSD_FromNetherland.csv", sep =";",row.names = FALSE) 
detach(Pattern)
##>>>>>>> second choice

## Finding 2015 pattern
TE15 <- sum(E15)
upto <- length(E15)
Patt2 <- seq(1,upto)
for (i in 1:upto){
  Patt2[i] <- E15[i]/TE15
}
Pattern2<- cbind(D15,Patt2)

## Generate Selected Days Pattern
attach(Pattern2)
PSD2 <- matrix(0,4,24)
for (i in 1:4){
  tahun <- 2015
  bulan <- c(2,2,8,8)
  tanggal <- c(11,15,12,16)
  PSD2[i,]<- (Pattern2$Patt2[Years==tahun & Months==bulan[i] & Dates==tanggal[i]])
}
PatternSD2 <- PSD2
write.table(PatternSD, "ElePatternSD.csv", sep =";",row.names = FALSE) 
###>>>> data set 2 <<<<<####

data1<- read.table(file ="/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCity/NL_ADAM/AdamClean1.csv", header=T, sep="," )
attach(data1)
#summing gas Consumption in 2015 use carlo data
TE <- sum(data1$elecv)   
TESD <- TE*PSD2
write.table(TESD, "HHLEleConsumpSD_FromCarlosdata.csv", sep =";",row.names = FALSE) 


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

#summing elecv consumption by tfm, from 1 to 13 persons
EleC <- seq(1,13)
for (i in 1:13){
  EleC[i]<- sum(datax$elecv[tfm==i])
}

#Percentage of electricity Consumption to overcome 5 profiles
PEleC <- seq(1,13)
for (i in 1:13){
  PEleC[i] <- (EleC[i]/TE)*100
}
Profile <- c(PEleC[1:4], sum(PEleC[5:13]))
write.table(Profile, "EleConsumpPercentage.csv", sep =";",row.names = FALSE) 

##>>>>>>>> third Section, Combine Pattern and Profile <<<<<###

#Profile 1, which is household with one family member
#total gas X pattern selected days

Profile1 <- ((Profile[1])/100)*PatternSD  
Profile2 <- ((Profile[2])/100)*PatternSD
Profile3 <- ((Profile[3])/100)*PatternSD
Profile4 <- ((Profile[4])/100)*PatternSD
Profile5 <- ((Profile[5])/100)*PatternSD

All <- rbind(Profile1,Profile2,Profile3,Profile4,Profile5)
write.table(All, "EleConsumptionProfileSD_FromNethData.csv", sep =";") 

Profile12 <- ((Profile[1]*TE)/100)*PatternSD2  
Profile22 <- ((Profile[2]*TE)/100)*PatternSD2
Profile32 <- ((Profile[3]*TE)/100)*PatternSD2
Profile42 <- ((Profile[4]*TE)/100)*PatternSD2
Profile52 <- ((Profile[5]*TE)/100)*PatternSD2

All <- rbind(Profile12,Profile22,Profile32,Profile42,Profile52)
write.table(All, "EleConsumptionProfileSD_FromCarlosData.csv", sep =";") 
