rm(list=ls())
ProjectWD <- "/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCityFinal"
setwd(ProjectWD)

##>>>>>>>> Electricity <<<<###
Cleandata <- read.table(file ="/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCity/NetherlandClean.csv", header=T, sep="," )
attach(Cleandata)
DE15<-subset(Cleandata,Years==2015)
E15 <- (NL_load_[Years==2015])
file1E<- cbind(DE15,E15)

## Finding 2015 pattern1
TE15 <- sum(E15)
upto <- length(E15)
PattE <- seq(1,upto)
for (i in 1:upto){
  PattE[i] <- E15[i]/TE15
}
#PatternE<- cbind(DE15,PattE)
PattEx <- append(PattE, 0, after = length(PattE))  ## adjust data size

##>>>>>>>> Gas <<<<###
Cleandata2 <- read.table(file ="/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCity/aarhus.csv", header=T, sep="," )
attach(Cleandata2)
DH15<-subset(Cleandata2,Years==2015)
H15 <- (TotalHeating[Years==2015])
file1H<- cbind(DH15,H15)

## Finding 2015 pattern2
TH15 <- sum(H15)
upto <- length(H15)
PattH <- seq(1,upto)
for (i in 1:upto){
  PattH[i] <- H15[i]/TH15
}
## Conbine Pattern1 and Pattern2 
NewPatt <- (PattEx+PattH)/2
PatternEH <- cbind(DH15,NewPatt)

## Generate Selected Days Pattern
attach(PatternEH)
PSDEH <- matrix(0,4,24)
for (i in 1:4){
  tahun <- 2015
  bulan <- c(2,2,8,8)
  tanggal <- c(11,15,12,16)
  PSDEH[i,]<- (PatternEH$NewPatt[Years==tahun & Months==bulan[i] & Dates==tanggal[i]])
}
PatternSDP <- PSDEH
#write.table(PatternSDP, "PollutionPatternSD.csv", sep =";", row.names = FALSE) 

###>>>> data set 2 <<<<<####
## PM10 and NOX
data1 <- read.table(file ="/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCity/NL_ADAM/AdamClean1.csv", header=T, sep="," )
attach(data1)
data2 <- read.table(file ="/media/helle/Data/Pinrolinvic/Drive E/Latihan R/ClairCity/NL_ADAM/Adam_emi.csv", header=T, sep=";" )
attach(data2)

##selecting which BU-CODE match with data1
PM10<-data2[which(pollutant=="PM10"), ]
NOx2<-subset(data2,pollutant=="NOX" )
jEMI<- merge(PM10, NOx2, by="zone")
EMI<- jEMI[c(1,3,6)]
names(EMI)<- c("CODE", "PM10_mg","NOX_mg")
##write.table(EMI, "pollution.csv", sep =";") 

attach(EMI)
zona <-levels(BU_CODE)  #data1
zonal <- length(zona)   #data2
sPM10 <- seq(1,zonal)
sNOX <- seq(1,zonal)
for (i in 1:zonal){
  sPM10[i]<- EMI$PM10_mg[CODE==zona[i]]
  sNOX[i]<-  EMI$NOX_mg[CODE==zona[i]]
}
EMIx <- as.data.frame(cbind(sPM10,sNOX))
EMIxx<- cbind(zona,EMIx)   ## emi perzona
#write.table(EMIxx, "EmissionPM10NOXPerZona.csv", sep =";", row.names = FALSE) 
detach(EMI)
detach(data1)

## Total Emission per year
TPM10 <- sum(EMIxx$sPM10) 
TNOX <- sum(EMIxx$sNOX)

## Combine Pattern to PM10 and NOX
## Format 1
PM10SD <- TPM10*PSDEH
PNOXSD <- TNOX*PSDEH
#write.table(PM10SD, "PM10PollutionSD.csv", sep =";", row.names = FALSE) 
#write.table(PNOXSD, "NOXPollutionSD.csv", sep =";", row.names = FALSE) 

## Format 2
PM10SD1 <- TPM10*PSDEH[1,]
PM10SD2 <- TPM10*PSDEH[2,]
PM10SD3 <- TPM10*PSDEH[3,]
PM10SD4 <- TPM10*PSDEH[4,]

Z1 <- c(TPM10,PM10SD1,PM10SD2,PM10SD3,PM10SD4)

NOXSD1 <-  PSDEH[1,]*TNOX
NOXSD2 <-  PSDEH[2,]*TNOX
NOXSD3 <-  PSDEH[3,]*TNOX
NOXSD4 <-  PSDEH[4,]*TNOX

Z2 <- c(TNOX,NOXSD1,NOXSD2,NOXSD3,NOXSD4)
Z3 <- rbind(Z1,Z2)
#write.table(Z3, "PM10And NOXHourlyPollutionSD.csv", sep =";", row.names = FALSE) 

uptox <- length(EMIxx$zona)
## Apply Pattern to PM10
PP211 <- matrix(0,uptox,24)
tictoc1 <- system.time(
  for (i in 1:uptox){
    PP211[i,]<- EMIxx$sPM10[i]*PSDEH[1,]
  })

PP215 <- matrix(0,uptox,24)
tictoc2 <- system.time(
  for (i in 1:uptox){
    PP215[i,]<- EMIxx$sPM10[i]*PSDEH[2,]
  })

PP812 <- matrix(0,uptox,24)
tictoc3 <- system.time(
  for (i in 1:uptox){
    PP812[i,]<- EMIxx$sPM10[i]*PSDEH[3,]
  })

PP816 <- matrix(0,uptox,24)
tictoc4 <- system.time(
  for (i in 1:uptox){
    PP816[i,]<- EMIxx$sPM10[i]*PSDEH[4,]
  })

APP<- cbind(EMIxx[c(-3)],PP211,PP215,PP812,PP816) # electricity
write.table(APP, "PM10PerZona.csv", sep =";", row.names = FALSE) 

## Apply Pattern to NOX
PN211 <- matrix(0,uptox,24)
tictoc1 <- system.time(
  for (i in 1:uptox){
    PN211[i,]<- EMIxx$sNOX[i]*PSDEH[1,]
  })

PN215 <- matrix(0,uptox,24)
tictoc2 <- system.time(
  for (i in 1:uptox){
    PN215[i,]<- EMIxx$sNOX[i]*PSDEH[2,]
  })

PN812 <- matrix(0,uptox,24)
tictoc3 <- system.time(
  for (i in 1:uptox){
    PN812[i,]<- EMIxx$sNOX[i]*PSDEH[3,]
  })

PN816 <- matrix(0,uptox,24)
tictoc4 <- system.time(
  for (i in 1:uptox){
    PN816[i,]<- EMIxx$sNOX[i]*PSDEH[4,]
  })

APN<- cbind(EMIxx[c(-2)],PN211,PN215,PN812,PN816) # electricity
write.table(APN, "NOXPerZona.csv", sep =";", row.names = FALSE) 




##>>>>>>>>>>>>>>>>>>>>>>

psHHL <- function(bulan,tanggal,sd){
  tahun <- 2015
  #bulan <- 8
  #tanggal <- 16
  
  plot(PNOXSD[sd,],  type="b", col="green", col.lab = "blue", #ylim =c(0.05, 0.15),
       xlab=paste("Hourly NOX Emission on ",bulan,'/',tanggal,'/',tahun,' (mm/dd/yyyy)', sep=""), 
       ylab="Mega gram (Mg)" )
  title("Amsterdam HH Emission Profile", #sub = "---------------",
        cex.main = 1,   font.main= 2, col.main= "black",
        cex.sub = 0.79, font.sub = 4, col.sub = "black" )
}

##multiple plot
par(mfrow=c(2,2))   ## figure 3
psHHL(2,11,1)
psHHL(2,15,2)
psHHL(8,12,3)
psHHL(8,16,4)

