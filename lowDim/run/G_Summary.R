
rm(list=ls())
# setwd("~/Dropbox/FamosHydroModel/lowDim/")

# Try out Emulator
load("output/GPEmulator_Full.RData")
load("output/mcmcResults.RData")
sampID<-sample(50000:100000, 1000)
testMat<-amcmc.out$samples[sampID,1:4]
# PRedict on test parameters
predTest_CM<-predTest<-matrix(NA, nrow=nrow(testMat) , ncol=18)
for(k in 1:18){
  print(k)
  predTest[,k]<-apply(testMat, 1, predict, object=gpEmulator[[k]])
  predTest_CM[,k]<-apply(testMat, 1, predict, object=gpEmulator_CM[[k]])
}


save(predTest,predTest_CM , file="output/finalEmulationResults.RData")


#load observations
load("input/fullObservations.RData")
subsetFinalObs

# Format Date
dateVect<-paste(sprintf("%04d",as.numeric(obs[,1])),
                sprintf("%02d",as.numeric(obs[,2])),
                sprintf("%02d",as.numeric(obs[,3])),sep="-")
dateVect<-as.Date(dateVect, format = "%Y-%m-%d")

# observation Index
extremeDate<-dateVect[obsInd] # Extreme Dates
# PLot Results
par(mfrow=c(5,4), mar=c(2,2,2,2))
for(i in 1:18){
  d1<-density(predTest[,i])
  plot(d1, xlim=range(d1$x,subsetFinalValidation[i],4950.55, na.rm = TRUE), 
       main = extremeDate[i])
  abline(v=subsetFinalValidation[i], col="blue" ,pch=16)
  abline(v=4950.55, col="red" ,lwd=1 , lty=2) # ACtion Stage
}