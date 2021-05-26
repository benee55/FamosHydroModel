# Precalibration Summary
rm(list=ls())
library(vioplot)
# Set working directory and load Full observations
setwd("~/Dropbox/FamosHydroModel/Official_Fast/output/")
load("../input/fullObservations.RData") # Load Full observation
load("../precalibration/output/mhParameters_0.RData")
source("../run/mcmc_source_Tr.R")
# Compile precalibration data
for(i in 1:22){
  load(paste("../precalibration/output/preCalibrationResults",i,".RData",sep=""))
  if(i==1){
    modelOutput<-outputMat[-nrow(outputMat),]
  }else{
    modelOutput<-cbind(modelOutput,outputMat[-nrow(outputMat),])
  }
}
modelOutput<-modelOutput[,-ncol(modelOutput)] # Remove last model run (overlap with below)
load("../precalibration/output/preCalibrationResults_Additional.RData")
modelOutput<-cbind(modelOutput,outputMat[-nrow(outputMat),-1]) # Remove overlapping model run


# Format Date
dateVect<-paste(sprintf("%04d",as.numeric(obs[,1])),
                sprintf("%02d",as.numeric(obs[,2])),
                sprintf("%02d",as.numeric(obs[,3])),sep="-")
dateVect<-as.Date(dateVect, format = "%Y-%m-%d")
  
# observation Index
extremeDate<-dateVect[obsInd] # Extreme Dates
extremeObs<-subsetFinalObs # Extreme Values
modelStart<-which(dateVect==as.Date("2004-08-01")) # Remove Spin-up
modelEnd<-which(dateVect==as.Date("2008-03-31")) # Trim Last Days
keepInd<-1:ncol(modelOutput) # Can use sample() to select subset
newModelOutput<-modelOutput[modelStart:modelEnd,keepInd] # Trimmed Data
newDateVect<-dateVect[modelStart:modelEnd] # Trimmed Dates

############################################################################################################
############################################################################################################
# Scoring
############################################################################################################
############################################################################################################
extremeModelOuput<-modelOutput[obsInd,]
MSE<-apply(extremeModelOuput,2,function(x){mean((x-extremeObs)^2)})
goodRuns<-which(MSE<quantile(MSE, probs=0.05))
goodModelOutput<-modelOutput[modelStart:modelEnd,goodRuns] # Trimmed Data
goodParMat<-parMat[goodRuns,-1]
############################################################################################################
############################################################################################################
# Figures
############################################################################################################
############################################################################################################
# All runs with good runs in green
############################################################################################################
par(mfrow=c(1,1), mar=c(5,4,2,2))
plot(x=newDateVect, y= obs[modelStart:modelEnd,4], typ="n", 
     ylim=range(newModelOutput), xlim=c(as.Date("2004-08-01") , as.Date("2008-03-31")),
     ylab="Streamflow" , xlab="Date " , 
     main="Streamflow")
for(k in 1:ncol(newModelOutput)){
  lines(x=newDateVect, y= newModelOutput[,k] , col="gray" , lwd=0.5)
}
for(k in goodRuns){
  lines(x=newDateVect, y= newModelOutput[,k] , col="green" , lwd=0.5)
}
lines(x=newDateVect, y= obs[modelStart:modelEnd,4] , col="blue", lwd=1)
points(x=extremeDate , y = extremeObs , col="red" , pch=16, cex=1.5)
abline(h=4950.55, col="red", lty=2)
legend("topright" , legend=c("Observations" , "Model Output" , "Good Model Runs","Extreme Points" , "Action Stage"),
       lty=c(1,1,1,NA,2) , pch=c(NA,NA,NA,16,NA), col=c("blue","gray","green","red","red"),
       lwd=rep(2,2,2,NA,1),cex=0.75)
############################################################################################################
############################################################################################################
# Good runs Violin Plots
############################################################################################################
par(mfrow=c(5,5), mar=c(2,2,2,2))
for(i in 1:21){
  k<-obsInd[i]
  vioplot(modelOutput[k,goodRuns], ylim=range(modelOutput[k,goodRuns],extremeObs[i],4950.55), 
          main = extremeDate[i])
  points(x=1, y=extremeObs[i], col="red" ,pch=16)
  abline(h=4950.55, col="red" ,lwd=1 , lty=2) # ACtion Stage
}
############################################################################################################
############################################################################################################
# Good runs Parameters
############################################################################################################
par(mfrow=c(4,3), mar=c(2,2,2,2))
for(i in 1:12){
  plot(density(goodParMat[,i]) , xlim=range(boundMat[i,1:2]),  main=parNames[i+1])
  abline(v=boundMat[i,1:2], col="red")
}


############################################################################################################
############################################################################################################

# Figure - Streamflow 
par(mfrow=c(1,1), mar=c(5,4,2,2))
plot(x=newDateVect, y= obs[modelStart:modelEnd,4], typ="n", 
     ylim=range(newModelOutput), xlim=c(as.Date("2004-08-01") , as.Date("2008-03-31")),
     ylab="Streamflow" , xlab="Date " , 
     main="Pre-calibration Streamflow")
for(k in 1:ncol(newModelOutput)){
  lines(x=newDateVect, y= newModelOutput[,k] , col="gray" , lwd=0.5)
}
lines(x=newDateVect, y= obs[modelStart:modelEnd,4] , col="blue", lwd=1)
points(x=extremeDate , y = extremeObs , col="red" , pch=16, cex=1.5)
abline(h=4950.55, col="red", lty=2)
legend("topright" , legend=c("Observations" , "Model Output" , "Extreme Points" , "Action Stage"),
       lty=c(1,1,NA,2) , pch=c(NA,NA,16,NA), col=c("blue","gray","red","red"),
       lwd=rep(2,2,NA,1),cex=0.75)

# Figure - Violin Plots of Extreme Dates 
par(mfrow=c(5,5), mar=c(2,2,2,2))
for(i in 1:21){
  k<-obsInd[i]
  vioplot(modelOutput[k,], ylim=range(modelOutput[k,],extremeObs[i],4950.55), 
          main = extremeDate[i])
  points(x=1, y=extremeObs[i], col="red" ,pch=16)
  abline(h=4950.55, col="red" ,lwd=1 , lty=2) # ACtion Stage
}


# Figure - 2004-2005 of observations vs. max value of model runs
maxVals<-apply(modelOutput,1,max)
plot(x=dateVect, y=maxVals, lwd=0.5,
     typ="l",xlim=c(as.Date("2004-09-01") , as.Date("2005-04-30")),
     ylim=c(0,20000))
points(x=extremeDate , y = extremeObs , col="red" , pch=16, cex=1.5)
points(x=dateVect , y = maxVals , col="black" , pch=16, cex=1)



# Figure - Streamflow + Includes Spinup Time
useModelOutput<-modelOutput[,keepInd]
par(mfrow=c(1,1), mar=c(5,4,2,2))
plot(x=dateVect, y= obs[,4], typ="n", 
     ylim=range(newModelOutput), 
     ylab="Streamflow" , xlab="Date " , 
     main="Streamflow with Spinup")
for(k in 1:ncol(useModelOutput)){
  lines(x=dateVect, y= useModelOutput[,k] , col="gray" , lwd=0.5)
}
lines(x=dateVect, y= obs[,4] , col="blue", lwd=1)
points(x=extremeDate , y = extremeObs , col="red" , pch=16, cex=1.5)
abline(h=4950.55, col="red", lty=2)
legend("topright" , legend=c("Observations" , "Model Output" , "Extreme Points" , "Action Stage"),
       lty=c(1,1,NA,2) , pch=c(NA,NA,16,NA), col=c("blue","gray","red","red"),
       lwd=rep(2,2,NA,1),cex=0.75)



# Figure - Streamflow of 2005 observations and model runs
par(mfrow=c(1,1), mar=c(5,4,2,2))
plot(x=newDateVect, y= obs[modelStart:modelEnd,4], typ="n", 
     ylim=c(0,15000), xlim=c(as.Date("2005-01-01") , as.Date("2005-04-15")),
     ylab="Streamflow" , xlab="Date " , 
     main="2005 Streamflow")
for(k in 1:ncol(newModelOutput)){
  lines(x=newDateVect, y= newModelOutput[,k] , col="gray" , lwd=0.5)
}
lines(x=newDateVect, y= obs[modelStart:modelEnd,4] , col="blue", lwd=1)
points(x=extremeDate , y = extremeObs , col="red" , pch=16, cex=1.5)
abline(h=4950.55, col="red", lty=2)
legend("topright" , legend=c("Observations" , "Model Output" , "Extreme Points" , "Action Stage"),
       lty=c(1,1,NA,2) , pch=c(NA,NA,16,NA), col=c("blue","gray","red","red"),
       lwd=rep(2,2,NA,1),cex=0.75)

