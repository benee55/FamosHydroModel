rm(list=ls())
library(vioplot)
# Set working directory and load Full observations
setwd("~/Dropbox/FamosHydroModel/Official_Fast/output/")
load("../input/fullObservations.RData") # Load Full observation
load("../precalibration/output/mhParameters_0.RData")
# source("../run/mcmc_source_Tr.R")
# Compile precalibration data
for(i in 1:14){
  load(paste("../precalibration/output/preCalibrationResults",i,".RData",sep=""))
  if(i==1){
    modelOutput<-outputMat[-nrow(outputMat),]
  }else{
    modelOutput<-cbind(modelOutput,outputMat[-nrow(outputMat),])
  }
}
# Compile precalibration data - Validation

for(i in 1:14){
  
  if(i==1){
    load(paste("~/Dropbox/FamosHydroModel/Official_Fast/output_validation/preCalibrationResults",i,".RData",sep=""))
    bar<-unlist(outputMat[1,])
    modelOutputValidation<-matrix(bar, nrow=length(outputMat[1,]), ncol=length(outputMat[1,1][[1]]), byrow = TRUE)
    
  }else{
    load(paste("~/Dropbox/FamosHydroModel/Official_Fast/output_validation/preCalibrationResults",i,".RData",sep=""))
    bar<-unlist(outputMat[1,])
    foo<-matrix(bar, nrow=length(outputMat[1,]), ncol=length(outputMat[1,1][[1]]), byrow = TRUE)
    modelOutputValidation<-rbind(modelOutputValidation,foo)
  }
}


# Format Date
dateVect<-paste(sprintf("%04d",as.numeric(obs[,1])),
                sprintf("%02d",as.numeric(obs[,2])),
                sprintf("%02d",as.numeric(obs[,3])),sep="-")
dateVect<-as.Date(dateVect, format = "%Y-%m-%d")
2003/06/01-2008/03/31

modelStart<-which(dateVect==as.Date("2003-06-01")) # Remove Spin-up
modelEnd<-which(dateVect==as.Date("2008-03-31")) # Trim Last Days
newModelOutput<-modelOutput[modelStart:modelEnd,] # Trimmed Data
newDateVect<-dateVect[modelStart:modelEnd] # Trimmed Dates
flowObs<-obs[modelStart:modelEnd,4]

# observation Index
extremeDate<-dateVect[obsInd] # Extreme Dates
extremeObs<-subsetFinalObs # Extreme Values
validationDate<-dateVect[validationInd] # Extreme Dates
validationObs<-subsetFinalValidation # Extreme Values

# Begin PCA
M<-t(modelOutput) # p x n
meanVect<-as.numeric(t(M)%*%rep(1/nrow(M),nrow(M))) # Mean
centerdM<-M - matrix(meanVect,nrow=nrow(M) , ncol= ncol(M),byrow=TRUE) #Center the model output

svdVal<-svd(centerdM) # SIngular Value Decomposition
Jy<-which.min(abs(cumsum(svdVal$d)/sum(svdVal$d)-0.9)) ; print(Jy) # Rank
K<-svdVal$v[,1:Jy]%*%diag(sqrt(svdVal$d[1:Jy])) # n x p
Y_i<-t(solve(t(K)%*%K)%*%t(K)%*%t(centerdM)) # Projections of all center outputs -> Dimension-reduction

U<-svdVal$v[,1:Jy]
D_half<-diag(sqrt(svdVal$d[1:Jy]))

Z_proj<-(U%*%D_half)%*%t(Y_i) # Recover original dimensions (smoothing)
Z_proj<-t(Z_proj)

p1<-Z_proj[2,]+meanVect # Dimension-reduced recovery
p2<-centerdM[2,]+meanVect # Truth output
err<-p1-p2
sum(err^2)/ncol(M)


plot(x=newDateVect, y=p1 , typ="l"  , main="Truth vs. Approximation")
lines(x=newDateVect, y=p2 , col="blue" )
plot(x=newDateVect,y=p1-p2, typ="l"  , main="Errors")

# Observations
projZ<-t(solve(t(K)%*%K)%*%t(K)%*%(flowObs-meanVect))
p1<-flowObs
p2<-(U%*%D_half)%*%t(projZ)+meanVect
err<-p1-p2
sum(err^2)/length(flowObs)
plot(x=newDateVect, y=p1 , typ="l")
lines(x=newDateVect, y=p2 , col="red")


plot(x=newDateVect, y=err , typ="l")



######## WHICH REDUCED SPACE COEEFS
# Using reduced-dimensional space
redY<-apply(Y_i,1,function(x)sum((x-projZ)^2))
closeInd<-which.min(redY)
Z_proj<-(U%*%D_half)%*%t(Y_i) # Recover original dimensions (smoothing)
Z_proj<-t(Z_proj)
projected_estimate<-Z_proj[closeInd,]+meanVect 


plot(x=newDateVect, y=flowObs , typ="l" ,ylim=range(projected_estimate,flowObs,modelOutput[,closeInd]))
lines(x=newDateVect, y=projected_estimate , col="red")
lines(x=newDateVect, y=modelOutput[,closeInd] , col="blue")


# Using full-dimensional space
redY<-apply(modelOutput,2,function(x)sum((x-flowObs)^2))
closeInd2<-which.min(redY)

plot(x=newDateVect, y=flowObs , typ="l" ,ylim=range(flowObs,modelOutput[,closeInd2]))
lines(x=newDateVect, y=modelOutput[,closeInd2] , col="blue")



plot(x=newDateVect, y=flowObs , typ="l" ,ylim=range(flowObs,modelOutput[,closeInd2],modelOutput[,closeInd]))
lines(x=newDateVect, y=modelOutput[,closeInd2] , col="blue")
lines(x=newDateVect, y=modelOutput[,closeInd] , col="red")


plot(x=newDateVect, y=modelOutput[,closeInd2] , typ="l" ,
     ylim=range(modelOutput[,closeInd2],modelOutput[,closeInd], extremeObs))
lines(x=newDateVect, y=modelOutput[,closeInd] , col="blue")
points(x=extremeDate , y= extremeObs, pch=16, col="red")

lines(x=newDateVect, y=modelOutput[,closeInd2] , col="blue")
lines(x=newDateVect, y=modelOutput[,closeInd] , col="red")

##############################################################################
# Find extreme basis Functions
extremeIndex<-which(newDateVect%in%extremeDate)
basisMatrix<-(U%*%D_half)
centeredObs<-(flowObs-meanVect); centeredExtremeObs<-centeredObs[extremeIndex]
extremeObs==(centeredExtremeObs+meanVect[extremeIndex])


####################################################################################
# Which model runs are above activation?
greater5k<-apply(modelOutput[extremeIndex,],2,function(x){sum(x>4950.55)})
largeModelIndex<-which(greater5k>=21)
summary(largeModelIndex)
largeParMat<-parMat[largeModelIndex,-1]


extremeLargeModelOutput<-modelOutput[extremeIndex,largeModelIndex]
extremeLargeValidationOutput<-t(modelOutputValidation[largeModelIndex,])
scoreVal<-apply(extremeLargeModelOutput,2,function(x){
  sum(dnorm(x=extremeObs , mean=x , sd=2000, log=TRUE))
  })
goodRuns<-which(scoreVal>quantile(scoreVal, probs=0.95, na.rm = TRUE))



# Parameters
boundMat<-rbind(c(0, 5) , # PCTIM 0.3=original maximum
                c(0 , 2), # ADIMP 0.5=original maximum
                c(-50 , -0.1), # UZTWM
                c(-70 , -0.1), # LZTWM
                c(-100 , -0.1), # LZFSM
                c(-100 , -0.1), # LZFPM Old -120
                c(-3.8 , -0.1), # LZSK
                c(0.5 , 1.5), # snow_SCF
                c(-3.5 , -0.1), # REXP
                c(-3.5 , -0.1), # UZK
                c(0.5,4.5), # rutpix_Q0CHN
                c(0.3,1.9)) # rutpix_QMCHN Use Original: 3.4 ; BPrior: 2.25 
parNames<-c("PCTIM" , "ADIMP" , "UZTWM" ,"LZTWM" , 
            "LZFSM" , "LZFPM" , "LZSK" , "snow_SCF" , 
            "REXP" , "UZK" , "Q0CHN" , "QMCHN")
par(mfrow=c(3,4), mar=c(2,2,2,2))
for(k in 1:ncol(largeParMat)){
  d1<-density(largeParMat[,k])
  plot(d1, xlim=range(d1$x,boundMat[k,]), main=parNames[k])
  abline(v=boundMat[k,], col="red")
}

par(mfrow=c(3,4), mar=c(2,2,2,2))
for(k in 1:ncol(largeParMat)){
  d1<-density(largeParMat[goodRuns,k])
  plot(d1, xlim=range(d1$x,boundMat[k,]), main=parNames[k])
  abline(v=boundMat[k,], col="red")
}

par(mfrow=c(5,5), mar=c(2,2,2,2))
for(h in 1:length(extremeDate)){
  useDat<-extremeLargeModelOutput[h,goodRuns]
  plot(density(useDat), xlim=range(useDat, extremeObs[h], 4950.55), main=extremeDate[h])
  abline(v=4950.55, col="red", lty=2)
  abline(v=extremeObs[h], col="blue", lty=2)
  
}

par(mfrow=c(5,4), mar=c(2,2,2,2))
for(h in 1:length(validationDate)){
  useDat<-extremeLargeValidationOutput[h,goodRuns]
  plot(density(useDat), xlim=range(useDat, extremeObs[h], 4950.55), main=validationDate[h])
  abline(v=4950.55, col="red", lty=2)
  abline(v=validationObs[h], col="blue", lty=2)
  
}
extremeLargeValidationOutput