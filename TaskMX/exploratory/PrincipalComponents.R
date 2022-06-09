rm(list=ls())
library(vioplot)
# Set working directory and load Full observations
setwd("~/Dropbox/FamosHydroModel/Official_Fast/output/")
load("../input/fullObservations.RData") # Load Full observation
load("../precalibration/output/mhParameters_0.RData")

# Compile precalibration data
for(i in 1:14){
  load(paste("../precalibration/output/preCalibrationResults",i,".RData",sep=""))
  if(i==1){
    modelOutput<-outputMat[-nrow(outputMat),]
  }else{
    modelOutput<-cbind(modelOutput,outputMat[-nrow(outputMat),])
  }
}
# Compile precalibration data - For validation Runs
for(i in 1:14){
  
  if(i==1){
    load(paste("~/Dropbox/FamosHydroModel/Official_Fast/output_validation/preCalibrationResults",i,".RData",sep=""))
    bar<-unlist(outputMat[2,])
    modelOutputValidation<-matrix(bar, nrow=length(outputMat[2,]), ncol=length(outputMat[2,1][[1]]), byrow = TRUE)
    
  }else{
    load(paste("~/Dropbox/FamosHydroModel/Official_Fast/output_validation/preCalibrationResults",i,".RData",sep=""))
    bar<-unlist(outputMat[2,])
    foo<-matrix(bar, nrow=length(outputMat[2,]), ncol=length(outputMat[2,1][[1]]), byrow = TRUE)
    modelOutputValidation<-rbind(modelOutputValidation,foo)
  }
}


# Format Date
dateVect<-paste(sprintf("%04d",as.numeric(obs[,1])),
                sprintf("%02d",as.numeric(obs[,2])),
                sprintf("%02d",as.numeric(obs[,3])),sep="-")
dateVect<-as.Date(dateVect, format = "%Y-%m-%d")

# Model Time Series
# 2003/06/01-2008/03/31
modelStart<-which(dateVect==as.Date("2003-06-01")) # Remove Spin-up
modelEnd<-which(dateVect==as.Date("2008-03-31")) # Trim Last Days
newModelOutput<-modelOutput[modelStart:modelEnd,] # Trimmed Data
newDateVect<-dateVect[modelStart:modelEnd] # Trimmed Dates
flowObs<-obs[modelStart:modelEnd,4]

# Validation Time Series
# "20090101T00","20111001T23"
modelStart_val<-which(dateVect==as.Date("2009-01-01")) # Remove Spin-up
modelEnd_val<-which(dateVect==as.Date("2011-10-01")) # Trim Last Days
newModelOutput_val<-modelOutputValidation # Trimmed Data
newDateVect_val<-dateVect[modelStart_val:modelEnd_val] # Trimmed Dates
flowObs_val<-obs[modelStart_val:modelEnd_val,4]

# observation Index
extremeDate<-dateVect[obsInd] # Extreme Dates
extremeObs<-subsetFinalObs # Extreme Values
validationDate<-dateVect[validationInd] # Extreme Dates
validationObs<-subsetFinalValidation # Extreme Values

# Data Summary
plot(x=c(newDateVect,newDateVect_val) , 
     y = c(flowObs,flowObs_val), typ="n", main= "Selinsgrove Streamflow", 
     xlab="Date" , ylab="Streamflow")
lines(x=c(newDateVect) , 
      y = c(flowObs), col="black")
lines(x=c(newDateVect_val) , 
      y = c(flowObs_val), col="blue")
points(x=extremeDate , y = extremeObs , pch=16, cex=1.5, col="red")
points(x=validationDate , y = validationObs , pch=16, cex=1.5, col="red")
abline(h=4950.55, col="red" , lty=2)


library(RSpectra)
# Begin PCA
M<-t(modelOutput) # p x n
meanVect<-as.numeric(t(M)%*%rep(1/nrow(M),nrow(M))) # Mean
centerdM<-M - matrix(meanVect,nrow=nrow(M) , ncol= ncol(M),byrow=TRUE) #Center the model output
svdVal<-svds(A = centerdM , k = 1766) # SIngular Value Decomposition

# Examine the Principal COmponents 
## This is proportional to the empirical sample covariance matrix of t(M) 
## Sample Covariance matrix is (1/(n-1))*t(M)%(M)
U<-svdVal$v
D_half<-diag((svdVal$d)) # Note that the d are square roots of the eigenvectors. No need to take square root. 
PC<-U%*%D_half
plot(x=newDateVect , y = PC[,1], typ="l", ylim=range(PC), 
     main="Principal Components (1-50)", ylab="streamflow" , xlab="Date", 
     lwd=0.5)
for(k in 2:50){
  lines(x=newDateVect , y = PC[,k], col=k , lwd=0.5)  
}


################################################################################################
################################################################################################
################################################################################################
skreeVal<-abs(cumsum(svdVal$d^2)/sum(svdVal$d^2))
plot.ts(skreeVal, main = "Skree Plot" , xlab="Number of PCs" , 
        ylab="Variability ")
Jy<-which.min(abs(skreeVal-0.99)) ; print(Jy) # Rank
# Jy<-1750
U<-svdVal$v[,1:Jy]
D_half<-diag((svdVal$d[1:Jy]))

K<-U%*%D_half # n x p
Y_i<-t(solve(t(K)%*%K)%*%t(K)%*%t(centerdM)) # Projections of all center outputs -> Dimension-reduction


# Project Observations down to lower dimensional space
projZ<-t(solve(t(K)%*%K)%*%t(K)%*%(flowObs-meanVect))
p1<-flowObs
p2<-K%*%t(projZ)+meanVect
sqrt(sum((p1-p2)^2)/length(p1))
sqrt(mean((p1-p2)^2))# Information Loss 
plot(x=newDateVect, y=p1 , typ="l")
lines(x=newDateVect, y=p2 , col="red")
legend("topright", legend = c("True Obs", "Low-dim Approx"), 
       lty=c(1,1), col=c("black","red"))
points(x=extremeDate , y = extremeObs , pch=16, cex=1.5, col="red")
abline(h=4950.55, col="red" , lty=2)
######################################################
######################################################
# Select Best Model + CHeck if it is above activation level 
######################################################
######################################################

######## WHICH REDUCED SPACE COEEFS
# Using reduced-dimensional space
redY<-apply(Y_i,1,function(x)sum((x-projZ)^2)) # Difference between truth and model output in reduced dimension space
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

