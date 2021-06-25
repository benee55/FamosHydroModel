rm(list=ls())
# setwd("~/Dropbox/FamosHydroModel/lowDim/")
setwd("/glade/u/home/sanjib/FamosHydroModel/lowDim/")
# Load data
load("output/modelRuns.RData")
# process output
modelRuns<-matrix(NA,nrow=length(outputMat[1,]), ncol=length(unlist(outputMat[1,1])))
for(k in 1:length(outputMat[1,])){
  modelRuns[k,]<-  unlist(outputMat[1,k])
}
summary(modelRuns)

# load Design Matrix
load("input/design.RData")

# Build Emulator via paralellization
# Install on cheyenne by loading gnu module
library(snow);library(snowfall);library(mlegp)
sfInit(parallel=TRUE, cpus=5, type='PSOCK')
# How to do zero-mean GP?
gpEmulator_CM<-mlegp(X=parMat,
                  Z=modelRuns,
                  constantMean = 1,
                  nugget = 0,
                  parallel = TRUE)

save(gpEmulator_CM, modelRuns, parMat, 
     file="output/GPEmulator_Full.RData")  
# How to do zero-mean GP?
gpEmulator<-mlegp(X=parMat,
                  Z=modelRuns,
                  constantMean = 0,
                  nugget = 0,
                  parallel = TRUE)
sfStop()

save(gpEmulator,gpEmulator_CM, modelRuns, parMat, 
     file="output/GPEmulator_Full.RData")  



# Try out Emulator
load("output/GPEmulator_Full.RData")
# First parameter
testSeq<-seq(0,5, length.out = 100)
resVect<-vector("numeric")
for(j in 1:length(testSeq)){
  print(j)
  jobPar<-c(testSeq[j],as.numeric(parMat[1,-1]))
  resVect[j]<-predict(object = gpEmulator[[1]], newData = jobPar)
}

plot.ts(resVect)
, newData = matrix(testPar,nrow=1))


summary(parMat)

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

# Plot
par(mfrow=c(5,5), mar=c(2,2,2,2))
for(i in 1:21){
  vioplot(modelRuns[,i], ylim=range(modelRuns[,i],subsetFinalObs[i],4950.55, na.rm = TRUE), 
          main = extremeDate[i])
  points(x=1, y=subsetFinalObs[i], col="red" ,pch=16)
  abline(h=4950.55, col="red" ,lwd=1 , lty=2) # ACtion Stage
}

# Good runs
foo<-apply(modelRuns, 1, function(x){sum(x>4950.55)})
goodRuns<-which(foo==21)

par(mfrow=c(5,5), mar=c(2,2,2,2))
for(i in 1:21){
  d1<-density(modelRuns[goodRuns,i])
  plot(d1, xlim=range(d1$x,subsetFinalObs[i],4950.55, na.rm = TRUE), 
          main = extremeDate[i])
  abline(v=subsetFinalObs[i], col="blue" ,pch=16)
  abline(v=4950.55, col="red" ,lwd=1 , lty=2) # ACtion Stage
}
