rm(list=ls())
library(vioplot) # Load violin Plot
library(reshape2)
# Set working directory and load Full observations
setwd("~/Dropbox/FamosHydroModel/Official_Fast/output/")
load("../input/fullObservations.RData") # Load Full observation

# Compile precalibration data
for(i in 1:14){
  load(paste("../precalibration/output/preCalibrationResults",i,".RData",sep=""))
  if(i==1){
    modelOutput<-outputMat[-nrow(outputMat),]
  }else{
    modelOutput<-cbind(modelOutput,outputMat[-nrow(outputMat),])
  }
}

for(i in 1:14){
  print(i)
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

# Load parameter data
load("../precalibration/output/mhParameters_0.RData")
parMat<-parMat[1:nrow(modelOutputValidation),-1]
parMat<-cbind(parMat,1:nrow(parMat))
colnames(parMat)<-c(colnames(parMat)[-13],"runIndex")
parNames<-colnames(parMat)

# Load parameter data


# Training Data
trainStart<-which(obs[,1]==2003 & obs[,2]==6 & obs[,3]==1)
trainEnd<-which(obs[,1]==2008 & obs[,2]==3 & obs[,3]==31)
trainDatObs<-obs[trainStart:trainEnd,] #1766 observations
trainDat<-cbind(parMat,t(modelOutput))
colnames(trainDat)<-c(parNames, paste("obs",1:nrow(trainDatObs), sep=""))
trainDat<-data.frame(trainDat)
foo<-melt(trainDat , id=parNames)
colnames(foo)[14]<-"index"
trainDates<-data.frame(trainDatObs, paste("obs",1:nrow(trainDatObs), sep=""))
colnames(trainDates)<-c("year" , "month" , "day" ,"truth","index")
trainDat<-merge(x = foo, y=trainDates , by = "index")


# Test Data 
testStart<-which(obs[,1]==2009 & obs[,2]==1 & obs[,3]==1)
testEnd<-which(obs[,1]==2011 & obs[,2]==10 & obs[,3]==01)
testDataObs<-obs[testStart:testEnd,] #1766 observations #1004 observations
testDat<-cbind(parMat,modelOutputValidation)
colnames(testDat)<-c(parNames, paste("obs",1:nrow(testDataObs), sep=""))
testDat<-data.frame(testDat)
foo<-melt(testDat , id=parNames)
colnames(foo)[14]<-"index"
testDates<-data.frame(testDataObs, paste("obs",1:nrow(testDataObs), sep=""))
colnames(testDates)<-c("year" , "month" , "day" ,"truth","index")
testDat<-merge(x = foo, y=testDates , by = "index")

save(testDat , trainDat , trainDatObs , testDataObs , file="~/Dropbox/FamosHydroModel/TaskMX/traintest_04172022.RData")



# Dataset for Neural Network
load("../traintest_04172022.RData")
rm(trainDatObs, testDataObs, testDat)

# Create model-fitting and validation dataset
mean(table(trainDat$index)/5026) # Check if there are 5026 runs total
trainInd<-sample(1:5026, 4021) # 80% Training
testInd<-(1:5026)[-trainInd] # 20% Test

# Create Data set to train NN model 
useTrain<-which(trainDat$runIndex%in%trainInd)
trainingDataset<-trainDat[useTrain,]
useTest<-which(trainDat$runIndex%in%testInd)
testDataset<-trainDat[useTest,]
save(testDataset, trainingDataset , file="../NN_datasetTrain_04172022.RData")
nrow(trainingDataset)+nrow(testDataset)==nrow(trainDat)# CHeck
