rm(list=ls())
library(vioplot) # Load violin Plot
library(reshape2)
# Set working directory and load Full observations
setwd("~/Dropbox/FamosHydroModel/Official_Fast/output/")
load("../input/fullObservations.RData") # Load Full observation

# Format Date
dateVect<-paste(sprintf("%04d",as.numeric(obs[,1])),
                sprintf("%02d",as.numeric(obs[,2])),
                sprintf("%02d",as.numeric(obs[,3])),sep="-")
dateVect<-as.Date(dateVect, format = "%Y-%m-%d")

#Training Data
trainStart<-which(dateVect==as.Date("2003-06-01")) # Remove Spin-up
trainEnd<-which(dateVect==as.Date("2008-03-31")) # Trim Last Days
trainDateVect<-dateVect[trainStart:trainEnd] # Trimmed Dates
trainObs<-obs[trainStart:trainEnd,4] 
trainExtremeDate<-trainDateVect[obsInd] # Extreme Dates
trainExtremeObs<-trainObs[obsInd] # Extreme Values
sum(trainObs[obsInd]-subsetFinalObs) # Check

# Test Data 
testStart<-which(dateVect==as.Date("2009-01-01")) # Remove Spin-up
testEnd<-which(dateVect==as.Date("2011-10-01")) # Trim Last Days
testDateVect<-dateVect[testStart:testEnd] # Trimmed Dates
testObs<-obs[testStart:testEnd,4]
testIndex<-which(testObs%in%sort(testObs, decreasing = TRUE)[1:18]) # Index for extreme observations
testExtremeDate<-testDateVect[testIndex] # Extreme Dates
testExtremeObs<-testObs[testIndex] # Extreme Values
sum(testObs[testIndex]-subsetFinalValidation) # Check

save(trainDateVect , trainObs , trainExtremeDate , trainExtremeObs , 
     testDateVect , testObs , testExtremeDate , testExtremeObs , file="dataForCalibration04172022.RData")