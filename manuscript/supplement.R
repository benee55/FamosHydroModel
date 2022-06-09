
rm(list=ls())
setwd("~/Dropbox/FamosHydroModel/Analysis/")
load(file = "resultsStremflow_calibration_extreme.RData")
obsDate<-c(c("190904","200904"),
           c("150105","160105", "300305" , "310305" , "030405" , "040405" ,"050405" , "060405"),
           c("011205"),
           c("280606" , "290606" , "300606"),
           c("181106"),
           c("160307" , "170307"),
           c("080208" , "060308" , "090308" , "100308"))

obsDate<-as.Date(obsDate,format = "%d%m%y")
summaryData<-data.frame(as.character(obsDate), subsetFinalObs)
colnames(summaryData)<-c("Observation Date","Streamflow (mm)")
library(xtable)
print(xtable(summaryData), include.rownames=FALSE)


rm(list=ls())
load(file = "resultsStremflow_validation_extreme.RData")
validationDate<-c("260110" , "270110" ,
           "021210" , "031210" ,
           "070311" , "080311" ,
           "110311" , "120311" , "130311" , "140311" ,
           "280411" , "290411" , "300411" ,
           "080911" , "090911" , "100911" , "110911" , "300911")
validationDate<-as.Date(validationDate,format = "%d%m%y")
summaryData<-data.frame(as.character(validationDate), subsetFinalValidation)
colnames(summaryData)<-c("Observation Date","Streamflow (mm)")
library(xtable)
print(xtable(summaryData), include.rownames=FALSE)

rm(list=ls())
load("calibrationParameters.RData")
handTuneParMat


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

summaryPar<-data.frame(parNames, boundMat)
colnames(summaryPar)<-c("parameter", "lower", "upper")
library(xtable)
print(xtable(summaryPar), include.rownames=FALSE)
 