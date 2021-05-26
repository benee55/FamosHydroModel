rm(list=ls())
setwd("/glade/u/home/sanjib/FamosHydroModel/Official_Fast/precalibration")
source("../run/rWrapper_Continuous.R")
source("../run/mcmc_source_Tr.R")

# Precalibration
ensembleN<-100
parMat<-apply(boundMat,1,function(x,ens){runif(ens,min=x[1],max=x[2])},ens=ensembleN)
parMat<-cbind(rinvgamma(ensembleN,shape = priorPar[1,1], rate = priorPar[1,2]),parMat)
save(parMat, file="output/mhParameters_0.RData")

source("../run/rWrapper_Continuous.R")
source("../run/mcmc_source_Tr.R")
load("output/mhParameters_0.RData")
inputDir<-"/glade/scratch/sanjib/precalibration/input"
outputDir<-"/glade/scratch/sanjib/precalibration/output"
jobNum=1
jobPar<-parMat[jobNum,]
pt<-proc.time()
outputMat<- modelEval( par = jobPar , j = jobNum , inputDir =inputDir , outputDir = outputDir)
ptFinal<-proc.time()-pt

save(outputMat,ptFinal,file = "preCalibrationResults.RData")
