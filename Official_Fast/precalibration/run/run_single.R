rm(list=ls())
setwd("/gpfs/group/kzk10/default/private/hydrocalib/SGrove/famos/Official_Fast/precalibration")
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
inputDir<-"/gpfs/scratch/skl5261/precalibration/input"
outputDir<-"/gpfs/scratch/skl5261/precalibration/output"
jobNum=1
jobPar<-parMat[jobNum,]
outputMat<- modelEval( par = jobPar , j = jobNum , inputDir =inputDir , outputDir = outputDir)

save(outputMat,file = "preCalibrationResults.RData")


# Write Input
# j=1
# dat<-modelEval( par=parMat[j,], j=j , inputDir=inputDir , outputDir=outputDir)
