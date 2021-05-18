rm(list=ls())
setwd("/gpfs/group/kzk10/default/private/hydrocalib/SGrove/famos/Official_Fast/precalibration")
source("../run/rWrapper_Continuous.R")
source("../run/mcmc_source_Tr.R")
inputDir<-"/gpfs/group/kzk10/default/private/hydrocalib/SGrove/famos/Official_Fast/precalibration/input"
outputDir<-"/gpfs/group/kzk10/default/private/hydrocalib/SGrove/famos/Official_Fast/precalibration/output"
# Precalibration
ensembleN<-100
parMat<-apply(boundMat,1,function(x,ens){runif(ens,min=x[1],max=x[2])},ens=ensembleN)
parMat<-cbind(rinvgamma(ensembleN,shape = priorPar[1,1], rate = priorPar[1,2]),parMat)
# Write Input
j=1
dat<-modelEval( par=parMat[j,], j=j , inputDir=inputDir , outputDir=outputDir)
save(dat,file = "FinalTime.RData")