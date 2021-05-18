rm(list=ls())
setwd("/gpfs/group/kzk10/default/private/hydrocalib/SGrove/famos/Official_Fast/precalibration")
source("../run/rWrapper_Continuous.R")
source("../run/mcmc_source_Tr.R")
inputDir<-"~/Dropbox/hydroFamos/run/precalibration/input"
outputDir<-"~/Dropbox/hydroFamos/run/precalibration/output"
# Precalibration
ensembleN<-100
parMat<-apply(boundMat,1,function(x,ens){runif(ens,min=x[1],max=x[2])},ens=ensembleN)
# Write Input
writeInput( par = parMat[1,] , j = j , dir = inputDir)# Write Input
writeOutput( j = j , dir = outputDir) # Write Output
pt<-proc.time()
runHydroModel( j = j , dir = inputDir) # Run Model
ptFinal<-proc.time()-pt
save(ptFinal, file = "FinalTime.RData")