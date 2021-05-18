rm(list=ls())
setwd("/gpfs/group/kzk10/default/private/hydrocalib/SGrove/famos/Official_Fast/precalibration")
source("../run/rWrapper_Continuous.R")
source("../run/mcmc_source_Tr.R")
inputDir<-"/gpfs/group/kzk10/default/private/hydrocalib/SGrove/famos/Official_Fast/precalibration/input"
outputDir<-"/gpfs/group/kzk10/default/private/hydrocalib/SGrove/famos/Official_Fast/precalibration/output"
# Precalibration
ensembleN<-100
parMat<-apply(boundMat,1,function(x,ens){runif(ens,min=x[1],max=x[2])},ens=ensembleN)
# Write Input
j=1
writeInput( par = parMat[j,] , j = j , dir = inputDir)# Write Input
writeOutput( j = j , dir = outputDir) # Write Output
pt<-proc.time()
runHydroModel( j = j , dir = inputDir) # Run Model
ptFinal<-proc.time()-pt
save(ptFinal, file = "FinalTime.RData")