# Generate Pre-calibration Samples

rm(list=ls())
setwd("/glade/u/home/sanjib/FamosHydroModel/lowDim/precalibration")
setwd("~/Dropbox/FamosHydroModel/lowDim/precalibration")
source("../run/rWrapper_Continuous.R")
source("../run_f/mcmc_source_Tr.R")

# Generate 10k samples
ensembleN<-5030
parMat<-apply(boundMat,1,function(x,ens){runif(ens,min=x[1],max=x[2])},ens=ensembleN)
save(parMat, file="mhParameters_0.RData")