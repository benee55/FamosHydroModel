# Generate Pre-calibration Samples

rm(list=ls())
setwd("/glade/u/home/sanjib/FamosHydroModel/lowDim")
# setwd("~/Dropbox/FamosHydroModel/lowDim/")
source("run/mcmc_source_Tr.R")
source("run/rWrapper_Continuous.R")

J=5
ensembleN<-J^4
seqInterval<-t(apply(boundMat,1,function(x,j){seq(x[1],x[2],length.out = j)}, j=J))

parMat<-expand.grid(seqInterval[1,],seqInterval[2,], seqInterval[3,],seqInterval[4,])
save(parMat, file="input/design.RData")

