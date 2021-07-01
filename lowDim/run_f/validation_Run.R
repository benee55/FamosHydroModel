
library(snow);library(Rmpi);library(doParallel);library(foreach)
setwd("/glade/u/home/sanjib/FamosHydroModel/lowDim/output_f")

# Parallelize
nprocs <-mpi.universe.size() - 1
print(nprocs)

mp_type = "MPI"
cl <- parallel::makeCluster(spec = nprocs, type=mp_type)
doParallel::registerDoParallel(cl)

# Values for runs
runIndex<-1:2015
outputMat<-foreach::foreach(jobNum=runIndex , .combine = "cbind" , .packages = c("mvtnorm","tmvtnorm","invgamma")) %dopar% {
  source("../run_f/rWrapper_validation.R")
  source("../run_f/mcmc_source_Tr.R")
  load(file="mhParameters_4.RData")
  inputDir<-"/glade/scratch/sanjib/validationLowDim/input"
  outputDir<-"/glade/scratch/sanjib/validationLowDim/output"
  jobPar<-parMat[jobNum,]
  modelEval(par = jobPar , j = jobNum , inputDir =inputDir , outputDir = outputDir)
}

save(outputMat,file = paste("validationResults",jobIndex,".RData",sep=""))

