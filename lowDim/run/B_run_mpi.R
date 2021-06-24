library(snow);library(Rmpi);library(doParallel);library(foreach)
setwd("/glade/u/home/sanjib/FamosHydroModel/lowDim")

# Parallelize
nprocs <-mpi.universe.size() - 1
print(nprocs)

mp_type = "MPI"
cl <- parallel::makeCluster(spec = nprocs, type=mp_type)
doParallel::registerDoParallel(cl)
load(file="input/design.RData")
ens<-nrow(parMat); rm(parMat)
# Values for runs
outputMat<-foreach::foreach(jobNum=1:ens , .combine = "cbind") %dopar% {
  source("run/rWrapper_Continuous.R")
  source("run/mcmc_source_Tr.R")
  load(file="input/design.RData")
  inputDir<-"/glade/scratch/sanjib/lowDim/input"
  outputDir<-"/glade/scratch/sanjib/lowDim/output"
  jobPar<-parMat[jobNum,]
  modelEval(par = jobPar , j = jobNum , inputDir =inputDir , outputDir = outputDir)
}

save(outputMat,file = "output/modelRuns.RData")

