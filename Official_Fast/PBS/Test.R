library(snow);library(Rmpi);library(doParallel);library(foreach);
# initialize an Rmpi environment
ns <- commandArgs(trailingOnly=TRUE)
print("Begin")
cl <- parallel::makeCluster(spec = ns, type="MPI")
print("Made Cluster")
doParallel::registerDoParallel(cl)
print("Registered Cluster")
outputMat<-foreach::foreach(jobNum=1:ns, .combine = "c") %dopar% {
  jobNum
}
print("Parallelized Operations")
save(outputMat, file = "testNew.RData")
print("Job Ended")
