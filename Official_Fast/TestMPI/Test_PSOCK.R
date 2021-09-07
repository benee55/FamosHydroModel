library(snow);library(doParallel);library(foreach);
# initialize an Rmpi environment

ns <- as.numeric(commandArgs(trailingOnly=TRUE))
class(ns)
print("Begin")
cl <- parallel::makeCluster(spec = ns, type="PSOCK")
print("Made Cluster")
doParallel::registerDoParallel(cl)
print("Registered Cluster")
outputMat<-foreach::foreach(jobNum=1:ns, .combine = "c") %dopar% {
  sleep(2)
  jobNum
}
print("Parallelized Operations")
save(outputMat, file = "testNew.RData")
print("Job Ended")


