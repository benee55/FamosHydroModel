library(snow);library(Rmpi);library(doParallel);library(foreach);
# initialize an Rmpi environment

ns <- as.numeric(commandArgs(trailingOnly=TRUE))
class(ns)
print("Begin")
cl <- parallel::makeCluster(spec = ns, type="MPI")
print("Made Cluster")
doParallel::registerDoParallel(cl)
print("Registered Cluster")
pt<-proc.time()
outputMat<-foreach::foreach(jobNum=1:(ns*10), .combine = "c") %dopar% {
  Sys.sleep(10)
  jobNum
  }
ptFinal<-proc.time()-pt
print("Parallelized Operations")
print(ptFinal)
save(outputMat,ptFinal, file = "testNew.RData")
print("Job Ended")
