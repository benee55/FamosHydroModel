rm(list=ls())

forwardmodel<-function(mu,X){
  return(5*exp(-mu*(X)))
}
muEnd<-5
X=seq(0,5,length.out = muEnd)
mu=1.7
muRange=c(0,muEnd)
dat<-forwardmodel(mu=mu, X=X)
plot(x=X,y=dat, typ="l")


B=200
n=length(X)
kIndex<-100
parVal<-c(seq(0,muEnd,length.out = kIndex),
          runif(B-kIndex,0,muEnd))
dat<-matrix(NA, nrow=B, ncol=n )
for(i in 1:B){
  dat[i,]<-forwardmodel(mu=parVal[i], X=X)
}

plot(x=X, y = dat[1,], typ="l", ylim=range(as.numeric(dat)))
for(k in 1:B){
  lines(x=X, y= dat[k,], col="gray")
}



output<-dat[,3]

outputA<-output[1:kIndex]
outputB<-output[(kIndex+1):B]
plot(x=parVal, y=output)
gpFit<-function(x, distMat, theta){
  covMat<-theta[1]*exp(-distMat/theta[2])
  -dmvnorm(x=as.numeric(x), mean=rep(0,length(x)), sigma = covMat,log = TRUE)
}

distMat<-as.matrix(rdist(parVal))
distMatA<-distMat[1:kIndex,1:kIndex]
estPar<-optim(par = c(1,1), fn = gpFit, x=outputA, distMat=distMatA, method="BFGS")$par

covMat<-estPar[1]*exp(-distMat/estPar[2])
krigingVal<-covMat[(kIndex+1):B,1:kIndex]%*%(solve(covMat[1:kIndex,1:kIndex])%*%outputA)
krigingVal<-as.numeric(krigingVal)
sqrt(mean((krigingVal-outputB)^2))
plot(x=parVal[(kIndex+1):B], y=krigingVal,pch=16,col="blue", ylim=range(output,outputB))
points(x=parVal[(kIndex+1):B], y=outputB,col="red" , pch=16)

################################################################################
# Gaussian Process Emulator
library(fields)
theta<-c(1,0.1)
n=100
locationDat<-sort(runif(n=n))
distMat<-as.matrix(rdist(locationDat))
covMatUse<-theta[1]*exp(-distMat/theta[2])
x<-t(chol(covMatUse))%*%rnorm(n)
plot(x=locationDat, y=x, typ="l")

# 0.05=exp(-d/theta)
# -log(0.05)*0.4
# 
expCov<-function(theta,distMat){
  theta[1]*exp(-distMat/theta[2])
}

library(mvtnorm)
