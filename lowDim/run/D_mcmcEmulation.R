# Log prior
logPrior<-function(par, boundMat){
  sum(dunif(x=par[-4],min=boundMat[1,],max=boundMat[2,],log = TRUE),
      dinvgamma(x = par[4] , shape = 2, rate =2, log=TRUE))
}

logLlhd<-function(par,boundMat,gpEmulator,Jy,dat,K){
  coeffs<-predict(gpEmulator[[1]], newData = matrix(par[-4],nrow=1))
  for(i in 2:Jy){
    coeffs<-c(coeffs,
              predict(gpEmulator[[i]], newData = matrix(par[-4],nrow=1)))
  }
  output<-as.numeric(K%*%coeffs)
  # Model Output
  sum(dnorm(x=dat,mean=output,sd=par[4],log=TRUE))
  
}

# Log Posterior
logPost<-function(par,boundMat,gpEmulator,Jy,dat,K){
  
  if(any(par[-4]<boundMat[1,]) | any(par[-4]>boundMat[2,]) | par[4]<0) {
    lPost<- -Inf
  }else{
    lPrior<-logPrior(par=par , boundMat=boundMat)
    llhd<-logLlhd(par = par,
                  boundMat = boundMat,
                  gpEmulator = gpEmulator,
                  Jy = Jy,
                  dat = dat,
                  K = K)
    
    lPost<-lPrior+llhd
  }
  
  return(lPost)
}

# MCMC
library(adaptMCMC)
accept.mcmc = 0.234										# Optimal acceptance rate as # parameters->infinity
#	(Gelman et al, 1996; Roberts et al, 1997)
niter.mcmc =25000										# number of iterations for MCMC
gamma.mcmc = 0.55										# rate of adaptation (between 0.5 and 1, lower is faster adaptation)
burnin = round(niter.mcmc*0.1)				# how much to remove for burn-in
stopadapt.mcmc = round(niter.mcmc*5)

par.init<-c(apply(theta.ori,2,mean),1)
useDat<-fullUvicDat[,20]+rnorm(n=nTot,mean=0,sd=0.1)
# Run MCMC
pt<-proc.time()
amcmc.out = MCMC(p=logPost, n=niter.mcmc, init=par.init, 
                 boundMat = boundMat,
                 gpEmulator = gpEmulator,
                 Jy = Jy,dat = useDat,K=K,
                 acc.rate=accept.mcmc,
                 gamma=gamma.mcmc, list=TRUE, 
                 n.start=round(0.01*niter.mcmc),
                 adapt=TRUE)
ptFinal<-proc.time()-pt
ptFinal
amcmc.out$acceptance.rate
apply(amcmc.out$samples,2,mean)
par(mfrow=c(4,2),mar=c(2,2,2,2))
burnin=1000
for(i in 1:4){
  plot.ts(amcmc.out$samples[-(1:burnin),i])
  abline(h=theta.ori[20,i],col="red")
  plot(density(amcmc.out$samples[-(1:burnin),i]))
  abline(v=theta.ori[20,i],col="red")
}
