rm(list=ls())
setwd("~/Dropbox/FamosHydroModel/Official_Fast/output/")
load("../input/fullObservations.RData") # Load Full observation

# Format Date
dateVect<-paste(sprintf("%04d",as.numeric(obs[,1])),
                sprintf("%02d",as.numeric(obs[,2])),
                sprintf("%02d",as.numeric(obs[,3])),sep="-")
dateVect<-as.Date(dateVect, format = "%Y-%m-%d")

# observation Index
extremeDate<-dateVect[obsInd] # Extreme Dates
extremeObs<-subsetFinalObs # Extreme Values

fullParMat<-list()
extremeOutput<-list()
output<-list()
for(k in 1:5){
  load(paste("mhParameters_",k,".RData",sep=""))
  fullParMat[[k]]<-parMat
  extremeOutput[[k]]<-matrix(NA,
                        nrow=length(initResultsList[[2]]), 
                        ncol=length(initResultsList[[2]][[1]][[1]]))
  output[[k]]<-matrix(NA,
                        nrow=length(initResultsList[[2]]), 
                        ncol=length(initResultsList[[2]][[1]][[2]]))
  for(h in 1:length(initResultsList[[2]])){

      extremeOutput[[k]][h,]<-initResultsList[[2]][[h]][[1]]
      output[[k]][h,]<-initResultsList[[2]][[h]][[2]]
  }
}

head(parMat)
parNames<-c("S2",
            "PCTIM" , "ADIMP" , "UZTWM" ,"LZTWM" , 
            "LZFSM" , "LZFPM" , "LZSK" , "snow_SCF" , 
            "REXP" , "UZK" , "Q0CHN" , "QMCHN")

par(mfrow=c(4,4), mar=c(2,2,2,2))
for(k in 1:ncol(fullParMat[[1]])){
dens<-list()
for(h in 1:5){dens[[h]]<-density(fullParMat[[h]][,k])}
plot(dens[[1]], 
     xlim=range(dens[[1]]$x , dens[[2]]$x , dens[[3]]$x , dens[[4]]$x, dens[[5]]$x) ,  
     ylim=range(dens[[1]]$y , dens[[2]]$y , dens[[3]]$y , dens[[4]]$y, dens[[5]]$y), 
     main=parNames[k])
for(j in 1:5){lines((dens[[j]]), col=j)}
}


# Plot Model Output
par(mfrow=c(5,5), mar=c(2,2,2,2))
for(k in 1:ncol(extremeOutput[[1]])){
  dens<-list()
  for(h in 1:5){dens[[h]]<-density(extremeOutput[[h]][,k])}
  plot(dens[[1]], 
       xlim=range(dens[[1]]$x , dens[[2]]$x , dens[[3]]$x , dens[[4]]$x , dens[[5]]$x) ,  
       ylim=range(dens[[1]]$y , dens[[2]]$y , dens[[3]]$y , dens[[4]]$y , dens[[5]]$y), 
       main=extremeDate[k])
  for(j in 1:5){lines((dens[[j]]), col=j)}
  abline(v=extremeObs[k], col="red", lwd=2)
}


# Plot Violin Plots
library(vioplot)
par(mfrow=c(5,5), mar=c(2,2,2,2))
for(i in 1:21){
  vioplot(extremeOutput[[5]][,i], ylim=range(extremeOutput[[5]][,i],extremeObs[i],4950.55, na.rm=TRUE), 
          main = extremeDate[i])
  points(x=1, y=extremeObs[i], col="red" ,pch=16)
  abline(h=4950.55, col="red" ,lwd=1 , lty=2) # ACtion Stage
}



# Plot Model Output
par(mfrow=c(5,5), mar=c(2,2,2,2))
for(k in 1:ncol(extremeOutput[[1]])){
  dens<-density(extremeOutput[[5]][,k])
  plot(dens, 
       xlim=range(dens$x ) ,  
       ylim=range(dens$y ), 
       main=extremeDate[k])
  abline(v=extremeObs[k], col="red", lwd=2)
}

# Figure - Streamflow 
par(mfrow=c(1,1), mar=c(5,4,2,2))
newModelOutput<-t(output[[4]])
plot(x=dateVect, y= obs[,4], typ="n", 
     ylim=range(newModelOutput, na.rm = TRUE), xlim=c(as.Date("2004-08-01") , as.Date("2008-03-31")),
     ylab="Streamflow" , xlab="Date " , 
     main="Pre-calibration Streamflow")
for(k in 1:ncol(newModelOutput)){
  lines(x=dateVect, y= newModelOutput[,k] , col="gray" , lwd=0.5)
}
lines(x=dateVect, y= obs[,4] , col="blue", lwd=1)
points(x=extremeDate , y = extremeObs , col="red" , pch=16, cex=1.5)
abline(h=4950.55, col="red", lty=2)
legend("topright" , legend=c("Observations" , "Model Output" , "Extreme Points" , "Action Stage"),
       lty=c(1,1,NA,2) , pch=c(NA,NA,16,NA), col=c("blue","gray","red","red"),
       lwd=rep(2,2,NA,1),cex=0.75)





sqrt(summary(fullParMat[[5]][,1]))
load("mhParameters_5.RData")
mean(acceptVect)     
