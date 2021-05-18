rm(list=ls())
# Observations
obs<-read.table("~/work/hydro/Official_Fast/input/SBYP1_obs.txt" , stringsAsFactors = FALSE)
obs<-obs[-(1:2),]
obs<-t(apply(obs,1,as.numeric))
obsInd<-c(which(obs[,2]==2004 & obs[,3]==9 & obs[,4]%in%c(19,20)),
          which(obs[,2]==2005 & obs[,3]==1 & obs[,4]%in%c(15,16)),
          which(obs[,2]==2005 & obs[,3]==3 & obs[,4]%in%c(30:31)),
          which(obs[,2]==2005 & obs[,3]==4 & obs[,4]%in%c(3:6)),
          which(obs[,2]==2005 & obs[,3]==12 & obs[,4]%in%c(1)),
          which(obs[,2]==2006 & obs[,3]==6 & obs[,4]%in%c(28:30)),
          which(obs[,2]==2006 & obs[,3]==11 & obs[,4]%in%c(18)),
          which(obs[,2]==2007 & obs[,3]==3 & obs[,4]%in%c(16:17)),
          which(obs[,2]==2008 & obs[,3]==2 & obs[,4]%in%c(8)),
          which(obs[,2]==2008 & obs[,3]==3 & obs[,4]%in%c(6,9,10))
)
obs<-obs[obsInd,]
obs<-as.numeric(obs[,5])*0.0283 # Need to convert

save(obs, file="~/work/hydro/Official_Fast/input/obsData.RData")

