##############################################################################
######################## code to analize data with FDA models ###############
####          Created by Marcela Alfaro Cordoba Oct, 2017           ####
##############################################################################

rm(list=ls())
source(file="getData_base.R")

ls()
Y<-counts$TTC

library(fda)

transfn<-function(variable){
  nn<-dim(variable)
  fnvar<-cbind(variable[-1,1]+1,variable[-nn[1],-c(2,3)],variable[-1,c(2,3)])
  fnvar<-data.frame(fnvar)[5:65,1:14]
  names(fnvar)<-c("fnyear",names(fnvar)[-1])
  return(fnvar)
}

varlist<-list(amm,amo,ao,censo,dm,epo,ggst,mdrolr,mdrslp,mdrsst,
              mdru200,mdru850,mdrv200,mdrv850,mdrvws,mei,nao,ngst,
              nino12,nino3,nino34,nino4,np,oni,pdo,pna,qbo,sgst,soi,
              tna,tni,tsa,whwp,wp)
X<-lapply(varlist, transfn)
namesVAR<-c("amm","amo","ao","censo","dm","epo","ggst","mdrolr","mdrslp","mdrsst",
            "mdru200","mdru850","mdrv200","mdrv850","mdrvws","mei","nao","ngst",
            "nino12","nino3","nino34","nino4","np","oni","pdo","pna","qbo","sgst","soi",
            "tna","tni","tsa","whwp","wp")

### Plot all functions (34 variables)

for(i in 1:length(X)){
  ccc<-namesVAR[i]
  matplot(t(X[[i]][,-c(1,2)]), type='l', col='gray', main=paste(i,ccc))
  matlines(apply(X[[i]][,-c(1,2)],2,mean), type='l', col='black')
}


t      <- c(1:12)
Ypred  <- log(Y)[-c(1:5)]

cc<-rep(NA,length(X))
#for(i in 1:length(X)){
smallbasis  <- create.fourier.basis(c(0, 12), 7)
tempfd1 <- smooth.basis(t, t(X[[33]][,-c(1,2)]), smallbasis)$fd
tempfd2 <- smooth.basis(t, t(X[[18]][,-c(1,2)]), smallbasis)$fd
tempfd3 <- smooth.basis(t, t(X[[7]][,-c(1,2)]), smallbasis)$fd
tempfd4 <- smooth.basis(t, t(X[[30]][,-c(1,2)]), smallbasis)$fd
tempfd5 <- smooth.basis(t, t(X[[1]][,-c(1,2)]), smallbasis)$fd
tempfd6 <- smooth.basis(t, t(X[[28]][,-c(1,2)]), smallbasis)$fd
precip.Temp.f <- fRegress(Ypred ~ tempfd1+tempfd2+tempfd3+tempfd4+tempfd5+tempfd6)
plot(Ypred,precip.Temp.f$yhatfdobj)
precip.Temp.f$OCV
#print(i)
#}

namesVAR[order(cc)]




