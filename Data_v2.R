rm(list=ls())

#####################################
#####################################

library(gdata)
library(lars)
library(abind)
library(reshape)
library(ggplot2)
library(MASS)
library(BMA)
library(stringi)
library(plyr)
library(glmnet)
library(refund)
library(fields)

##################################
######   read the data  ##########
##################################

source(file="1.getDATA_base.R")
load("lhfscores.Rdata")
ls()
### Create a list with all variables in the same format (66 years x 13 columns) all together!!
all<-list(amm=amm,amo=amo,ao=ao,censo=censo,dm=dm,epo=epo,ggst=ggst,mdrolr=mdrolr,mdrslp=mdrslp,mdrsst=mdrsst,
          mdru200=mdru200,mdru850=mdru850,mdrv200=mdrv200,mdrv850=mdrv850,mdrvws=mdrvws,mei=mei,nao=nao,
          ngst=ngst,nino12=nino12,nino3=nino3,nino34=nino34,nino4=nino4,pdo=pdo,pna=pna,qbo=qbo,sfi=sfi,
          sgst=sgst,soi=soi,tna=tna,tni=tni,tsa=tsa,whwp=whwp,wp=wp)
rm(list=setdiff(ls(), c("counts","all","nino12forecast","lhf.scores","run.bic","predict.fun","varselfun")))
allave1<-lapply(all,function(x){x[,-1]})
allave1lag1<-lapply(allave1,function(x){rbind(c(rep(NA,12)),x[-66,])})

## Extract PCA from LHF
lhf<-cbind(lhf.scores[2:66,1:5,1],lhf.scores[2:66,1:5,2],
           lhf.scores[2:66,1:5,3],lhf.scores[2:66,1:5,4])
#################################################################

counts<-data.frame(counts)
names(counts)<-c("years","N_TC_NA","N_H_NA","N_MH_NA",
                 "N_TC_CA","N_H_CA","N_MH_CA",
                 "N_TC_GM","N_H_GM","N_MH_GM",
                 "N_TC_T","N_H_T","N_MH_T")

##################################
###### create data sets ##########
##### 9 responses and 3 sets of variables
##################################

#### 9 response variables:
yy0<-  list(counts[2:66,11],counts[2:66,12],counts[2:66,13],
            counts[2:66,5],counts[2:66,6],counts[2:66,7],
            counts[2:66,8],counts[2:66,9],counts[2:66,10])

par(mfrow=c(3,3))
names<-c("ATL TS", "ATL HU", "ATL MH",
         "CAR TS", "CAR HU", "CAR MH",
         "GoM TS", "GoM HU", "GoM MH")
lapply(1:9,function(x)hist(yy0[[x]],xlim=c(0,30), main=paste(names[x]),
       xlab="Number of Storms"))

#### 3 sets of variables:
dim(allave1[[1]]) ## 66 years, 12 months
length(allave1) ## 33
dim(allave1lag1[[1]]) ## 66 years, 12 months
length(allave1lag1) ## 33
allave1[[1]][-66,]==allave1lag1[[1]][-1,]

### pdo: 
allave1[[6]][,12]<-apply(allave1[[6]][,-c(12)],1,mean)

allaverage<-lapply(1:33,function(x){
  apply(allave1[[x]][2:66,c(1:12)],1,mean)})

annualave<-do.call(cbind,allaverage)
colnames(annualave)<-names(all)
col<- colorRampPalette(c("blue", "white", "red"))(20)
a<-heatmap(x = cor(annualave), col = col, symm = TRUE,
            keep.dendro=T)

library(corrplot)
dev.off()
sel<-c(1,0,0,0,0,0,1,0,0,0,1,0,
       1,1,0,0,0,0,1,0,0,0,0,0,
       0,0,0,1,1,1,0,0,1)
aa<-colnames(annualave)[which(sel==1)]
corrplot(cor(annualave[,aa]), type="upper", 
         order="hclust", tl.col="black", tl.srt=45,
         mar=c(1,8.5,2,2))

##### Select variables out:

allave2<-((allave1))
allave2lag1<-((allave1lag1))
allave1a<-lapply(1:33,function(x){allave2[[x]][2:66,c(1,2)]})
allave2a<-lapply(1:33,function(x){allave2[[x]][2:66,c(1:4)]})
allave3a<-lapply(1:33,function(x){allave2[[x]][2:66,c(7:9)]})
#allave1lag1a<-lapply(1:20,function(x){allave2lag1[[x]][2:66,c(7:12)]})

aa1<-data.frame(cbind(as.data.frame(do.call(cbind,allave1a))))
names(aa1)<-c(sapply(names(all), function(s) paste0(s,"0",1:2)))

aa2<-data.frame(cbind(as.data.frame(do.call(cbind,allave2a))))
names(aa2)<-c(sapply(names(all), function(s) paste0(s,"0",1:4)))

aa3<-data.frame(cbind(as.data.frame(do.call(cbind,allave3a))))
names(aa3)<-c(sapply(names(all), function(s) paste0(s,"0",c(7:9))))
dim(aa3) 

########### a1,a2 and a3 have NINO included, but not LHF.
##### We need to create  (a1.1) a1 - NINO, (a1.2) a1, and (a1.3) a1.1 + LHF
##### Same with a2, and a3 (May and Oracle data sets)

names(all)[which(sel==1)] ## list of all variables

aa5<-data.frame(lhf)
can<-c("lhf.win","lhf.spr","lhf.sum","lhf.fal")
names(aa5)<-c(sapply(can, function(s) paste0(s, 1:5)))

### select non nino (NINO12, NINO3, NINO34, NINO4, MEI):
selennino<-function(all){
ni1 <-  c(which(stri_sub(names(all),from=1,to=3)=="nin"),
                 which(stri_sub(names(all),from=1,to=3)=="mei"))
newall<- all[,-ni1]
return(newall)}

selenino<-function(all){
  ni1 <-  c(which(stri_sub(names(all),from=1,to=3)=="nin"))
  newall<- all[,ni1]
  return(newall)}

a1.1<-selennino(aa1);dim(a1.1)
a1.2<-aa1;(dim(a1.2))
a1.3<-cbind(a1.1,aa5[,1:5]);dim(a1.3)
a2.1<-selennino(aa2);dim(a2.1)
a2.2<-aa2;dim(a2.2)
a2.3<-cbind(a2.1,aa5[,1:5]);dim(a2.3)
a3.1<-cbind(a1.1,selenino(aa3));dim(a3.1)
a3.2<-cbind(a1.2,selenino(aa3));dim(a3.2)
a3.3<-cbind(a1.3,selenino(aa3));dim(a3.3)
dataX<-list(a1.1,a1.2,a1.3,a2.1,a2.2,a2.3,a3.1,a3.2,a3.3)
### dataX: JF: non nino, nino, nino+LHF
###     April: non nino, nino, nino+LHF
###    JF+JAS: non nino, nino, nino+LHF

matrix(unlist(lapply(dataX,dim)),nrow=2)
rm(list=setdiff(ls(), c("dataX","yy0")))

##################################
###### save data        ##########
save(yy0,dataX,file="alldataX.Rdata")
##################################

##### Description:
par(mfrow=c(3,3))
lapply(1:9,function(x)image.plot(as.matrix(dataX[[x]])))
lapply(1:9,function(x)hist(yy0[[x]], xlim=c(0,35), xlab="",
                      main=paste("Histogram of ",x)))
lapply(1:9,function(c)names(dataX[[c]]))
lapply(1:9,function(c)dim(dataX[[c]]))

##################################
##################################






