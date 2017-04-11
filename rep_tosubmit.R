##############################################################################
######################## code to generate tables and plots ###################
####          Created by Marcela Alfaro Cordoba  --- Jan, 2017           ####
##############################################################################

rm(list=ls())
source(file="functions_tosubmit.R") ## includes the LASSO and GLM functions.

##### Load data: #####
load(file="alldataX.Rdata")

# ## Now, run it for all combinations: THIS TAKES TIME! see results in allresultsCOR.Rdata
# a<-b<-rep(list(rep(list(),9)),9)
# for(i in 1:9){
#   for(j in 1:9){
#    Y<-yy0[[i]]
#    X<-dataX[[j]]
#   a[[i]][[j]]<-validation(Y,X,var.sel=TRUE)
#   b[[i]][[j]]<-validation(Y,X,var.sel=FALSE)
#   }
#   print(i)
#   print(j)
# }
# 
# allresults<-list(a,b)
# save(allresults, file="allresultsCOR.Rdata")

##############################################################################
##############################################################################

rm(list=ls())
library(reshape)
library(ggplot2)
source(file="functions_tosubmit.R") ## includes the LASSO and GLM functions.

##### Load data: #####
load(file="allresultsCOR.Rdata") ## load results
### allresults[[1]]: cluster+LASSO results
### allresults[[2]]: cluster results
### allresults[[a]][[response]][[dataset]] has five levels:
### x$tabSWCV = scores per window for SWCV: H1,H2 
### x$tabRCV  = scores per window for RCV:  H1,H2 
### x$resSWCV = observed,predicted and mean per window for SWCV
### x$resRCV  = observed,predicted and mean per window for RCV
### x[[5]]$SWF and x[[5]]$CVnF have selected variables

##############################################################
### First table: compare climatology S(\hat{F}_1 - \hat{F}_2):         
##############################################################

load(file="alldataX.Rdata")
getHclim<-function(data,obs){
  clima1         <-  -dpois(obs,mean(data),log=TRUE)
  nphistfit      <-   ecdf(data)
  clima2         <-  -log(nphistfit(obs)-nphistfit(obs-1))
  H<-clima1-clima2
  return(H)
}

### ATL - TS,HU,MH model F_{1B}:
Hscores<-list()
for(i in 1:9){
Y<-yy0[[i]];Y  ## 1-3
N<-length(Y)
#### Work with w windows of 30 years each + forecasting the 31st. (forecast)
wdef<-cbind(c(1:(N-30)),c(30:(N-1)))
wd<-dim(wdef)[1];wd
## 35 windows of 30 years each
res1<-lapply(1:wd,function(w){try(getHclim(Y[wdef[w,1]:wdef[w,2]],Y[wdef[w,2]+1]))})
## Select random erased value from 1:N (n-fold)
wall<-cbind(rep(1,N),rep(N,N),c(1:N))
res2<-lapply(1:65,function(w){try(getHclim(Y[(1:65)[-wall[w,3]]],Y[wall[w,3]]))})
a<-list(H_SWCV=unlist(res1),H_RCV=unlist(res2))
Hscores[[i]]<-a
}

tab1<-melt(Hscores)
names(tab1)<-c("H","CV","Response")

p <- ggplot(tab1, aes(factor(CV), (H)))
p + geom_boxplot(aes(fill = factor(CV))) +
  facet_grid(.~Response) 

mean.sd <- function(x) c(mean = mean(x[!is.finite(x)==F]), 
                         se = getse(x[!is.finite(x)==F]))

tab2<-lapply(1:9,function(x){
       rbind(cbind(mean.sd(tab1[tab1$CV=="H_SWCV"&tab1$Response==x,"H"]),
                   mean.sd(tab1[tab1$CV=="H_RCV"&tab1$Response==x,"H"])))})

## Number of -inf in the calculation of F_2:
for(i in 1:9){
a<-table(tab1[tab1$CV=="H_SWCV"&tab1$Response==i,"H"])[1]
b<-table(tab1[tab1$CV=="H_RCV"&tab1$Response==i,"H"])[1]
print(paste("Response",i,"SWCV",a[[1]]))
print(paste("Response",i,"RCV",b[[1]]))
}

finaltab<-rbind(
cbind(t(tab2[[1]]),t(tab2[[2]]),t(tab2[[3]])),
cbind(t(tab2[[4]]),t(tab2[[5]]),t(tab2[[6]])),
cbind(t(tab2[[7]]),t(tab2[[8]]),t(tab2[[9]])))

library(xtable)
xtable(finaltab)

##############################################################
### Second figure: Compare two crossvalidation methods:        
### H = S(F_{mclim},y)_SW - S(F_{mlasso},y)_SW - 
###    [S(F_{mclim},y)_RC - S(F_{mlasso},y)_RC] = H1_lasso - H1_cluster

### positive values mean that cluster are better model
#  allresults[[1]][[response]][[dataset]]$tabSWCV[,"H1"] ## H1_lasso SWCV
#  allresults[[2]][[response]][[dataset]]$tabRCV[,"H1"] ## H1_lasso RCV
#  allresults[[1]][[response]][[dataset]][[3]] ## pred, obs lasso SWCV
#  allresults[[2]][[response]][[dataset]][[4]] ## pred, obs lasso RCV
##############################################################

Hs   <- lapply(1:9,function(i)lapply(1:9,function(j){
  cbind(SWCV=allresults[[1]][[i]][[j]]$tabSWCV[,"H1"],
    RCV=allresults[[2]][[i]][[j]]$tabRCV[31:65,"H1"])}))

DspDS   <- lapply(1:9,function(i)lapply(1:9,function(j){
          SWCV=getmse(allresults[[1]][[i]][[j]][[3]][,1],
                    allresults[[1]][[i]][[j]][[3]][,2],
                    allresults[[1]][[i]][[j]][[3]][,3])}))

DspDR   <- lapply(1:9,function(i)lapply(1:9,function(j){
          RCV=getmse(allresults[[1]][[i]][[j]][[4]][31:65,1],
           allresults[[1]][[i]][[j]][[4]][31:65,2],
           allresults[[1]][[i]][[j]][[4]][31:65,3])}))


DsS<-melt(DspDS)
DsR<-melt(DspDR)


mean.sd <- function(x) c(mean = mean(x[which(is.finite(x))]), 
                         se = getse(x[which(is.finite(x))]))

getplot<-function(i){
  tab3    <- lapply(1:9,function(k){mean.sd(Hs[[i]][[k]][,"SWCV"])})
  tab4    <- lapply(1:9,function(k){mean.sd(Hs[[i]][[k]][,"RCV"])})
  tab5    <- (DsS[DsS$L3==1&DsS$L1==i,"value"])
  tab6    <- (DsR[DsR$L3==1&DsR$L1==i,"value"])
  tab7    <- (DsS[DsS$L3==2&DsS$L1==i,"value"])
  tab8    <- (DsR[DsR$L3==2&DsR$L1==i,"value"])
  
  createdf<-function(tab){
  mean<-unlist(lapply(1:9,function(x)tab[[x]]["mean"][[1]]))
  se  <-unlist(lapply(1:9,function(x)tab[[x]]["se"][[1]]))
  df <- data.frame(x =factor(c("F_1B","F_1N","F_1L",
                               "F_2B","F_2N","F_2L",
                               "F_3B","F_3N","F_3L"), 
                             levels=unique(c("F_1B","F_1N","F_1L",
                                             "F_2B","F_2N","F_2L",
                                             "F_3B","F_3N","F_3L"))),
                   H =mean,
                   L =mean-1.96*se,
                   U =mean+1.96*se)
  return(df)}
  
df1<-createdf(tab3)
df2<-createdf(tab4)
df3<-data.frame(x =factor(c("F_1B","F_1N","F_1L","F_2B","F_2N","F_2L","F_3B","F_3N","F_3L"), 
                   levels=unique(c("F_1B","F_1N","F_1L","F_2B","F_2N","F_2L","F_3B","F_3N","F_3L"))),
                   D =tab5,diff1=tab7)
df4<-data.frame(x =factor(c("F_1B","F_1N","F_1L","F_2B","F_2N","F_2L","F_3B","F_3N","F_3L"), 
                          levels=unique(c("F_1B","F_1N","F_1L","F_2B","F_2N","F_2L","F_3B","F_3N","F_3L"))),
                  D =tab6,diff1=tab8)
  
  dff1<-data.frame(rbind(cbind(df3,CV=rep("SWCV",9)),cbind(df4,CV=rep("RCV",9))))
  names(dff1)<-c("x",  "D",  "diff1"  ,"CV")
  dff2<-data.frame(rbind(cbind(df1,CV=rep("SWCV",9)),cbind(df2,CV=rep("RCV",9))))
  names(dff2)<-c("x",  "H",  "L" , "U"  ,"CV")
  
  require(ggplot2)
  
  ann_text<-data.frame(x = (1:9), y = dff1$D-1+(dff1$D/1.5), label = round(dff1$diff1,3)*100, CV=dff1$CV)
  
  p<-ggplot(dff1, aes(x = x, y = D)) +
    geom_point(size = 3) +
    geom_text(data=ann_text,aes(y = dff1$D-1+(dff1$D/1.5),label=label)) +
    #geom_errorbar(aes(ymax = U, ymin = L)) +
    geom_hline(aes(yintercept=0), colour=2) +theme(axis.title.x=element_blank())+
    facet_grid(CV ~ .) +
    theme(axis.title.y=element_blank())
  
  q<-ggplot(dff2, aes(x = x, y = H)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymax = U, ymin = L)) +
    geom_hline(aes(yintercept=0), colour=2) +theme(axis.title.x=element_blank())+
    facet_grid(CV ~ .)
  print(p)
  print(q)
}

getplot(1)

##############################################################
### Third Figure: Compare two models (lasso vs cluster):        
### H = S(F_{mclim},y)_SW - S(F_{mcluster},y)_SW - 
###    [S(F_{mclim},y)_RC - S(F_{mlasso},y)_RC] = H1_lasso - H1_cluster
### positive values mean that cluster are better model
#  allresults[[1]][[response]][[dataset]]$tabSWCV[,"H1"] ## H1_lasso
#  allresults[[2]][[response]][[dataset]]$tabSWCV[,"H1"] ## H1_cluster
##############################################################

Hs   <- lapply(1:9,function(i)lapply(1:9,function(j){
  -allresults[[1]][[i]][[j]]$tabSWCV[,"H1"] +allresults[[2]][[i]][[j]]$tabSWCV[,"H1"]}))

tab1<-melt(Hs)
tab1$value[which(tab1$value< -40)]<- -40 ## truncate numerical errors from LASSO
mean.sd <- function(x) c(mean = mean(x[which(is.finite(x))]), 
                         se = getse(x[which(is.finite(x))]))

getplot<-function(i){
  tab3    <-lapply(1:9,function(k){ ## L1 response, L2 data set
    c(mean.sd(tab1[tab1$L1==i&tab1$L2==k,"value"]))})
  
  mean1<-unlist(lapply(1:9,function(x)tab3[[x]]["mean"][[1]]))
  se1  <-unlist(lapply(1:9,function(x)tab3[[x]]["se"][[1]]))
  df <- data.frame(x =factor(c("F_1B","F_1N","F_1L",
                               "F_2B","F_2N","F_2L",
                               "F_3B","F_3N","F_3L"), 
                             levels=unique(c("F_1B","F_1N","F_1L",
                                             "F_2B","F_2N","F_2L",
                                             "F_3B","F_3N","F_3L"))),
                   H =mean1,
                   L =mean1-1.96*se1,
                   U =mean1+1.96*se1)
  require(ggplot2)
  p<-ggplot(df, aes(x = x, y = H)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymax = U, ymin = L)) +
    geom_hline(aes(yintercept=0), colour=2) +theme(axis.title.x=element_blank())
  
  print(round(rbind(mean1,se1),2))
  print(p)
}

getplot(1)
getplot(2)  
getplot(3)
getplot(4)
getplot(5)
getplot(6)
getplot(7)
getplot(8)
getplot(9)


##############################################################
### Fourth Figure: Compare LASSO models against climatology        
### H = S(F_{mclim},y)_SW - S(F_{mlasso},y)_SW 
### positive values mean that lasso models are better model
#  allresults[[1]][[response]][[dataset]]$tabSWCV[,"H1"] ## H1_lasso
##############################################################

Hs   <- lapply(1:9,function(i)lapply(1:9,function(j){
  allresults[[1]][[i]][[j]]$tabSWCV[,"H1"] }))

tab1<-melt(Hs)
tab1$value[which(tab1$value< -40)]<- -40 ## truncate numerical errors from LASSO
mean.sd <- function(x) c(mean = mean(x[which(is.finite(x))]), 
                         se = getse(x[which(is.finite(x))]))

getplot<-function(i){
  tab3    <-lapply(1:9,function(k){ ## L1 response, L2 data set
    c(mean.sd(tab1[tab1$L1==i&tab1$L2==k,"value"]))})
  
  mean1<-unlist(lapply(1:9,function(x)tab3[[x]]["mean"][[1]]))
  se1  <-unlist(lapply(1:9,function(x)tab3[[x]]["se"][[1]]))
  df <- data.frame(x =factor(c("F_1B","F_1N","F_1L",
                               "F_2B","F_2N","F_2L",
                               "F_3B","F_3N","F_3L"), 
                             levels=unique(c("F_1B","F_1N","F_1L",
                                             "F_2B","F_2N","F_2L",
                                             "F_3B","F_3N","F_3L"))),
                   H =mean1,
                   L =mean1-1.96*se1,
                   U =mean1+1.96*se1)
  require(ggplot2)
  p<-ggplot(df, aes(x = x, y = H)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymax = U, ymin = L)) +
    geom_hline(aes(yintercept=0), colour=2) +theme(axis.title.x=element_blank())
  
  print(round(rbind(mean1,se1),2))
  print(p)
}

getplot(1)
getplot(2)  
getplot(3)
getplot(4)
getplot(5)
getplot(6)
getplot(7)
getplot(8)
getplot(9)

##############################################################
##### Number of variables in each model and construct plot:
### x[[lasso]][[resp]][[dataset]][[4]]$SWF, 
### x[[lasso]][[resp]][[dataset]][[4]]$SWnF, 
### x[[lasso]][[resp]][[dataset]][[4]]$CVnF.
##############################################################

respu<-c("ATL TS","ATL HU","ATL MH",
         "CAR TS","CAR HU","CAR MH",
         "GOM TS","GOM HU","GOM MH")

datasets<-c(expression(paste("F"[11])),expression(paste("F"[12])),
            expression(paste("F"[13])),expression(paste("F"[21])),
            expression(paste("F"[22])),expression(paste("F"[23])),
            expression(paste("F"[31])),expression(paste("F"[32])),
            expression(paste("F"[33])))  

vs<-1 ## LASSO
#vs<-2 ## no var sel

library(fields)
all<-list()
for(j in 1:9){
  var<-sort(unique(unlist(lapply(1:9,function(x)names(allresults[[vs]][[j]][[x]][[5]]$SWF)))))
  collect<-data.frame(matrix(0,length(var),9))
  rownames(collect)<-var
  for(i in 1:9){
    this<-allresults[[vs]][[j]][[i]][[5]]$SWF
    collect[names(this),i]<-this
  }
  all[[j]]<-collect[order(apply(collect,1,sum), decreasing = T),]
  print(dim(all[[j]]))
}

colors<-c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")
set.panel()
par(oma=c( 0,0,0,4)) 
set.panel( 3,3) 
for(i in 1:9){
  titulo<-paste("Percentage of times a variable X is selected using", respu[i])
  data<-all[[i]]
  image(1:20,1:9,as.matrix(all[[i]][1:20,]/(35)), zlim=c(0,1),
        main=titulo,ylab="",xlab="", col=colors, xaxt="n", yaxt="n")
  text(cex=0.9,x=(1:20)-0.4, y=-0.5,rownames(data)[1:20], xpd=T,srt=45)
  text(cex=0.9,y=(1:9), x=-0.2,datasets, xpd=T)#,srt=90)
  #axis(2, at = 1:ncol(data), labels=datasets, )
  aa<-grep('nino', rownames(data)[1:20], value=TRUE)
  ll<-length(aa)
  position<-c()
    for(k in 1:ll){
    position[k]<-which(rownames(data)[1:20]==aa[k])  
    }
  abline(v=c(position-0.5,position+0.5), lwd=2)
  abline(h=c(3.5,6.5), lwd=2)
}
par(oma=c( 0,0,0,2))# reset margin to be much smaller.
image.plot( legend.only=TRUE, zlim=c(0,1), col=colors) 

# ### For non variable selection:

vs<-2;j<-1;  data<-9 ### vs (1 = lasso, 2=cluster) j=1, i=data 1:9
  var<-sort(unique(unlist(lapply(1:9,function(x)names(allresults[[vs]][[j]][[x]][[5]]$SWF)))))
  collect<-data.frame(matrix(0,length(var),9))
  rownames(collect)<-var
  for(i in 1:9){
    this<-allresults[[vs]][[j]][[i]][[5]]$SWF
    collect[names(this),i]<-this
  }
  aa<-collect[order(apply(collect,1,sum), decreasing = T),]
  as.vector(t(cbind(toupper(rownames(aa)[order(aa[,data], decreasing=T)][1:5]),
        paste("(",
  round(aa[order(aa[,data], decreasing=T),data][1:5]/35,2),")"))))

#### END ####

