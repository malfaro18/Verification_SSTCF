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


par(mfrow=c(3,3))
cc<-rep(NA,length(X))
for(i in 1:length(X)){
smallbasis  <- create.fourier.basis(c(0, 12), 5)
tempfd1 <- smooth.basis(t, t(X[[i]][,-c(1,2)]), smallbasis)$fd
precip.Temp.f <- fRegress(Ypred ~ tempfd1)
cc[i]<-precip.Temp.f$OCV
plot(precip.Temp.f$betaestlist[[2]], main=paste("FReg Coefficient for", namesVAR[i],cc[i]))
print(i)
}

namesVAR[order(cc)]

for(i in 1:length(X)){
  tt<-order(cc)[i]
tempfd1 <- smooth.basis(t, t(X[[tt]][,-c(1,2)]), smallbasis)$fd
plot(pca.fd(tempfd1)$meanfd, main=paste(namesVAR[tt],tt))
}

tempfd1 <- smooth.basis(t, t(X[[1]][,-c(1,2)]), smallbasis)$fd
tempfd2 <- smooth.basis(t, t(X[[18]][,-c(1,2)]), smallbasis)$fd


temp.cor = cor.fd(c(1:12),tempfd2)
filled.contour(c(1:12),c(1:12),temp.cor)
         


## We examine predicting a scalar response from a functional covariate. 

# In this case, we'll predict log annual precipitation from the temperature
# profile.  This is one of the most over-used examples in FDA; so get it out
# of your system here. 

#### 1. Setup Data

# First we'll obtain log annual precipitation

annualprec = log10( Y)[-c(1:5)]

# Now we need to set up a list of covariates.

xlist = list(len=2)

# First co-variate is just the intercept: a vector of ones

xlist[[1]] = rep(1,dim(X[[1]])[1])

# Second covariate is temperature

xlist[[2]] = tempfd1


#### 2. fdPar objects for coeffients

# We also need a list of functional parameter objects to define the coefficient
# functions. 

bwtlist = list(len=2)

# First is a constant basis and add it to the fdPar object

cbasis = create.constant.basis(c(1,12))
bwtlist[[1]] = fdPar(cbasis)

# Now we need the coefficient of temperature, we'll use the same basis for it
# and add it as the second element in the list. 

harmLfd = vec2Lfd(c(0,(2*pi/365)^2,0),rangeval=c(1,12))
fbasis = create.fourier.basis(c(1,12),11)
beta.fdPar = fdPar(fbasis,harmLfd,1^12.5)
bwtlist[[2]] = beta.fdPar

#### 3. fRegress and outcome

prec.model = fRegress(annualprec,xlist,bwtlist)

# Let's look at the outcome

names(prec.model)

# We can see the intercept as 

prec.model$betaestlist[[1]]$fd$coef

# We can also plot the estimated regression function

plot(prec.model$betaestlist[[2]])


#### 4. Cross-validation

### We should look at selecting lambda. We'll do this with OCV

lambdas = 10^(seq(5,15,0.5))

ocvs = rep(0,length(lambdas))

for(ilam in 1:length(lambdas)){
  bwtlisti = bwtlist                # define temporary beta.fdPar and bwtlist
  beta.fdPari = beta.fdPar
  
  beta.fdPari$lambda = lambdas[ilam]   # update lambda
  bwtlisti[[2]] = beta.fdPari
  
  prec.modeli = fRegress(annualprec,xlist,bwtlisti)
  
  ocvs[ilam] = prec.modeli$OCV        # record ocv
}

plot(lambdas,ocvs,type='b',log='x')

# It looks like our original choice of 12.5 was about right. 



#### 4. Statistics, Standard Errors and Tests 

# Degrees of freedom

prec.model$df 

# We'll plot y-by-yhat (in this case yhatfdobj is just a scalar despite 
# its name). 

yhat = prec.model$yhatfdobj

plot(yhat,annualprec)
abline(c(0,1))

# And we can caculate a residual variance

sigma = sum( (annualprec-yhat)^2 )/(35-prec.model$df)
sigma


# To obtain standard error estiamtes we can now call

sigmaE = sigma*diag(35)
prec.stderr = fRegress.stderr(prec.model,NULL,sigmaE)

# And we can obtain plots for beta from the estimated and standarderror

betahat = prec.model$betaestlist[[2]]
betastd = prec.stderr$betastderrlist[[2]]


