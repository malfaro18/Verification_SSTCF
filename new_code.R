##############################################################################
######################## code to analize data with ML and SL models #####
####          Created by Marcela Alfaro Cordoba Oct, 2017           ####
##############################################################################

rm(list=ls())
##### Load data: #####
load(file="alldataX.Rdata")
source(file="functions_tosubmit.R")

## Use only TS in the Atlantic as a response and 9 different data sets. Start with data set 9 (more complex)
data<-(cbind(yy0[[1]], dataX[[9]]))
matplot(scale(data),type="l",col=c(1,rep(2,56)))
matlines(scale(data)[,1])

## OPtion 1: devide the data into training and testing

runGLM(data[,1],data[,-1],1,64,64:65)
runLASSO(data[,1],data[,-1],1,64,64:65)


## Option 2: Moving Windows

#Poisson Regression
#LASSO
#Ridge
#BMA
#Neural Networks



