##############################################################################
######################## code to analize data with ML and SL models #####
####          Created by Marcela Alfaro Cordoba Oct, 2017           ####
##############################################################################

rm(list=ls())
##### Load data: #####
load(file="alldataX.Rdata")

## Use only TS in the Atlantic as a response and 9 different data sets. Start with data set 9

data<-(cbind(yy0[[1]], dataX[[1]]))
matplot(scale(data),type="l",col=c(1,rep(2,56)))
matlines(scale(data)[,1])

#Poisson Regression


#LASSO
#Ridge
#BMA
#Neural Networks



