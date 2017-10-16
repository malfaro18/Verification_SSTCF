rm(list=ls())

### Read and format data
### Sources sent by http://www.esrl.noaa.gov/psd/data/climateindices/list/ Jan, 2016
### Check file indices.xls to see list, definitions, formats.

########################################################
########################################################

readhttp<-function(http,nlines){
  amm1<-scan(http,nlines = nlines)
  amm1<-t(matrix(amm1[-c(1:2)],nrow=13)[,-69])
  colnames(amm1)<-c("year", "JAN","FEB","MAR","APR",
                   "MAY","JUN","JUL","AUG",
                   "SEP","OCT","NOV","DEC")
  amm<-amm1[-c(1:2),]
  print(head(amm))
  print(tail(amm))
  return(data.frame(amm))
}

########################################################
########################################################

## AMM (Atlantic Meridional Mode) -- sst
########################################################
h<-"http://sahale.aos.wisc.edu:4080/MModes/Data/AMM.txt"
amm1<-read.table(h, header=T)
hmm<-(ceiling(length(c(amm1[-c(1:24),3]))/12)-(length(c(amm1[-c(1:24),3]))/12))*12
amm<-cbind(unique(amm1[,1])[-c(1:2)],t(matrix(c(amm1[-c(1:24),3],rep(NA,hmm)),nrow=12)))
colnames(amm)<-c("year", "JAN","FEB","MAR","APR",
                  "MAY","JUN","JUL","AUG",
                  "SEP","OCT","NOV","DEC")
amm<-data.frame(amm[-c(67:68),]);rm(amm1);rm(hmm);amm

## AMO (Atlantic Multi-decadal Oscillation)
########################################################
h<-"http://www.esrl.noaa.gov/psd/data/correlation/amon.us.data"
n<-70
amo<-readhttp(h,n); rm(h,n);amo

## TNA (Tropical Northern Atlantic)
########################################################
h<-"http://www.esrl.noaa.gov/psd/data/correlation/tna.data"
n<-70
tna<-readhttp(h,n); rm(h,n);tna

## TSA (Tropical Southern Atlantic)
########################################################
h<-"http://www.esrl.noaa.gov/psd/data/correlation/tsa.data"
n<-70
tsa<-readhttp(h,n); rm(h,n);tsa

## DM (Atlantic Dipole Mode)
## Calculate by using TNA and TSA according to its definition
########################################################
dm<-tna-tsa
dm[,1]<-tsa[,1];dm

## WHWP
########################################################
h<-"http://www.esrl.noaa.gov/psd/data/correlation/whwp.data"
n<-70
whwp<-readhttp(h,n); rm(h,n);whwp


## NINO12
########################################################
h<-"http://www.esrl.noaa.gov/psd/data/correlation/nina1.data"
n<-70
nino12<-readhttp(h,n); rm(h,n);nino12

#### FORECAST: http://www.cpc.ncep.noaa.gov/products/people/wwang/cfsv2_fcst_history/

nino12forecast<-data.frame(t(cbind(c(NA,NA,0.95,0.85,0.80,0.70,0.40,NA,NA,0.05,NA,NA),
                      c(0.45,0.60,0.80,0.95,0.75,1.00,1.35,NA,NA,0.60,NA,NA),
                      c(0.20,-0.25,0.40,-0.50,-0.10,-0.15,-0.45,NA,NA,-0.80,NA,NA),
                      c(0.65,0.65,0.45,0.75,0.80,0.85,0.55,NA,NA,1.25,NA,NA),
                      c(0.30,0.05,0.90,1.20,1.70,1.55,1.60,NA,NA,2.25,NA,NA))))
nino12forecast<-cbind(unique(nino12[62:66,1]),nino12forecast)
colnames(nino12forecast)<-c("year", "JAN","FEB","MAR","APR",
                 "MAY","JUN","JUL","AUG",
                 "SEP","OCT","NOV","DEC")

# dat<-melt(nino12forecast, id="year")
# dat1<-data.frame(rep(1:12,each=5),cbind(dat[,c(1,3)],dat[,3]))
# names(dat1)<-c("Month","Year","value")
# ggplot(dat1, aes(Month, value, colour=as.factor(Year)) )+  scale_colour_brewer(name = "Year",palette=7,type="qual")+
#    geom_line() +geom_point() + xlim(1,11) + ylab("NINO12 Anomalies")
#   


## SOI (Southern Oscillation Index)
########################################################
h<-"http://www.esrl.noaa.gov/psd/data/correlation/soi.data"
n<-70
soi<-readhttp(h,n); rm(h,n);soi

## NAO (Northern Atlantic Oscillation)
########################################################
h<-"http://www.esrl.noaa.gov/psd/data/correlation/nao.data"
n<-70
nao<-readhttp(h,n); rm(h,n);nao

## EPO (East Pacific/North Pacific Oscillation)
########################################################
h<-"http://www.esrl.noaa.gov/psd/data/correlation/epo.data"
n<-70
epo<-readhttp(h,n); rm(h,n);epo
epo[,13]<-apply(epo[,c(2:12)],1,mean)

## PNA (Pacific North American Index)
########################################################
h<-"http://www.esrl.noaa.gov/psd/data/correlation/pna.data"
n<-70
pna<-readhttp(h,n); rm(h,n);pna

## QBO (Quasi-Biennial Oscillation)
########################################################
h<-"http://www.esrl.noaa.gov/psd/data/correlation/qbo.data"
n<-70
qbo<-readhttp(h,n); rm(h,n);qbo

# ## GGST 
# ## Save txt file in computer: http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts.txt
# ########################################################
ggst<-read.fwf(file="http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts.txt",skip=84,n=72,
         width = c(6,rep(5,12), rep(5,6),6))[-c(12,13,34,35,56,57,73:100),-c(14:20)]
ggst<-do.call(cbind,lapply(ggst,function(x)as.numeric(as.character(x))))
colnames(ggst)<-c("year","JAN","FEB","MAR","APR",
                "MAY","JUN","JUL","AUG",
                "SEP","OCT","NOV","DEC")
ggst<-data.frame(ggst);ggst

# ## NGST
# ## Save txt file in computer: http://data.giss.nasa.gov/gistemp/tabledata_v3/NH.Ts.txt
# ########################################################
ngst<-read.fwf(file="http://data.giss.nasa.gov/gistemp/tabledata_v3/NH.Ts.txt",skip=84,n=72,
         width = c(6,rep(5,12), rep(5,6),6))[-c(12,13,34,35,56,57,73:100),-c(14:20)]
ngst<-do.call(cbind,lapply(ngst,function(x)as.numeric(as.character(x))))
colnames(ngst)<-c("year","JAN","FEB","MAR","APR",
                "MAY","JUN","JUL","AUG",
                "SEP","OCT","NOV","DEC")
ngst<-data.frame(ngst);ngst

# ## SGST
# ## Save txt file in computer: http://data.giss.nasa.gov/gistemp/tabledata_v3/SH.Ts.txt
# ########################################################
sgst<-read.fwf(file="http://data.giss.nasa.gov/gistemp/tabledata_v3/SH.Ts.txt",skip=84, n=72,
               width = c(6,rep(5,12), rep(5,6),6))[-c(12,13,34,35,56,57),-c(14:21)]
sgst<-do.call(cbind,lapply(sgst,function(x)as.numeric(as.character(x))))
colnames(sgst)<-c("year","JAN","FEB","MAR","APR",
                  "MAY","JUN","JUL","AUG",
                  "SEP","OCT","NOV","DEC")
sgst<-data.frame(sgst);sgst

## AO (Arctic Oscillation) ### 66 years!!!
ao<-read.csv("http://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/monthly.ao.index.b50.current.ascii.table", header=F, sep='')[-c(1,68,69),]
ao<-matrix(as.numeric(as.matrix(ao)),66,13)
colnames(ao)<-c("year","JAN","FEB","MAR","APR",
                 "MAY","JUN","JUL","AUG",
                 "SEP","OCT","NOV","DEC")
ao<-data.frame(ao);ao

## SFI (Solar Flux)
########################################################
# h<-"sfi.txt"
# sfi1<-data.frame(read.table(h, header=F))
# sfi<-cbind(unique(sfi1[,1])[-c(1:3)],t(matrix(as.numeric(as.character((sfi1[-c(1:36),3]))),nrow=12)))
# sfi[67,3:13]<-NA
# colnames(sfi)<-c("year", "JAN","FEB","MAR","APR",
#                  "MAY","JUN","JUL","AUG",
#                  "SEP","OCT","NOV","DEC")
# sfi<-data.frame(sfi[-67,]);rm(sfi1);sfi

## ftp://ftp.ngdc.noaa.gov/STP/space-weather/solar-data/solar-features/solar-radio/noontime-flux/penticton/penticton_observed/listings/listing_drao_noontime-flux-observed_monthly.txt
## http://www.esrl.noaa.gov/psd/data/correlation/solar.data

## MDROLR
### "http://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype=1&var=OLR&level=2000&lat1=20&lat2=10&lon1=-80&lon2=-20&iseas=0&mon1=0&mon2=0&iarea=0&typeout=1&Submit=Create+Timeseries"
########################################################
#setwd("~/Dropbox/Cursos NCSU/Summer 2013/Dr.Fuentes/paper2_Applied_v2")
mdrolr<-data.frame(read.table("MDROLR.csv", sep=","))[-c(1:2),]
colnames(mdrolr)<-c("year", "JAN","FEB","MAR","APR",
                    "MAY","JUN","JUL","AUG",
                    "SEP","OCT","NOV","DEC")
mdrolr<-data.frame(mdrolr);mdrolr

## MDRSLP
## http://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype=1&var=Sea+Level+Pressure&level=2000&lat1=20&lat2=10&lon1=-80&lon2=-20&iseas=0&mon1=0&mon2=0&iarea=0&typeout=1&Submit=Create+Timeseries
########################################################
mdrslp<-data.frame(read.table("MDRSLP.csv", sep=","))[-c(1:2),]
colnames(mdrslp)<-c("year", "JAN","FEB","MAR","APR",
                 "MAY","JUN","JUL","AUG",
                 "SEP","OCT","NOV","DEC")
mdrslp<-data.frame(mdrslp);mdrslp

## MDRSST
## http://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype=1&var=SST&level=2000&lat1=20&lat2=10&lon1=-80&lon2=-20&iseas=0&mon1=0&mon2=0&iarea=0&typeout=1&Submit=Create+Timeseries
########################################################
mdrsst<-data.frame(read.table("MDRSST.csv", sep=","))[-c(1:2),]
colnames(mdrsst)<-c("year", "JAN","FEB","MAR","APR",
                    "MAY","JUN","JUL","AUG",
                    "SEP","OCT","NOV","DEC")
mdrsst<-data.frame(mdrsst);mdrsst

# MDRU200
# http://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype=1&var=Zonal+Wind&level=200&lat1=20&lat2=10&lon1=-80&lon2=-20&iseas=0&mon1=0&mon2=0&iarea=0&typeout=1&Submit=Create+Timeseries
########################################################
mdru200<-data.frame(read.table("MDRU200.csv", sep=","))[-c(1:2),]
colnames(mdru200)<-c("year", "JAN","FEB","MAR","APR",
                    "MAY","JUN","JUL","AUG",
                    "SEP","OCT","NOV","DEC")
mdru200<-data.frame(mdru200);mdru200

# MDRV200
# http://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype=1&var=Meridonal+Wind&level=200&lat1=20&lat2=10&lon1=-80&lon2=-20&iseas=0&mon1=0&mon2=0&iarea=0&typeout=1&Submit=Create+Timeseries
########################################################
mdrv200<-data.frame(read.table("MDRV200.csv", sep=","))[-c(1:2),]
colnames(mdrv200)<-c("year", "JAN","FEB","MAR","APR",
                     "MAY","JUN","JUL","AUG",
                     "SEP","OCT","NOV","DEC")
mdrv200<-data.frame(mdrv200);mdrv200
 
# MDRU850
# http://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype=1&var=Zonal+Wind&level=850&lat1=20&lat2=10&lon1=-80&lon2=-20&iseas=0&mon1=0&mon2=0&iarea=0&typeout=1&Submit=Create+Timeseries
########################################################
mdru850<-data.frame(read.table("MDRU850.csv", sep=","))[-c(1:2),]
colnames(mdru850)<-c("year", "JAN","FEB","MAR","APR",
                     "MAY","JUN","JUL","AUG",
                     "SEP","OCT","NOV","DEC")
mdru850<-data.frame(mdru850);mdru850

# MDRV850
# http://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype=1&var=Meridonal+Wind&level=850&lat1=20&lat2=10&lon1=-80&lon2=-20&iseas=0&mon1=0&mon2=0&iarea=0&typeout=1&Submit=Create+Timeseries
########################################################
mdrv850<-data.frame(read.table("MDRV850.csv", sep=","))[-c(1:2),]
colnames(mdrv850)<-c("year", "JAN","FEB","MAR","APR",
                     "MAY","JUN","JUL","AUG",
                     "SEP","OCT","NOV","DEC")
mdrv850<-data.frame(mdrv850);mdrv850

## MDRVWS need to be calculated by using MDRU200, MDRV200, MDRU850, and MDRV850 through:
########################################################
mdrvws <- sqrt( (mdru200-mdru850)^2 + (mdrv200-mdrv850)^2 )
mdrvws[,1]<-mdrv850[,1]
mdrvws<-data.frame(mdrvws)

#Read the response variable (counts)
########################################################
counts<-read.table(file="counts_new.csv", header=T, sep=",")
resp<-matrix(as.numeric(as.matrix(counts)),66 ,13)
colnames(resp)<-c("year","AOTC","AOHU","AOMH","CATC",
                     "CAHU","CAMH","GMTC","GMHU",
                     "GMMH","TTC","THU","TMH")
counts<-data.frame(resp)
rm(resp)

########################################################
########################################################
########################################################
ls()

### NINO:
#NINO3
h<-"http://www.esrl.noaa.gov/psd/data/correlation/nina3.data"
########################################################
n<-70
nino3<-readhttp(h,n); rm(h,n);nino3

## NINO12
########################################################
h<-"http://www.esrl.noaa.gov/psd/data/correlation/nina1.data"
n<-70
nino12<-readhttp(h,n); rm(h,n);nino12


## NINO4
########################################################
h<-"http://www.esrl.noaa.gov/psd/data/correlation/nina4.data"
n<-70
nino4<-readhttp(h,n); rm(h,n);nino4

## NINO34
########################################################
h<-"http://www.esrl.noaa.gov/psd/data/correlation/nina34.data"
n<-70
nino34<-readhttp(h,n); rm(h,n);nino34


#CENSO
h<-"http://www.esrl.noaa.gov/psd/data/correlation/censo.data"
########################################################
n<-70
censo<-readhttp(h,n); rm(h,n);censo

#ONI
h<-"http://www.esrl.noaa.gov/psd/data/correlation/oni.data"
########################################################
n<-70
oni<-readhttp(h,n); rm(h,n);oni

#MEI
h<-"http://www.esrl.noaa.gov/psd/data/correlation/mei.data"
########################################################
mei<-scan(h,nlines = 67)
mei<-t(matrix(mei[-c(1:2)],nrow=13)[,-69])
colnames(mei)<-c("year", "JAN","FEB","MAR","APR",
                  "MAY","JUN","JUL","AUG",
                  "SEP","OCT","NOV","DEC")
rm(h);mei<-data.frame(mei);mei

#NINO4
h<-"http://www.esrl.noaa.gov/psd/data/correlation/nina4.data"
########################################################
n<-70
nino4<-readhttp(h,n); rm(h,n);nino4

#NINO34
h<-"http://www.esrl.noaa.gov/psd/data/correlation/nina34.data"
########################################################
n<-70
nino34<-readhttp(h,n); rm(h,n);nino34

#TNI
h<-"http://www.esrl.noaa.gov/psd/data/correlation/tni.data"
########################################################
n<-70
tni<-readhttp(h,n); rm(h,n);tni


### other PACIFIC:
#WP 
h<-"http://www.esrl.noaa.gov/psd/data/correlation/wp.data"
########################################################
n<-70
wp<-readhttp(h,n); rm(h,n);wp

#PDO
h<-"http://www.esrl.noaa.gov/psd/data/correlation/pdo.data"
########################################################
n<-70
pdo<-readhttp(h,n); rm(h,n);pdo

#NP
h<-"http://www.esrl.noaa.gov/psd/data/correlation/np.data"
########################################################
n<-70
np<-readhttp(h,n); rm(h,n);np


rm(readhttp)
length(ls()) ### 35 variables + counts that includes the 12 possible responses + NINO forecast
 
