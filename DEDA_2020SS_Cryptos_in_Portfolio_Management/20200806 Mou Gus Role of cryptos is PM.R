#covmat check 

library(zoo)
library(rugarch)
library(rmgarch)
library(dplyr)
library(colorspace)
library(xts)
library(RiskPortfolios)
library(plyr)
library(readr)
library(ggplot2)
library(GGally)
library(dplyr)
library(mlbench)
library(IDPmisc)
library(RiskPortfolios)
library(dplyr)
library(colorspace)
library(lattice)
library(caret)
library(zoo)
library(xts)
library(corrplot) 
library(gplots)
library(RColorBrewer)
library(caTools)
library(diverge)
library(ggplot2)
library(lattice)
library(caret)
library(colorspace)
library(PerformanceAnalytics)
library(corrplot)

pricetable = read.csv("Price Table 2.csv")
pricetable$Date=as.Date(pricetable$Date)

#install packages
libraries = c("plyr", "readr","ggplot2","GGally","dplyr","mlbench","IDPmisc","RiskPortfolios","colorspace")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


#load data
pricetable = read.csv("Price Table 2.csv")

boxplot(pricetable[,-1])
n = nrow(pricetable)
head(pricetable)
summary(pricetable)
str(pricetable)


#return
spr = ((pricetable[2:n, 2] - pricetable[1:(n-1), 2])/pricetable[1:(n-1), 2])
crixr = ((pricetable[2:n, 3] - pricetable[1:(n-1), 3])/pricetable[1:(n-1), 3])
tltr = ((pricetable[2:n, 4] - pricetable[1:(n-1), 4])/pricetable[1:(n-1), 4])
eemr = ((pricetable[2:n, 5] - pricetable[1:(n-1), 5])/pricetable[1:(n-1), 5])
vnqr = ((pricetable[2:n, 6] - pricetable[1:(n-1), 6])/pricetable[1:(n-1), 6])
vear = ((pricetable[2:n, 7] - pricetable[1:(n-1), 7])/pricetable[1:(n-1), 7])
bcomr = ((pricetable[2:n, 8] - pricetable[1:(n-1), 8])/pricetable[1:(n-1), 8])
eurusdr = ((pricetable[2:n, 9] - pricetable[1:(n-1), 9])/pricetable[1:(n-1), 9])
gbpusdr = ((pricetable[2:n, 10] - pricetable[1:(n-1), 10])/pricetable[1:(n-1), 10])
chfusdr = ((pricetable[2:n, 11] - pricetable[1:(n-1), 11])/pricetable[1:(n-1), 11])
cadusdr = ((pricetable[2:n, 12] - pricetable[1:(n-1), 12])/pricetable[1:(n-1), 12])
jpyusdr = ((pricetable[2:n, 13] - pricetable[1:(n-1), 13])/pricetable[1:(n-1), 13])
sekusdr = ((pricetable[2:n, 14] - pricetable[1:(n-1), 14])/pricetable[1:(n-1), 14])

#install packages
libraries = c("lattice", "caret","zoo","xts","corrplot","gplots","RColorBrewer","caTools","diverge","ggplot2",
              "lattice","colorspace","PerformanceAnalytics","corrplot")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


pricetable$Date=as.Date(pricetable$Date)
pricetable$Date = as.Date(pricetable$Date)
dates=pricetable[2:1257,1]
dates

returntable = cbind(spr,crixr,eemr,tltr,vear,bcomr,vnqr,eurusdr,gbpusdr,chfusdr,cadusdr,jpyusdr,sekusdr)

returntable=as.data.frame(returntable)

returntable=cbind(dates, returntable)

str(returntable)

returntable= as.matrix.data.frame(returntable)

#these two lines are not necessary
#prices <- data$cl
#log_returns <- diff(log(pricetable[,2:14]), lag=1)

CRIX = c(sd(returntable[1:65,3]),sd(returntable[66:128,3]),sd(returntable[129:189,3]),sd(returntable[190:253,3]),sd(returntable[254:318,3]),sd(returntable[319:381,3]),sd(returntable[382:441,3]),sd(returntable[442:505,3]),sd(returntable[506:569,3]),sd(returntable[570:631,3]),sd(returntable[632:691,3]),sd(returntable[692:755,3]),sd(returntable[756:820,3]),sd(returntable[821:883,3]),sd(returntable[884:942,3]),sd(returntable[943:1005,3]),sd(returntable[1006:1069,3]),sd(returntable[1070:1132,3]),sd(returntable[1133:1194,3]),sd(returntable[1195:1256,3]))
SP500 = c(sd(returntable[1:65,2]),sd(returntable[66:128,2]),sd(returntable[129:189,2]),sd(returntable[190:253,2]),sd(returntable[254:318,2]),sd(returntable[319:381,2]),sd(returntable[382:441,2]),sd(returntable[442:505,2]),sd(returntable[506:569,2]),sd(returntable[570:631,2]),sd(returntable[632:691,2]),sd(returntable[692:755,2]),sd(returntable[756:820,2]),sd(returntable[821:883,2]),sd(returntable[884:942,2]),sd(returntable[943:1005,2]),sd(returntable[1006:1069,2]),sd(returntable[1070:1132,2]),sd(returntable[1133:1194,2]),sd(returntable[1195:1256,2]))
EM = c(sd(returntable[1:65,4]),sd(returntable[66:128,4]),sd(returntable[129:189,4]),sd(returntable[190:253,4]),sd(returntable[254:318,4]),sd(returntable[319:381,4]),sd(returntable[382:441,4]),sd(returntable[442:505,4]),sd(returntable[506:569,4]),sd(returntable[570:631,4]),sd(returntable[632:691,4]),sd(returntable[692:755,4]),sd(returntable[756:820,4]),sd(returntable[821:883,4]),sd(returntable[884:942,4]),sd(returntable[943:1005,4]),sd(returntable[1006:1069,4]),sd(returntable[1070:1132,4]),sd(returntable[1133:1194,4]),sd(returntable[1195:1256,4]))
DM = c(sd(returntable[1:65,6]),sd(returntable[66:128,6]),sd(returntable[129:189,6]),sd(returntable[190:253,6]),sd(returntable[254:318,6]),sd(returntable[319:381,6]),sd(returntable[382:441,6]),sd(returntable[442:505,6]),sd(returntable[506:569,6]),sd(returntable[570:631,6]),sd(returntable[632:691,6]),sd(returntable[692:755,6]),sd(returntable[756:820,6]),sd(returntable[821:883,6]),sd(returntable[884:942,6]),sd(returntable[943:1005,6]),sd(returntable[1006:1069,6]),sd(returntable[1070:1132,6]),sd(returntable[1133:1194,6]),sd(returntable[1195:1256,6]))
COMMODITY = c(sd(returntable[1:65,7]),sd(returntable[66:128,7]),sd(returntable[129:189,7]),sd(returntable[190:253,7]),sd(returntable[254:318,7]),sd(returntable[319:381,7]),sd(returntable[382:441,7]),sd(returntable[442:505,7]),sd(returntable[506:569,7]),sd(returntable[570:631,7]),sd(returntable[632:691,7]),sd(returntable[692:755,7]),sd(returntable[756:820,7]),sd(returntable[821:883,7]),sd(returntable[884:942,7]),sd(returntable[943:1005,7]),sd(returntable[1006:1069,7]),sd(returntable[1070:1132,7]),sd(returntable[1133:1194,7]),sd(returntable[1195:1256,7]))


arash = cbind(CRIX,SP500,EM,DM,COMMODITY)
boxplot(arash)

returntable = cbind(spr,eemr,tltr,vear,bcomr,vnqr,crixr,eurusdr,gbpusdr,chfusdr,cadusdr,jpyusdr,sekusdr)

cor(returntable)

corrplot(corr = cor(returntable),col = diverge_hcl(12,c=100,l=c(50,90), power= 1), method = "color")
corrplot(corr = cor(returntable[1:253,]),col = diverge_hcl(12,c=100,l=c(50,90), power= 1), method = "color")
corrplot(corr = cor(returntable[254:505,]),col = diverge_hcl(12,c=100,l=c(50,90), power= 1), method = "color")
corrplot(corr = cor(returntable[506:757,]),col = diverge_hcl(12,c=100,l=c(50,90), power= 1), method = "color")
corrplot(corr = cor(returntable[758:1008,]),col = diverge_hcl(12,c=100,l=c(50,90), power= 1), method = "color")
corrplot(corr = cor(returntable[1009:1256,]),col = diverge_hcl(12,c=100,l=c(50,90), power= 1), method = "color")

#compute covariance matrix
covest=covEstimation(returntable,control = list("naive"))
covest
#compute expected returns
mest=meanEstimation(returntable,control = list("naive"))
mest
#compute semi-deviation
semdev=semidevEstimation(returntable,control = list("naive"))

#optimization of portfolios
optimalPortfolio(covest,mu=mest,semiDev = semdev,control = list("maxdiv"))

n <- nrow(pricetable)

spr = ((pricetable[2:n, 2] - pricetable[1:(n-1), 2])/pricetable[1:(n-1), 2])
crixr = ((pricetable[2:n, 3] - pricetable[1:(n-1), 3])/pricetable[1:(n-1), 3])
tltr = ((pricetable[2:n, 4] - pricetable[1:(n-1), 4])/pricetable[1:(n-1), 4])
eemr = ((pricetable[2:n, 5] - pricetable[1:(n-1), 5])/pricetable[1:(n-1), 5])
vnqr = ((pricetable[2:n, 6] - pricetable[1:(n-1), 6])/pricetable[1:(n-1), 6])
vear = ((pricetable[2:n, 7] - pricetable[1:(n-1), 7])/pricetable[1:(n-1), 7])
bcomr = ((pricetable[2:n, 8] - pricetable[1:(n-1), 8])/pricetable[1:(n-1), 8])
eurusdr = ((pricetable[2:n, 9] - pricetable[1:(n-1), 9])/pricetable[1:(n-1), 9])
gbpusdr = ((pricetable[2:n, 10] - pricetable[1:(n-1), 10])/pricetable[1:(n-1), 10])
chfusdr = ((pricetable[2:n, 11] - pricetable[1:(n-1), 11])/pricetable[1:(n-1), 11])
cadusdr = ((pricetable[2:n, 12] - pricetable[1:(n-1), 12])/pricetable[1:(n-1), 12])
jpyusdr = ((pricetable[2:n, 13] - pricetable[1:(n-1), 13])/pricetable[1:(n-1), 13])
sekusdr = ((pricetable[2:n, 14] - pricetable[1:(n-1), 14])/pricetable[1:(n-1), 14])

returntable = cbind(spr,crixr,eemr,tltr,vear,bcomr,vnqr,eurusdr,gbpusdr,chfusdr,cadusdr,jpyusdr,sekusdr)

dates=pricetable[2:1257,1]
returntable=cbind(dates, returntable)

CRIX = c(sd(returntable[1:65,3]),sd(returntable[66:128,3]),sd(returntable[129:189,3]),sd(returntable[190:253,3]),sd(returntable[254:318,3]),sd(returntable[319:381,3]),sd(returntable[382:441,3]),sd(returntable[442:505,3]),sd(returntable[506:569,3]),sd(returntable[570:631,3]),sd(returntable[632:691,3]),sd(returntable[692:755,3]),sd(returntable[756:820,3]),sd(returntable[821:883,3]),sd(returntable[884:942,3]),sd(returntable[943:1005,3]),sd(returntable[1006:1069,3]),sd(returntable[1070:1132,3]),sd(returntable[1133:1194,3]),sd(returntable[1195:1256,3]))
SP500 = c(sd(returntable[1:65,2]),sd(returntable[66:128,2]),sd(returntable[129:189,2]),sd(returntable[190:253,2]),sd(returntable[254:318,2]),sd(returntable[319:381,2]),sd(returntable[382:441,2]),sd(returntable[442:505,2]),sd(returntable[506:569,2]),sd(returntable[570:631,2]),sd(returntable[632:691,2]),sd(returntable[692:755,2]),sd(returntable[756:820,2]),sd(returntable[821:883,2]),sd(returntable[884:942,2]),sd(returntable[943:1005,2]),sd(returntable[1006:1069,2]),sd(returntable[1070:1132,2]),sd(returntable[1133:1194,2]),sd(returntable[1195:1256,2]))
EM = c(sd(returntable[1:65,4]),sd(returntable[66:128,4]),sd(returntable[129:189,4]),sd(returntable[190:253,4]),sd(returntable[254:318,4]),sd(returntable[319:381,4]),sd(returntable[382:441,4]),sd(returntable[442:505,4]),sd(returntable[506:569,4]),sd(returntable[570:631,4]),sd(returntable[632:691,4]),sd(returntable[692:755,4]),sd(returntable[756:820,4]),sd(returntable[821:883,4]),sd(returntable[884:942,4]),sd(returntable[943:1005,4]),sd(returntable[1006:1069,4]),sd(returntable[1070:1132,4]),sd(returntable[1133:1194,4]),sd(returntable[1195:1256,4]))
DM = c(sd(returntable[1:65,6]),sd(returntable[66:128,6]),sd(returntable[129:189,6]),sd(returntable[190:253,6]),sd(returntable[254:318,6]),sd(returntable[319:381,6]),sd(returntable[382:441,6]),sd(returntable[442:505,6]),sd(returntable[506:569,6]),sd(returntable[570:631,6]),sd(returntable[632:691,6]),sd(returntable[692:755,6]),sd(returntable[756:820,6]),sd(returntable[821:883,6]),sd(returntable[884:942,6]),sd(returntable[943:1005,6]),sd(returntable[1006:1069,6]),sd(returntable[1070:1132,6]),sd(returntable[1133:1194,6]),sd(returntable[1195:1256,6]))
COMMODITY = c(sd(returntable[1:65,7]),sd(returntable[66:128,7]),sd(returntable[129:189,7]),sd(returntable[190:253,7]),sd(returntable[254:318,7]),sd(returntable[319:381,7]),sd(returntable[382:441,7]),sd(returntable[442:505,7]),sd(returntable[506:569,7]),sd(returntable[570:631,7]),sd(returntable[632:691,7]),sd(returntable[692:755,7]),sd(returntable[756:820,7]),sd(returntable[821:883,7]),sd(returntable[884:942,7]),sd(returntable[943:1005,7]),sd(returntable[1006:1069,7]),sd(returntable[1070:1132,7]),sd(returntable[1133:1194,7]),sd(returntable[1195:1256,7]))


arash = cbind(CRIX,SP500,EM,DM,COMMODITY)
boxplot(arash)

returntable = cbind(spr,eemr,tltr,vear,bcomr,vnqr,crixr,eurusdr,gbpusdr,chfusdr,cadusdr,jpyusdr,sekusdr)

cor(returntable)

corrplot(corr = cor(returntable),col = diverge_hcl(12,c=100,l=c(50,90), power= 1), method = "color")
corrplot(corr = cor(returntable[1:253,]),col = diverge_hcl(12,c=100,l=c(50,90), power= 1), method = "color")
corrplot(corr = cor(returntable[254:505,]),col = diverge_hcl(12,c=100,l=c(50,90), power= 1), method = "color")
corrplot(corr = cor(returntable[506:757,]),col = diverge_hcl(12,c=100,l=c(50,90), power= 1), method = "color")
corrplot(corr = cor(returntable[758:1008,]),col = diverge_hcl(12,c=100,l=c(50,90), power= 1), method = "color")
corrplot(corr = cor(returntable[1009:1256,]),col = diverge_hcl(12,c=100,l=c(50,90), power= 1), method = "color")

returntable = as.data.frame(returntable)
Data = returntable

TLTmean <- mean(Data$tltr)
S_pmean <- mean(Data$spr)
VEAmean <- mean(Data$vear)
EEMmean <- mean(Data$eemr)
VNQmean <- mean(Data$vnqr)
CADUSDmean <- mean(Data$cadusdr)
CHFUSDmean <- mean(Data$chfusdr)
EURUSDmean <- mean(Data$eurusdr)
GBPUSDmean <- mean(Data$gbpusdr)
JPYUSDmean <- mean(Data$jpyusdr) 
SEKUSDmean <- mean(Data$sekusdr)
CRIXmean <- mean(Data$crixr)
BCOMmean <- mean(Data$bcomr)

mean.v = c(TLTmean,S_pmean,VEAmean,VNQmean,EEMmean,CADUSDmean,CHFUSDmean,EURUSDmean,GBPUSDmean,JPYUSDmean, SEKUSDmean,CRIXmean,BCOMmean)
mean.v
format(mean.v,digits = 4)


### Standard Deviations
TLTstd <- sd(Data$tltr)
S_pstd <- sd(Data$spr)
VEAstd <- sd(Data$vear)
EEMstd <- sd(Data$eemr)
VNQstd <- sd(Data$vnqr)
CADUSDstd <- sd(Data$cadusdr)
CHFUSDstd <- sd(Data$chfusdr)
EURUSDstd <- sd(Data$eurusdr)
GBPUSDstd <- sd(Data$gbpusdr)
JPYUSDstd <- sd(Data$jpyusdr) 
SEKUSDstd <- sd(Data$sekusdr)
CRIXstd <- sd(Data$crixr)
BCOMstd <- sd(Data$bcomr)

std.v = c(TLTstd,S_pstd,VEAstd,EEMstd,VNQstd,CADUSDstd,CHFUSDstd,EURUSDstd,GBPUSDstd,JPYUSDstd, SEKUSDstd,CRIXstd,BCOMstd)
std.v
format(std.v,digits = 4)

### Skewnes
library(e1071)
TLTskew <- skewness(Data$tltr, na.rm = TRUE, type = 3)
S_pskew <- skewness(Data$spr, na.rm = TRUE, type = 3)
VEAskew <- skewness(Data$vear, na.rm = TRUE, type = 3)
EEMskew <- skewness(Data$eemr, na.rm = TRUE, type = 3)
VNQskew <- skewness(Data$vnqr, na.rm = TRUE, type = 3)
CADUSDskew <- skewness(Data$cadusdr, na.rm = TRUE, type = 3)
CHFUSDskew <- skewness(Data$chfusdr, na.rm = TRUE, type = 3)
EURUSDskew <- skewness(Data$eurusdr, na.rm = TRUE, type = 3)
GBPUSDskew <- skewness(Data$gbpusdr, na.rm = TRUE, type = 3)
JPYUSDskew <- skewness(Data$jpyusdr, na.rm = TRUE, type = 3) 
SEKUSDskew <- skewness(Data$sekusdr, na.rm = TRUE, type = 3)
CRIXskew <- skewness(Data$crixr, na.rm = TRUE, type = 3)
BCOMskew <- skewness(Data$bcomr, na.rm = TRUE, type = 3)


skew.v = c(TLTskew,S_pskew,VEAskew,EEMskew,VNQskew,CADUSDskew,CHFUSDskew,EURUSDskew,GBPUSDskew,JPYUSDskew, SEKUSDskew,CRIXskew,BCOMskew)
skew.v
format(skew.v,digits = 4)

### Kurtosis
TLTkur <- kurtosis(Data$tltr, na.rm = TRUE, type = 3)
S_pkur <- kurtosis(Data$spr, na.rm = TRUE, type = 3)
VEAkur <- kurtosis(Data$vear, na.rm = TRUE, type = 3)
EEMkur <- kurtosis(Data$eemr, na.rm = TRUE, type = 3)
VNQkur <- kurtosis(Data$vnqr, na.rm = TRUE, type = 3)
CADUSDkur <- kurtosis(Data$cadusdr, na.rm = TRUE, type = 3)
CHFUSDkur <- kurtosis(Data$chfusdr, na.rm = TRUE, type = 3)
EURUSDkur <- kurtosis(Data$eurusdr, na.rm = TRUE, type = 3)
GBPUSDkur <- kurtosis(Data$gbpusdr, na.rm = TRUE, type = 3)
JPYUSDkur <- kurtosis(Data$jpyusdr, na.rm = TRUE, type = 3) 
SEKUSDkur <- kurtosis(Data$sekusdr, na.rm = TRUE, type = 3)
CRIXkur <- kurtosis(Data$crixr, na.rm = TRUE, type = 3)
BCOMkur <- kurtosis(Data$bcomr, na.rm = TRUE, type = 3)

kur.v = c(TLTkur,S_pkur,VEAkur,EEMkur,VNQkur, CADUSDkur,CHFUSDkur,EURUSDkur,GBPUSDkur,JPYUSDkur, SEKUSDkur,CRIXkur,BCOMkur)
kur.v
format(kur.v,digits = 4)

### Coefficient of Variation

TLT_CV <- TLTstd/TLTmean
S_p_CV <- S_pstd/S_pmean
VEA_CV <- VEAstd/VEAmean
EEM_CV <- EEMstd/EEMmean
VNQ_CV = VNQstd/VNQmean
CADUSD_CV <- CADUSDstd/CADUSDmean
CHFUSD_CV <- CHFUSDstd/CHFUSDmean
EURUSD_CV <- EURUSDstd/EURUSDmean
GBPUSD_CV <- GBPUSDstd/GBPUSDmean
JPYUSD_CV <- JPYUSDstd/JPYUSDmean 
SEKUSD_CV <- SEKUSDstd/SEKUSDmean
CRIX_CV <- CRIXstd/CRIXmean
BCOM_CV <- BCOMstd/BCOMmean

coe.v = c(TLT_CV,S_p_CV,VEA_CV,EEM_CV,VNQ_CV,CADUSD_CV,CHFUSD_CV,EURUSD_CV,GBPUSD_CV,JPYUSD_CV, SEKUSD_CV,CRIX_CV,BCOM_CV)
coe.v

format(coe.v,digits = 4)

table = cbind(titles,mean.v,std.v,skew.v,kur.v,coe.v)



xts_spp = xts(pricetable$S.P500, order.by = pricetable$Date)
xts_eemp = xts(pricetable$EEM, order.by = pricetable$Date)
xts_tltp = xts(pricetable$TLT, order.by = pricetable$Date)
xts_veap = xts(pricetable$VEA, order.by = pricetable$Date)
xts_vnqp = xts(pricetable$VNQ, order.by = pricetable$Date)
xts_bcomp = xts(pricetable$BCOM, order.by = pricetable$Date)
xts_crixp = xts(pricetable$CRIX, order.by = pricetable$Date)
xts_eurusdp = xts(pricetable$EURUSD, order.by = pricetable$Date)
xts_gbpusdp = xts(pricetable$GBPUSD, order.by = pricetable$Date)
xts_chfusdp = xts(pricetable$CHFUSD, order.by = pricetable$Date)
xts_cadusdp = xts(pricetable$CADUSD, order.by = pricetable$Date)
xts_jpyusdp = xts(pricetable$JPYUSD, order.by = pricetable$Date)
xts_sekusdp = xts(pricetable$SEKUSD, order.by = pricetable$Date)


ret_sp = diff(log(xts_spp))
ret_eem = diff(log(xts_eemp))
ret_tlt = diff(log(xts_tltp))
ret_vea = diff(log(xts_veap))
ret_vnq = diff(log(xts_vnqp))
ret_bcom = diff(log(xts_bcomp))
ret_crix = diff(log(xts_crixp))
ret_eurusd = diff(log(xts_eurusdp))
ret_gbpusd = diff(log(xts_gbpusdp))
ret_chfusd = diff(log(xts_chfusdp))
ret_cadusd = diff(log(xts_cadusdp))
ret_jpyusd = diff(log(xts_jpyusdp))
ret_sekusd = diff(log(xts_sekusdp))

logreturn = cbind(ret_sp,ret_crix,ret_eem,ret_tlt,ret_vea,ret_bcom,ret_vnq,ret_eurusd,ret_gbpusd,ret_chfusd,ret_cadusd,ret_jpyusd,ret_sekusd)

logreturn = as.data.frame(logreturn)


## GARCH-DCC and ADCC
uspec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")
spec1 = dccspec(uspec = multispec( replicate(13, uspec) ),  dccOrder = c(1,1),  distribution = "mvnorm")
spec1a = dccspec(uspec = multispec(replicate(13,uspec)), dccOrder = c(1, 1), model='aDCC', distribution = 'mvnorm')
fit1 = dccfit(spec1, data = logreturn[2:1257,], fit.control = list(eval.se=T))
fit_adcc = dccfit(spec1a, data = logreturn[2:1257,], fit.control = list(eval.se = TRUE))
print(fit1)           
print(fit_adcc)

#Forecasting 
dcc.focast=dccforecast(fit1, n.ahead = 1, n.roll = 0)
adcc.focast=dccforecast(fit_adcc, n.ahead = 1, n.roll = 0)
print(dcc.focast)


covmat.focast = rcov(dcc.focast)
covmat.focast

covmata.focast = rcov(adcc.focast)
covmata.focast

covmat = covmat.focast$`2020-05-29`[,,1]  ##The Covariance matrix based on DCC
covmat

covmata = covmata.focast$`2020-05-29`[,,1]   ##The Covariance matrix based on ADCC
covmata

# weights based on maximum diversification, with DCC Covariance matrix
weights_DCC <- optimalPortfolio(Sigma = covmat,
                 control = list(type = 'maxdiv', constraint = 'lo'))

# weights based on maximum diversification, with ADCC Covariance matrix
weights_ADCC <- optimalPortfolio(Sigma = covmata,
                 control = list(type = 'maxdiv', constraint = 'lo'))

returns_zoo <- as.xts(logreturn)
returns_f <- returns_zoo[complete.cases(returns_zoo)]

weights_DCC_CRIX <- c(weights_DCC)
weights_ADCC_CRIX <- c(weights_ADCC)

portfolio_daily_returns_DCC <- Return.portfolio(returns_f, weights = weights_DCC_CRIX)
portfolio_daily_returns_ADCC <- Return.portfolio(returns_f, weights = weights_ADCC_CRIX)

chart.CumReturns(portfolio_daily_returns_ADCC$portfolio.returns)
chart.CumReturns(portfolio_daily_returns_DCC$portfolio.returns)

#### Portfolio Analysis: Mean Std Sharpe Sortino MaxDrawdown DR
#### ADCC
port_mean_pa <- Return.annualized(portfolio_daily_returns_DCC$portfolio.returns, scale = 252, geometric = TRUE)
port_mean_pa

port_std_pa <- StdDev.annualized(portfolio_daily_returns_DCC$portfolio.returns, scale = 252)
port_std_pa

library(PerformanceAnalytics)

# Sharpe ratio
returns_zoo <- as.xts(logreturn)
port_sharpe_pa <- SharpeRatio.annualized(portfolio_daily_returns_DCC$portfolio.returns, Rf = 0.00003968, geometric=TRUE)
port_sharpe_pa

# Sortino ratio
port_sortino <- round(SortinoRatio(portfolio_daily_returns_DCC$portfolio.returns, MAR = 0.00003968),4)
port_sortino

# maxDrawdowm
Port_maxdr <- maxDrawdown(portfolio_daily_returns_DCC[,1], weights = NULL, geometric = TRUE, invert = TRUE)
Port_maxdr

# DR
print(DRatio(portfolio_daily_returns_DCC$portfolio.returns[,1]))

#### ADCC
port_mean_pa_ADCC <- Return.annualized(portfolio_daily_returns_ADCC$portfolio.returns, scale = 252, geometric = TRUE)
port_mean_pa_ADCC

port_std_pa_ADCC <- StdDev.annualized(portfolio_daily_returns_ADCC$portfolio.returns, scale = 252)
port_std_pa_ADCC

# Sharpe ratio
returns_zoo <- as.xts(logreturn)
port_sharpe_pa_ADCC <- SharpeRatio.annualized(portfolio_daily_returns_ADCC$portfolio.returns, Rf = 0.00003968, geometric=TRUE)
port_sharpe_pa_ADCC

# Sortino ratio
port_sortino_ADCC <- round(SortinoRatio(portfolio_daily_returns_ADCC$portfolio.returns, MAR = 0.00003968),4)
port_sortino_ADCC

# maxDrawdowm
Port_maxdr_ADCC <- maxDrawdown(portfolio_daily_returns_ADCC[,1], weights = NULL, geometric = TRUE, invert = TRUE)
Port_maxdr_ADCC

# DR
print(DRatio(portfolio_daily_returns_ADCC$portfolio.returns[,1]))
