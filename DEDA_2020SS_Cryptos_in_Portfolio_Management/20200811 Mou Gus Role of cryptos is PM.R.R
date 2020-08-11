
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

setwd("...")

pricetable = read.csv("Price Table 2.csv")
pricetable$Date=as.Date(pricetable$Date)

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
boxplot(arash, xlab = "Assets", ylab = "Standard Deviation" )

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
str(Data)

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
exlogreturn = cbind(ret_sp,ret_eem,ret_tlt,ret_vea,ret_bcom,ret_vnq,ret_eurusd,ret_gbpusd,ret_chfusd,ret_cadusd,ret_jpyusd,ret_sekusd)

logreturn = as.data.frame(logreturn)
exlogreturn = as.data.frame(exlogreturn)

## GARCH-DCC
uspec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")
spec1 = dccspec(uspec = multispec( replicate(13, uspec) ),  dccOrder = c(1,1),  distribution = "mvnorm")
exspec1 = dccspec(uspec = multispec( replicate(12, uspec) ),  dccOrder = c(1,1),  distribution = "mvnorm")
spec1a = dccspec(uspec = multispec(replicate(13,uspec)), dccOrder = c(1, 1), model='aDCC', distribution = 'mvnorm')
exspec1a = dccspec(uspec = multispec(replicate(12,uspec)), dccOrder = c(1, 1), model='aDCC', distribution = 'mvnorm')
fit1 = dccfit(spec1, data = logreturn[2:1257,], fit.control = list(eval.se=T))
exfit1 = dccfit(exspec1, data = exlogreturn[2:1257,], fit.control = list(eval.se=T))
fit_adcc = dccfit(spec1a, data = logreturn[2:1257,], fit.control = list(eval.se = TRUE))
exfit_adcc = dccfit(exspec1a, data = exlogreturn[2:1257,], fit.control = list(eval.se = TRUE))

print(fit1)
print(exfit1)
print(fit_adcc)
print(exfit_adcc)

#Forecasting 
dcc.focast=dccforecast(fit1, n.ahead = 1, n.roll = 0)
exdcc.focast=dccforecast(exfit1, n.ahead = 1, n.roll = 0)
adcc.focast=dccforecast(fit_adcc, n.ahead = 1, n.roll = 0)
exadcc.focast=dccforecast(exfit_adcc, n.ahead = 1, n.roll = 0)


covmat.focast = rcov(dcc.focast)
covmat.focast

excovmat.focast = rcov(exdcc.focast)
excovmat.focast


covmata.focast = rcov(adcc.focast)
covmata.focast

excovmata.focast = rcov(exadcc.focast)
excovmata.focast

covmat = covmat.focast$`2020-05-29`[,,1]  ##The Covariance matrix based on DCC
covmat

excovmat = excovmat.focast$`2020-05-29`[,,1]  ##The Covariance matrix based on DCC without CRIX
excovmat

covmata = covmata.focast$`2020-05-29`[,,1]   ##The Covariance matrix based on ADCC
covmata

excovmata = excovmata.focast$`2020-05-29`[,,1]   ##The Covariance matrix based on ADCC without CRIX
excovmata


# weights based on maximum diversification, with DCC Covariance matrix
dccweights = optimalPortfolio(Sigma = covmat,
                 control = list(type = 'maxdiv', constraint = 'lo'))

# weights based on maximum diversification, with DCC Covariance matrix without CRIX
exdccwights = optimalPortfolio(Sigma = excovmat,
                 control = list(type = 'maxdiv', constraint = 'lo'))

# weights based on maximum diversification, with ADCC Covariance matrix
adccweights = optimalPortfolio(Sigma = covmata,
                 control = list(type = 'maxdiv', constraint = 'lo'))

# weights based on maximum diversification, with ADCC Covariance matrix without CRIX
exadccweights = optimalPortfolio(Sigma = excovmata,
                 control = list(type = 'maxdiv', constraint = 'lo'))

dccweights
exdccwights
adccweights
exadccweights

returns_zoo <- as.xts(logreturn)
returns_f <- returns_zoo[complete.cases(returns_zoo)]
exreturns_zoo <- as.xts(exlogreturn)
exreturns_f <- exreturns_zoo[complete.cases(exreturns_zoo)]

weights_DCC_CRIX <- c(dccweights)
weights_ADCC_CRIX <- c(adccweights)
weights_DCC <- c(exdccwights)
weights_ADCC <- c(exadccweights)

port_DCC_CRIX <- Return.portfolio(returns_f, weights = weights_DCC_CRIX)
port_ADCC_CRIX <- Return.portfolio(returns_f, weights = weights_ADCC_CRIX)
port_DCC <- Return.portfolio(exreturns_f, weights = weights_DCC)
port_ADCC <- Return.portfolio(exreturns_f, weights = weights_ADCC)



rtn.obj <- merge(port_DCC_CRIX ,port_ADCC_CRIX , port_DCC, port_ADCC )
colnames(rtn.obj) = c("DDC CRIX","ADCC CRIX", "DCCnCRIX","ADCCnCRIX")





gg.charts.PerformanceSummary <- function(rtn.obj, geometric = TRUE, main = "", plot = TRUE)
{
  
  # load libraries
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(scales))
  suppressPackageStartupMessages(require(reshape))
  suppressPackageStartupMessages(require(PerformanceAnalytics))
  
  # create function to clean returns if having NAs in data
  clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
    univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)]<- na.replace
    univ.rtn.xts.obj  
  }
  
  # Create cumulative return function
  cum.rtn <- function(clean.xts.obj, g = TRUE)
  {
    x <- clean.xts.obj
    if(g == TRUE){y <- cumprod(x+1)-1} else {y <- cumsum(x)}
    y
  }
  
  # Create function to calculate drawdowns
  dd.xts <- function(clean.xts.obj, g = TRUE)
  {
    x <- clean.xts.obj
    if(g == TRUE){y <- PerformanceAnalytics:::Drawdowns(x)} else {y <- PerformanceAnalytics:::Drawdowns(x,geometric = FALSE)}
    y
  }
  
  # create a function to create a dataframe to be usable in ggplot to replicate charts.PerformanceSummary
  cps.df <- function(xts.obj,geometric)
  {
    x <- clean.rtn.xts(xts.obj)
    series.name <- colnames(xts.obj)[1]
    tmp <- cum.rtn(x,geometric)
    tmp$rtn <- x
    tmp$dd <- dd.xts(x,geometric)
    colnames(tmp) <- c("Cumulative Return","Daily Return","Drawdown") # names with space
    tmp.df <- as.data.frame(coredata(tmp))
    tmp.df$Date <- as.POSIXct(index(tmp))
    tmp.df.long <- melt(tmp.df,id.var="Date")
    tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
    tmp.df.long
  }
  
  # A conditional statement altering the plot according to the number of assets
  if(ncol(rtn.obj)==1)
  {
    # using the cps.df function
    df <- cps.df(rtn.obj,geometric)
    # adding in a title string if need be
    if(main == ""){
      title.string <- paste("Asset Performance")
    } else {
      title.string <- main
    }
    
    gg.xts <- ggplot(df, aes_string( x = "Date", y = "value", group = "variable" )) +
      facet_grid(variable ~ ., scales = "free_y", space = "fixed") +
      geom_line(data = subset(df, variable == "Cumulative Return")) +
      geom_bar(data = subset(df, variable == "Daily Return"), stat = "identity") +
      geom_line(data = subset(df, variable == "Drawdown")) +
      geom_hline(yintercept = 0, size = 0.5, colour = "black") +
      ggtitle(title.string) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      scale_x_datetime(breaks = date_breaks("6 months"), labels = date_format("%m/%Y")) +
      ylab("") +
      xlab("")
    
  } 
  else 
  {
    # a few extra bits to deal with the added rtn columns
    no.of.assets <- ncol(rtn.obj)
    asset.names <- colnames(rtn.obj)
    df <- do.call(rbind,lapply(1:no.of.assets, function(x){cps.df(rtn.obj[,x],geometric)}))
    df$asset <- ordered(df$asset, levels=asset.names)
    if(main == ""){
      title.string <- paste("Asset",asset.names[1],asset.names[2],asset.names[3],asset.names[4],"Performance")
    } else {
      title.string <- main
    }
    
    if(no.of.assets>5){legend.rows <- 5} else {legend.rows <- no.of.assets}
    
    gg.xts <- ggplot(df, aes_string(x = "Date", y = "value" )) +
      
      # panel layout
      facet_grid(variable~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, margin = 
                   , labeller = label_value) + # label_value is default
      
      # display points for Index and Drawdown, but not for Return
      geom_point(data = subset(df, variable == c("Cumulative Return","Drawdown"))
                 , aes(colour = factor(asset), shape = factor(asset)), size = 2.5, show.legend = TRUE) + 
      
      
      
      # line colours for the Index
      geom_line(data = subset(df, variable == "Cumulative Return"), aes(colour = factor(asset)), show.legend = FALSE) +
      
      # bar colours for the Return
      geom_bar(data = subset(df,variable == "Daily Return"), stat = "identity"
               , aes(fill = factor(asset), colour = factor(asset)), position = "dodge", show.legend = FALSE) +
      
      # line colours for the Drawdown
      geom_line(data = subset(df, variable == "Drawdown"), aes(colour = factor(asset)), show.legend = FALSE) +
      
      # horizontal line to indicate zero values
      geom_hline(yintercept = 0, size = 0.5, colour = "black") +
      
      # horizontal ticks
      scale_x_datetime(breaks = date_breaks("6 months"), labels = date_format("%m/%Y")) +
      
      # main y-axis title
      ylab("") +
      
      # main x-axis title
      xlab("") +
      
      # main chart title
      ggtitle(title.string)
    
    # legend 
    
    gglegend <- guide_legend(override.aes = list(size = 4))
    
    gg.xts <- gg.xts + guides(colour = gglegend, size = "none") +
      
      # gglegend <- guide_legend(override.aes = list(size = 3), direction = "horizontal") # direction overwritten by legend.box?
      # gg.xts <- gg.xts + guides(colour = gglegend, size = "none", shape = gglegend) + # Warning: "Duplicated override.aes is ignored"
      
      theme( legend.title = element_blank()
             , legend.position = c(0,1)
             , legend.justification = c(0,1)
             , legend.background = element_rect(colour = 'grey')
             , legend.key = element_rect(fill = "white", colour = "white")
             , axis.text.x = element_text(angle = 0, hjust = 1)
             , strip.background = element_rect(fill = "white")
             , panel.background = element_rect(fill = "white", colour = "white")
             
      )
    
  }
  
  assign("gg.xts", gg.xts,envir=.GlobalEnv)
  if(plot == TRUE){
    plot(gg.xts)
  } else {}
  
}

# display chart
gg.charts.PerformanceSummary(rtn.obj, geometric = TRUE)




# Investing 1$ for 5 years
chart.CumReturns(
  port_DCC_CRIX$portfolio.returns,
  wealth.index = TRUE,
  geometric = TRUE,
  legend.loc = NULL,
  colorset = (1:12),
  begin = c("first", "axis"),
  plot.engine = "default")

chart.CumReturns(
  port_ADCC_CRIX$portfolio.returns,
  wealth.index = TRUE,
  geometric = TRUE,
  legend.loc = NULL,
  colorset = (1:12),
  begin = c("first", "axis"),
  plot.engine = "default")

chart.CumReturns(
  port_DCC$portfolio.returns,
  wealth.index = TRUE,
  geometric = TRUE,
  legend.loc = NULL,
  colorset = (1:12),
  begin = c("first", "axis"),
  plot.engine = "default")

chart.CumReturns(
  port_ADCC$portfolio.returns,
  wealth.index = TRUE,
  geometric = TRUE,
  legend.loc = NULL,
  colorset = (1:12),
  begin = c("first", "axis"),
  plot.engine = "default")

#### Portfolio Analysis: Mean Std Sharpe Sortino MaxDrawdown
library(PerformanceAnalytics)

# Mean
mean_DCC_CRIX <- Return.annualized(port_DCC_CRIX$portfolio.returns, scale = 252, geometric = TRUE)
mean_ADCC_CRIX <- Return.annualized(port_ADCC_CRIX$portfolio.returns, scale = 252, geometric = TRUE)
mean_DCC <- Return.annualized(port_DCC$portfolio.returns, scale = 252, geometric = TRUE)
mean_ADCC <- Return.annualized(port_ADCC$portfolio.returns, scale = 252, geometric = TRUE)

mean_DCC_CRIX
mean_ADCC_CRIX
mean_DCC
mean_ADCC

# Standard Deviation
std_DCC_CRIX <- StdDev.annualized(port_DCC_CRIX$portfolio.returns, scale = 252)
std_ADCC_CRIX <- StdDev.annualized(port_ADCC_CRIX$portfolio.returns, scale = 252)
std_DCC <- StdDev.annualized(port_DCC$portfolio.returns, scale = 252)
std_ADCC <- StdDev.annualized(port_ADCC$portfolio.returns, scale = 252)

std_DCC_CRIX 
std_ADCC_CRIX 
std_DCC
std_ADCC 

# Coefficient of Variation
cv_DCC_CRIX <- std_DCC_CRIX/mean_DCC_CRIX
cv_ADCC_CRIX <- std_ADCC_CRIX/mean_ADCC_CRIX
cv_DCC <- std_DCC/mean_DCC
cv_ADCC <- std_ADCC/mean_ADCC

cv_DCC_CRIX
cv_ADCC_CRIX
cv_DCC
cv_ADCC

# Sharpe ratio

sharpe_DCC_CRIX <- SharpeRatio.annualized(port_DCC_CRIX$portfolio.returns, Rf = 0.00003968, geometric=TRUE)
sharpe_ADCC_CRIX <- SharpeRatio.annualized(port_ADCC_CRIX$portfolio.returns, Rf = 0.00003968, geometric=TRUE)
sharpe_DCC <- SharpeRatio.annualized(port_DCC$portfolio.returns, Rf = 0.00003968, geometric=TRUE)
sharpe_ADCC <- SharpeRatio.annualized(port_ADCC$portfolio.returns, Rf = 0.00003968, geometric=TRUE)

sharpe_DCC_CRIX 
sharpe_ADCC_CRIX 
sharpe_DCC
sharpe_ADCC 

# Sortino ratio
sortino_DCC_CRIX <- round(SortinoRatio(port_DCC_CRIX$portfolio.returns, MAR = 0.00003968),4)
sortino_ADCC_CRIX <- round(SortinoRatio(port_ADCC_CRIX$portfolio.returns, MAR = 0.00003968),4)
sortino_DCC <- round(SortinoRatio(port_DCC$portfolio.returns, MAR = 0.00003968),4)
sortino_ADCC <- round(SortinoRatio(port_ADCC$portfolio.returns, MAR = 0.00003968),4)

sortino_DCC_CRIX 
sortino_ADCC_CRIX 
sortino_DCC
sortino_ADCC 

# maxDrawdown
maxdr_DCC_CRIX <- maxDrawdown(port_DCC_CRIX[,1], weights = NULL, geometric = TRUE, invert = TRUE)
maxdr_ADCC_CRIX <- maxDrawdown(port_ADCC_CRIX[,1], weights = NULL, geometric = TRUE, invert = TRUE)
maxdr_DCC <- maxDrawdown(port_DCC[,1], weights = NULL, geometric = TRUE, invert = TRUE)
maxdr_ADCC <- maxDrawdown(port_ADCC[,1], weights = NULL, geometric = TRUE, invert = TRUE)

maxdr_DCC_CRIX
maxdr_ADCC_CRIX
maxdr_DCC
maxdr_ADCC

means <- c(mean_DCC_CRIX,mean_ADCC_CRIX,mean_DCC,mean_ADCC)
stds <- c(std_DCC_CRIX,std_ADCC_CRIX,std_DCC,std_ADCC)
cvs <- c(cv_DCC_CRIX,cv_ADCC_CRIX,cv_DCC,cv_ADCC)
sharpes <- c(sharpe_DCC_CRIX,sharpe_ADCC_CRIX,sharpe_DCC,sharpe_ADCC)
sortinos <- c(sortino_DCC_CRIX,sortino_ADCC_CRIX,sortino_DCC,sortino_ADCC)
maxdrs <- c(maxdr_DCC_CRIX,maxdr_ADCC_CRIX,maxdr_DCC,maxdr_ADCC)

analysis_table <- round(cbind(means,stds,cvs,sharpes,sortinos,maxdrs),4)
as.table(analysis_table)
names(analysis_table) <- c("DCC_CRIX","ADCC_CRIX","DCC","ADCC")

library(xtable)

xtable(analysis_table)

