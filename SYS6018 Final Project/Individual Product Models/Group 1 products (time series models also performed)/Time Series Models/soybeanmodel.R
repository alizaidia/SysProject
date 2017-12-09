#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)
library(e1071)
#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
soybean<-read.csv('Soybeans.csv')
source('Metrics.R')

#==============Data Transformations==============
soybean2<-soybean
soybean2$name<-NULL
soybean2$date<-NULL
soybean2$ind_manager_spread<-NULL
soybean2$ind_other_report_spread<-NULL
soybean2$ind_swap_spread<-NULL

#==================ADF Tests=====================
adf.test(soybean2$OI, alternative="stationary")
tOI<-diff(log10(soybean2$OI))
adf.test(soybean2$merchant_long_OI, alternative="stationary")
tmerchant_long_OI<-diff(log10(soybean2$merchant_long_OI))
adf.test(soybean2$merchant_short_OI, alternative="stationary")
tmerchant_short_OI<-diff(log10(soybean2$merchant_short_OI))
adf.test(soybean2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(soybean2$swap_long_OI))
adf.test(soybean2$swap_short_OI, alternative="stationary")
tswap_short_OI<-diff(log10(soybean2$swap_short_OI))
adf.test(soybean2$swap_spread_OI, alternative="stationary")
tswap_spread_OI<-diff(log10(soybean2$swap_spread_OI))
adf.test(soybean2$manager_long_OI, alternative="stationary")
tmanager_long_OI<-diff(log10(soybean2$manager_long_OI))
adf.test(soybean2$manager_short_OI, alternative="stationary")
adf.test(soybean2$manager_spread_OI, alternative="stationary")
adf.test(soybean2$otherreportable_long_OI, alternative="stationary")
adf.test(soybean2$otherreportable_short_OI, alternative="stationary")
totherreportable_short_OI<-diff(log10(soybean2$otherreportable_short_OI))
adf.test(soybean2$low_volume_traders_long, alternative="stationary")
tlow_volume_traders_long<-diff(log10(soybean2$low_volume_traders_long))
adf.test(soybean2$low_volume_traders_short, alternative="stationary")
tlow_volume_traders_short<-diff(log10(soybean2$low_volume_traders_short))
adf.test(soybean2$big8_long, alternative="stationary")
adf.test(soybean2$big8_short, alternative="stationary")
tbig8_short<-diff(log10(soybean2$big8_short))
adf.test(soybean2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
soybean2<-soybean2[-c(1),]
soybean2$OI<-tOI
soybean2$merchant_short_OI<-tmerchant_short_OI
soybean2$merchant_long_OI<-tmerchant_long_OI
soybean2$swap_long_OI<-tswap_long_OI
soybean2$swap_short_OI<-tswap_short_OI
soybean2$swap_spread_OI<-tswap_spread_OI
soybean2$manager_long_OI<-tmanager_long_OI
soybean2$otherreportable_short_OI<-totherreportable_short_OI
soybean2$low_volume_traders_long_OI<-tlow_volume_traders_long
soybean2$low_volume_traders_short_OI<-tlow_volume_traders_short
soybean2$big8_short<-tbig8_short
#==================VAR Model=====================
train<-soybean2[seq(105:396),]
soybean_model<-VARselect(train,lag.max=52)
soybean_model
model<-VAR(train,type="const",lag.max=1)
causality(model,cause="weekly_change")$Granger
#=========Predictions============================
preds <- vector(mode="numeric", length=104)
pred<-predict(model,test,n.ahead=104)
preds<-pred$fcst$weekly_change[,1]
act<-soybean[1:104,]$weekly_change
metrics(preds,act)