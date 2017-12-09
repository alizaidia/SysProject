#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)

#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
wheat<-read.csv('WheatSRW.csv')
source('Metrics.R')

#==============Data Transformations==============
wheat2<-wheat
wheat2$name<-NULL
wheat2$date<-NULL
wheat2$ind_manager_spread<-NULL
wheat2$ind_other_report_spread<-NULL
wheat2$ind_swap_spread<-NULL
#==================ADF Tests=====================
adf.test(wheat2$OI, alternative="stationary")
tOI<-diff(log10(wheat2$OI))
adf.test(wheat2$merchant_long_OI, alternative="stationary")
tmerchant_long_OI<-diff(log10(wheat2$merchant_long_OI))
adf.test(wheat2$merchant_short_OI, alternative="stationary")
adf.test(wheat2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(wheat2$swap_long_OI))
adf.test(wheat2$swap_short_OI, alternative="stationary")
tswap_short_OI<-diff(log10(wheat2$swap_short_OI))
adf.test(wheat2$swap_spread_OI, alternative="stationary")
tswap_spread_OI<-diff(log10(wheat2$swap_spread_OI))
adf.test(wheat2$manager_long_OI, alternative="stationary")
tmanager_long_OI<-diff(log10(wheat2$manager_long_OI))
adf.test(wheat2$manager_short_OI, alternative="stationary")
adf.test(wheat2$manager_spread_OI, alternative="stationary")
tmanager_spread_OI<-diff(log10(wheat2$manager_spread_OI))
adf.test(wheat2$otherreportable_long_OI, alternative="stationary")
totherreportable_long_OI<-diff(log10(wheat2$otherreportable_long_OI))
adf.test(wheat2$otherreportable_short_OI, alternative="stationary")
adf.test(wheat2$low_volume_traders_long, alternative="stationary")
tlow_volume_traders_long<-diff(log10(wheat2$low_volume_traders_long))
adf.test(wheat2$low_volume_traders_short, alternative="stationary")
tlow_volume_traders_short<-diff(log10(wheat2$low_volume_traders_short))
adf.test(wheat2$big8_long, alternative="stationary")
tbig8_long<-diff(log10(wheat2$big8_long))
adf.test(wheat2$big8_short, alternative="stationary")
adf.test(wheat2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
wheat2<-wheat2[-c(1),]
wheat2$OI<-tOI
wheat2$merchant_long_OI<-tmerchant_long_OI
wheat2$swap_long_OI<-tswap_long_OI
wheat2$swap_short_OI<-tswap_short_OI
wheat2$swap_spread_OI<-tswap_spread_OI
wheat2$manager_long_OI<-tmanager_long_OI
wheat2$manager_spread_OI<-tmanager_spread_OI
wheat2$otherreportable_long_OI<-totherreportable_long_OI
wheat2$low_volume_traders_long<-tlow_volume_traders_long
wheat2$low_volume_traders_short<-tlow_volume_traders_short
wheat2$big8_long<-tbig8_long
#==================VAR Model=====================
train<-wheat2[seq(53:199),]
wheat_model<-VARselect(train,lag.max=52)
wheat_model
model<-VAR(train,type="const",lag.max=1)
causality(model,cause="weekly_change")$Granger
#=========Predictions============================
preds <- vector(mode="numeric", length=52)
pred<-predict(model,test,n.ahead=52)
preds<-pred$fcst$weekly_change[,1]
act<-wheat[1:52,]$weekly_change
metrics(preds,act)