#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)
library(e1071)
#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
oil<-read.csv('Heatingoil.csv')
source('Metrics.R')

#==============Data Transformations==============
oil2<-oil
oil2$name<-NULL
oil2$date<-NULL
oil2$ind_manager_spread<-NULL
oil2$ind_other_report_spread<-NULL
oil2$ind_swap_spread<-NULL

#==================ADF Tests=====================
adf.test(oil2$OI, alternative="stationary")
tOI<-diff(log10(oil2$OI))
adf.test(oil2$merchant_long_OI, alternative="stationary")
adf.test(oil2$merchant_short_OI, alternative="stationary")
tmerchant_short_OI<-diff(log10(oil2$merchant_short_OI))
adf.test(oil2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(oil2$swap_long_OI))
adf.test(oil2$swap_short_OI, alternative="stationary")
adf.test(oil2$swap_spread_OI, alternative="stationary")
tswap_spread_OI<-diff(log10(oil2$swap_spread_OI))
adf.test(oil2$manager_long_OI, alternative="stationary")
tmanager_long_OI<-diff(log10(oil2$manager_long_OI))
adf.test(oil2$manager_short_OI, alternative="stationary")
tmanager_short_OI<-diff(log10(oil2$manager_short_OI))
adf.test(oil2$manager_spread_OI, alternative="stationary")
adf.test(oil2$otherreportable_long_OI, alternative="stationary")
totherreportable_long_OI<-diff(log10(oil2$otherreportable_long_OI))
adf.test(oil2$otherreportable_short_OI, alternative="stationary")
totherreportable_short_OI<-diff(log10(oil2$otherreportable_short_OI))
adf.test(oil2$low_volume_traders_long, alternative="stationary")
tlow_volume_traders_long<-diff(log10(oil2$low_volume_traders_long))
adf.test(oil2$low_volume_traders_short, alternative="stationary")
tlow_volume_traders_short<-diff(log10(oil2$low_volume_traders_short))
adf.test(oil2$big8_long, alternative="stationary")
adf.test(oil2$big8_short, alternative="stationary")
tbig8_short<-diff(log10(oil2$big8_short))
adf.test(oil2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
oil2<-oil2[-c(1),]
oil2$OI<-tOI
oil2$merchant_short_OI<-tmerchant_short_OI
oil2$swap_long_OI<-tswap_long_OI
oil2$swap_spread_OI<-tswap_spread_OI
oil2$manager_long_OI<-tmanager_long_OI
oil2$manager_short_OI<-tmanager_short_OI
oil2$otherreportable_short_OI<-totherreportable_short_OI
oil2$low_volume_traders_long<-tlow_volume_traders_long
oil2$low_volume_traders_short<-tlow_volume_traders_short
oil2$big8_short<-tbig8_short
#==================VAR Model=====================
train<-oil2[seq(53:204),]
oil_model<-VARselect(train,lag.max=52)
oil_model
model<-VAR(train,type="const",lag.max=1)
causality(model,cause="weekly_change")$Granger
#=========Predictions============================
preds <- vector(mode="numeric", length=26)
pred<-predict(model,test,n.ahead=26)
preds<-pred$fcst$weekly_change[,1]
act<-oil[1:26,]$weekly_change
metrics(preds,act)