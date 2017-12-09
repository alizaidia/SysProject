#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)

#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
oats<-read.csv('Oats.csv')
oats$date<-as.Date(oats$date)
source('Metrics.R')

#==============Data Transformations==============
oats2<-oats
oats2$name<-NULL
oats2$date<-NULL
oats2$ind_manager_spread<-NULL
oats2$ind_other_report_spread<-NULL
oats2$ind_swap_spread<-NULL
#==================ADF Tests=====================
adf.test(oats2$OI, alternative="stationary")
adf.test(oats2$merchant_long_OI, alternative="stationary")
adf.test(oats2$merchant_short_OI, alternative="stationary")
tmerchant_short_OI<-diff(log10(oats2$merchant_short_OI))
adf.test(oats2$swap_long_OI, alternative="stationary")
adf.test(oats2$swap_short_OI, alternative="stationary")
oats2$swap_short_OI<-NULL
adf.test(oats2$swap_spread_OI, alternative="stationary")
adf.test(oats2$manager_long_OI, alternative="stationary")
adf.test(oats2$manager_short_OI, alternative="stationary")
adf.test(oats2$manager_spread_OI, alternative="stationary")
adf.test(oats2$otherreportable_long_OI, alternative="stationary")
adf.test(oats2$otherreportable_short_OI, alternative="stationary")
adf.test(oats2$otherreportable_spread_OI, alternative="stationary")
adf.test(oats2$low_volume_traders_long, alternative="stationary")
tlow_volume_traders_long<-diff(log10(oats2$low_volume_traders_long))
adf.test(oats2$low_volume_traders_short, alternative="stationary")
adf.test(oats2$big8_long, alternative="stationary")
tbig8_long<-diff(log10(oats2$big8_long))
adf.test(oats2$big8_short, alternative="stationary")
adf.test(oats2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
oats2<-oats2[-c(1),]
oats2$merchant_short_OI<-tmerchant_short_OI
oats2$low_volume_traders_long<-tlow_volume_traders_long
oats2$big8_long<-tbig8_long
#==================VAR Model=====================
train<-oats2[seq(105:592),]
oats_model<-VARselect(train,lag.max=52)
oats_model
model<-VAR(train,type="const",lag.max=1)
causality(model,cause="weekly_change")$Granger
#=========Predictions============================
preds <- vector(mode="numeric", length=104)
pred<-predict(model,test,n.ahead=104)
preds<-pred$fcst$weekly_change[,1]
act<-oats[1:104,]$weekly_change
metrics(preds,act)