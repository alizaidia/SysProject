#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)
library(randomForest)
#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
cattle<-read.csv('FeederCattle.csv')
source('Metrics.R')

#==============Data Transformations==============
cattle2<-cattle
cattle2$name<-NULL
cattle2$date<-NULL
cattle2$ind_manager_spread<-NULL
cattle2$ind_other_report_spread<-NULL
cattle2$ind_swap_spread<-NULL
#==================ADF Tests=====================
adf.test(cattle2$OI, alternative="stationary")
adf.test(cattle2$merchant_long_OI, alternative="stationary")
tmerchant_long_OI<-diff(log10(cattle2$merchant_long_OI))
adf.test(cattle2$merchant_short_OI, alternative="stationary")
adf.test(cattle2$swap_long_OI, alternative="stationary")
adf.test(cattle2$swap_short_OI, alternative="stationary")
adf.test(cattle2$swap_spread_OI, alternative="stationary")
adf.test(cattle2$manager_long_OI, alternative="stationary")
tmanager_long_OI<-diff(log10(cattle2$manager_long_OI))
adf.test(cattle2$manager_short_OI, alternative="stationary")
adf.test(cattle2$manager_spread_OI, alternative="stationary")
adf.test(cattle2$otherreportable_long_OI, alternative="stationary")
totherreportable_long_OI<-diff(log10(cattle2$otherreportable_long_OI))
adf.test(cattle2$otherreportable_short_OI, alternative="stationary")
adf.test(cattle2$otherreportable_spread_OI, alternative="stationary")
adf.test(cattle2$low_volume_traders_long, alternative="stationary")
tlow_volume_traders_long<-diff(log10(cattle2$low_volume_traders_long))
adf.test(cattle2$low_volume_traders_short, alternative="stationary")
adf.test(cattle2$big8_long, alternative="stationary")
adf.test(cattle2$big8_short, alternative="stationary")
adf.test(cattle2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
cattle2<-cattle2[-c(1),]
cattle2$merchant_long_OI<-tmerchant_long_OI
cattle2$low_volume_traders_long<-tlow_volume_traders_long
cattle2$otherreportable_long_OI<-totherreportable_long_OI
cattle2$manager_long_OI<-tmanager_long_OI
cattle2$merchant_long_OI<-tmerchant_long_OI
#==================VAR Model=====================
train<-cattle2[seq(105:591),]
cattle_model<-VARselect(train,lag.max=52)
cattle_model
model<-VAR(train,type="const",lag.max=1)
causality(model,cause="weekly_change")$Granger
#=========Predictions============================
preds <- vector(mode="numeric", length=104)
pred<-predict(model,test,n.ahead=104)
preds<-pred$fcst$weekly_change[,1]
act<-cattle[1:104,]$weekly_change
metrics(preds,act)