#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)

#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
lumber<-read.csv('Lumber.csv')
source('Metrics.R')

#==============Data Transformations==============
lumber2<-lumber
lumber2$name<-NULL
lumber2$date<-NULL
lumber2$ind_manager_spread<-as.factor(lumber2$ind_manager_spread)
lumber2$ind_other_report_spread<-as.factor(lumber2$ind_other_report_spread)
lumber2$ind_swap_spread<-as.factor(lumber2$ind_swap_spread)
#==================ADF Tests=====================
adf.test(lumber2$OI, alternative="stationary")
tOI<-diff(log10(lumber2$OI))
adf.test(lumber2$merchant_long_OI, alternative="stationary")
adf.test(lumber2$merchant_short_OI, alternative="stationary")
adf.test(lumber2$swap_long_OI, alternative="stationary")
adf.test(lumber2$swap_short_OI, alternative="stationary")
tswap_short_OI<-NULL
adf.test(lumber2$swap_spread_OI, alternative="stationary")
adf.test(lumber2$manager_long_OI, alternative="stationary")
tmanager_long_OI<-diff(log10(lumber2$manager_long_OI))
adf.test(lumber2$manager_short_OI, alternative="stationary")
adf.test(lumber2$manager_spread_OI, alternative="stationary")
adf.test(lumber2$otherreportable_long_OI, alternative="stationary")
totherreportable_long_OI<-diff(log10(lumber2$otherreportable_long_OI))
adf.test(lumber2$otherreportable_short_OI, alternative="stationary")
adf.test(lumber2$low_volume_traders_long, alternative="stationary")
tlow_volume_traders_long<-diff(log10(lumber2$low_volume_traders_long))
adf.test(lumber2$low_volume_traders_short, alternative="stationary")
tlow_volume_traders_short<-diff(log10(lumber2$low_volume_traders_short))
adf.test(lumber2$big8_long, alternative="stationary")
tbig8_long<-diff(log10(lumber2$big8_long))
adf.test(lumber2$big8_short, alternative="stationary")
adf.test(lumber2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
lumber2<-lumber2[-c(1),]
lumber2$OI<-tOI
lumber2$manager_long_OI<-tmanager_long_OI
lumber2$otherreportable_long_OI<-totherreportable_long_OI
lumber2$low_volume_traders_long<-tlow_volume_traders_long
lumber2$low_volume_traders_short<-tlow_volume_traders_short
lumber2$big8_long<-tbig8_long
#==================Linear Model=====================
train<-lumber2[seq(105:591),]
test<-lumber2[seq(1:104),]
lumbermodel<-lm(weekly_change~.,data=train)
summary(lumbermodel)
#==================Model Adequacy===================
res<-resid(lumbermodel)
qqnorm(res)
qqline(res)
#The normality assumption holds
vals<-fitted(lumbermodel)
plot(vals,res)
#No pattern
#=========Predictions============================
preds <- vector(mode="numeric", length=104)
pred<-predict(lumbermodel,test)
act<-lumber[1:104,]$weekly_change
metrics(pred,act)