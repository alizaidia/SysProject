#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)
library(randomForest)
#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
corn<-read.csv('Corn.csv')
source('Metrics.R')

#==============Data Transformations==============
corn2<-corn
corn2$name<-NULL
corn2$date<-NULL
corn2$ind_manager_spread<-NULL
corn2$ind_other_report_spread<-NULL
corn2$ind_swap_spread<-as.factor(corn2$ind_swap_spread)

#==================ADF Tests=====================
adf.test(corn2$OI, alternative="stationary")
tOI<-diff(log10(corn2$OI))
adf.test(corn2$merchant_long_OI, alternative="stationary")
adf.test(corn2$merchant_short_OI, alternative="stationary")
adf.test(corn2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(corn2$swap_long_OI))
adf.test(corn2$swap_short_OI, alternative="stationary")
corn2$swap_short_OI<-NULL
adf.test(corn2$swap_spread_OI, alternative="stationary")
tswap_spread_OI<-diff(log10(corn2$swap_spread_OI))
adf.test(corn2$manager_long_OI, alternative="stationary")
adf.test(corn2$manager_short_OI, alternative="stationary")
adf.test(corn2$manager_spread_OI, alternative="stationary")
adf.test(corn2$otherreportable_long_OI, alternative="stationary")
totherreportable_long_OI<-diff(log10(corn2$otherreportable_long_OI))
adf.test(corn2$otherreportable_short_OI, alternative="stationary")
adf.test(corn2$low_volume_traders_long, alternative="stationary")
tlow_volume_traders_long<-diff(log10(corn2$low_volume_traders_long))
adf.test(corn2$low_volume_traders_short, alternative="stationary")
tlow_volume_traders_short<-diff(log10(corn2$low_volume_traders_short))
adf.test(corn2$big8_long, alternative="stationary")
adf.test(corn2$big8_short, alternative="stationary")
adf.test(corn2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
corn2<-corn2[-c(1),]
corn2$OI<-tOI
corn2$swap_long_OI<-tswap_long_OI
corn2$swap_spread_OI<-tswap_spread_OI
corn2$otherreportable_long_OI<-totherreportable_long_OI
corn2$low_volume_traders_short<-tlow_volume_traders_short
corn2$low_volume_traders_long<-tlow_volume_traders_long
#============Generating Predictions=============
train<-corn2[seq(105:591),]
test<-corn2[seq(1:104),]
rf_corn = randomForest(weekly_change~., data = train)
summary(rf_corn)
#Obtain the OOB error rate and confusion matrix. 
print(rf_corn)
#Determine which features were the best
importance(rf_corn)
preds<-predict(rf_corn,test)
act<-corn[1:104,]$weekly_change
metrics(preds, act)