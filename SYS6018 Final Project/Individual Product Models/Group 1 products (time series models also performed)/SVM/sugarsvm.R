#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)
library(e1071)
#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
sugar<-read.csv('sugar.csv')
source('Metrics.R')

#==============Data Transformations==============
sugar2<-sugar
sugar2$name<-NULL
sugar2$date<-NULL
sugar2$ind_manager_spread<-NULL
sugar2$ind_other_report_spread<-as.factor(sugar2$ind_other_report_spread)
sugar2$ind_swap_spread<-as.factor(sugar2$ind_swap_spread)

#==================ADF Tests=====================
adf.test(sugar2$OI, alternative="stationary")
tOI<-diff(log10(sugar2$OI))
adf.test(sugar2$merchant_long_OI, alternative="stationary")
adf.test(sugar2$merchant_short_OI, alternative="stationary")
tmerchant_short_OI<-diff(log10(sugar2$merchant_short_OI))
adf.test(sugar2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(sugar2$swap_long_OI))
adf.test(sugar2$swap_short_OI, alternative="stationary")
tswap_short_OI<-diff(log10(sugar2$swap_short_OI))
adf.test(sugar2$swap_spread_OI, alternative="stationary")
tswap_spread_OI<-diff(log10(sugar2$swap_spread_OI))
adf.test(sugar2$manager_long_OI, alternative="stationary")
adf.test(sugar2$manager_short_OI, alternative="stationary")
tmanager_short_OI<-diff(log10(sugar2$manager_short_OI))
adf.test(sugar2$manager_spread_OI, alternative="stationary")
adf.test(sugar2$otherreportable_long_OI, alternative="stationary")
totherreportable_long_OI<-diff(log10(sugar2$otherreportable_long_OI))
adf.test(sugar2$otherreportable_short_OI, alternative="stationary")
totherreportable_short_OI<-diff(log10(sugar2$otherreportable_short_OI))
adf.test(sugar2$low_volume_traders_long, alternative="stationary")
tlow_volume_traders_long<-diff(log10(sugar2$low_volume_traders_long))
adf.test(sugar2$low_volume_traders_short, alternative="stationary")
tlow_volume_traders_short<-diff(log10(sugar2$low_volume_traders_short))
adf.test(sugar2$big8_long, alternative="stationary")
tbig8_long<-diff(log10(sugar2$big8_long))
adf.test(sugar2$big8_short, alternative="stationary")
tbig8_short<-diff(log10(sugar2$big8_short))
adf.test(sugar2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
sugar2<-sugar2[-c(1),]
sugar2$OI<-tOI
sugar2$merchant_short_OI<-tmerchant_short_OI
sugar2$swap_long_OI<-tswap_long_OI
sugar2$swap_short_OI<-tswap_short_OI
sugar2$swap_spread_OI<-tswap_spread_OI
sugar2$manager_short_OI<-tmanager_short_OI
sugar2$otherreportable_long_OI<-totherreportable_long_OI
sugar2$otherreportable_short_OI<-totherreportable_short_OI
sugar2$low_volume_traders_long_OI<-tlow_volume_traders_long
sugar2$low_volume_traders_short_OI<-tlow_volume_traders_short
sugar2$big8_long<-tbig8_long
sugar2$big8_short<-tbig8_short
#============Generating Predictions=============
train<-sugar2[seq(105:501),]
test<-sugar2[seq(1:104),]
svm_sugar <- svm(weekly_change~., train)
summary(svm_sugar)
pred_sugar <- predict(svm_sugar, test)
plot(test$weekly_change,pch=16) #Plot the dataset
points(test$weekly_change, pred_sugar, col = "blue", pch=4)
act<-sugar[1:104,]$weekly_change
metrics(pred_sugar, act)