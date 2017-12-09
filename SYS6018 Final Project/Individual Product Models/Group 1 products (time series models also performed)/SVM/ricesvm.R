#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)
library(e1071)
#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
rice<-read.csv('Roughrice.csv')
source('Metrics.R')

#==============Data Transformations==============
rice2<-rice
rice2$name<-NULL
rice2$date<-NULL
rice2$ind_manager_spread<-as.factor(rice2$ind_manager_spread)
rice2$ind_other_report_spread<-as.factor(rice2$ind_other_report_spread)
rice2$ind_swap_spread<-as.factor(rice2$ind_swap_spread)

#==================ADF Tests=====================
adf.test(rice2$OI, alternative="stationary")
tOI<-diff(log10(rice2$OI))
adf.test(rice2$merchant_long_OI, alternative="stationary")
tmerchant_long_OI<-diff(log10(rice2$merchant_long_OI))
adf.test(rice2$merchant_short_OI, alternative="stationary")
tmerchant_short_OI<-diff(log10(rice2$merchant_short_OI))
adf.test(rice2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(rice2$swap_long_OI))
adf.test(rice2$swap_short_OI, alternative="stationary")
adf.test(rice2$swap_spread_OI, alternative="stationary")
adf.test(rice2$manager_long_OI, alternative="stationary")
adf.test(rice2$manager_short_OI, alternative="stationary")
adf.test(rice2$manager_spread_OI, alternative="stationary")
adf.test(rice2$otherreportable_long_OI, alternative="stationary")
adf.test(rice2$otherreportable_short_OI, alternative="stationary")
adf.test(rice2$low_volume_traders_long, alternative="stationary")
adf.test(rice2$low_volume_traders_short, alternative="stationary")
tlow_volume_traders_short<-diff(log10(rice2$low_volume_traders_short))
adf.test(rice2$big8_long, alternative="stationary")
adf.test(rice2$big8_short, alternative="stationary")
adf.test(rice2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
rice2<-rice2[-c(1),]
rice2$OI<-tOI
rice2$merchant_long_OI<-tmerchant_long_OI
rice2$merchant_short_OI<-tmerchant_short_OI
rice2$swap_long_OI<-tswap_long_OI
rice2$swap_spread_OI<-NULL
rice2$low_volume_traders_short_OI<-tlow_volume_traders_short
#============Generating Predictions=============
train<-rice2[seq(105:591),]
test<-rice2[seq(1:104),]
svm_rice <- svm(weekly_change~., train)
summary(svm_rice)
pred_rice <- predict(svm_rice, test)
plot(test$weekly_change,pch=16) #Plot the dataset
points(test$weekly_change, pred_rice, col = "blue", pch=4)
act<-rice[1:104,]$weekly_change
metrics(pred_rice, act)