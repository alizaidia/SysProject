#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)
library(e1071)
#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
oats<-read.csv('Oats.csv')
source('Metrics.R')

#==============Data Transformations==============
oats2<-oats
oats2$name<-NULL
oats2$date<-NULL
oats2$ind_manager_spread<-as.factor(oats2$ind_manager_spread)
oats2$ind_other_report_spread<-as.factor(oats2$ind_other_report_spread)
oats2$ind_swap_spread<-as.factor(oats2$ind_swap_spread)

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
#============Generating Predictions=============
train<-oats2[seq(105:591),]
test<-oats2[seq(1:104),]
svm_oats <- svm(weekly_change~., train)
summary(svm_oats)
pred_oats <- predict(svm_oats, test)
plot(test$weekly_change,pch=16) #Plot the dataset
points(test$weekly_change, pred_oats, col = "blue", pch=4)
act<-oats[1:104,]$weekly_change
metrics(pred_oats, act)