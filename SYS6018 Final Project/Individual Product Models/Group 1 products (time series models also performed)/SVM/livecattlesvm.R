#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)
library(e1071)
#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
livecattle<-read.csv('Livecattle.csv')
source('Metrics.R')

#==============Data Transformations==============
livecattle2<-livecattle
livecattle2$name<-NULL
livecattle2$date<-NULL
livecattle2$ind_manager_spread<-NULL
livecattle2$ind_other_report_spread<-NULL
livecattle2$ind_swap_spread<-as.factor(livecattle2$ind_swap_spread)

#==================ADF Tests=====================
adf.test(livecattle2$OI, alternative="stationary")
tOI<-diff(log10(livecattle2$OI))
adf.test(livecattle2$merchant_long_OI, alternative="stationary")
tmerchant_long_OI<-diff(log10(livecattle2$merchant_long_OI))
adf.test(livecattle2$merchant_short_OI, alternative="stationary")
tmerchant_short_OI<-diff(log10(livecattle2$merchant_short_OI))
adf.test(livecattle2$swap_long_OI, alternative="stationary")
adf.test(livecattle2$swap_short_OI, alternative="stationary")
adf.test(livecattle2$swap_spread_OI, alternative="stationary")
adf.test(livecattle2$manager_long_OI, alternative="stationary")
tmanager_long_OI<-diff(log10(livecattle2$manager_long_OI))
adf.test(livecattle2$manager_short_OI, alternative="stationary")
tmanager_short_OI<-diff(log10(livecattle2$manager_short_OI))
adf.test(livecattle2$manager_spread_OI, alternative="stationary")
adf.test(livecattle2$otherreportable_long_OI, alternative="stationary")
adf.test(livecattle2$otherreportable_short_OI, alternative="stationary")
adf.test(livecattle2$low_volume_traders_long, alternative="stationary")
tlow_volume_traders_long<-diff(log10(livecattle2$low_volume_traders_long))
adf.test(livecattle2$low_volume_traders_short, alternative="stationary")
adf.test(livecattle2$big8_long, alternative="stationary")
adf.test(livecattle2$big8_short, alternative="stationary")
tbig8_short<-diff(log10(livecattle2$big8_short))
adf.test(livecattle2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
livecattle2<-livecattle2[-c(1),]
livecattle2$OI<-tOI
livecattle2$merchant_long_OI<-tmerchant_long_OI
livecattle2$merchant_short_OI<-tmerchant_short_OI
livecattle2$manager_long_OI<-tmanager_long_OI
livecattle2$manager_short_OI<-tmanager_short_OI
livecattle2$low_volume_traders_long<-tlow_volume_traders_long
livecattle2$big8_short<-tbig8_short
#============Generating Predictions=============
train<-livecattle2[seq(105:591),]
test<-livecattle2[seq(1:104),]
svm_livecattle <- svm(weekly_change~., train)
summary(svm_livecattle)
pred_livecattle <- predict(svm_livecattle, test)
plot(test$weekly_change,pch=16) #Plot the dataset
points(test$weekly_change, pred_livecattle, col = "blue", pch=4)
act<-livecattle[1:104,]$weekly_change
metrics(pred_livecattle, act)