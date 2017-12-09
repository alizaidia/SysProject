#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)
library(randomForest)
#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
soyoil<-read.csv('Soybeanoil.csv')
source('Metrics.R')

#==============Data Transformations==============
soyoil2<-soyoil
soyoil2$name<-NULL
soyoil2$date<-NULL
soyoil2$ind_manager_spread<-NULL
soyoil2$ind_other_report_spread<-NULL
soyoil2$ind_swap_spread<-as.factor(soyoil2$ind_swap_spread)

#==================ADF Tests=====================
adf.test(soyoil2$OI, alternative="stationary")
adf.test(soyoil2$merchant_long_OI, alternative="stationary")
adf.test(soyoil2$merchant_short_OI, alternative="stationary")
adf.test(soyoil2$swap_long_OI, alternative="stationary")
adf.test(soyoil2$swap_short_OI, alternative="stationary")
adf.test(soyoil2$swap_spread_OI, alternative="stationary")
adf.test(soyoil2$manager_long_OI, alternative="stationary")
adf.test(soyoil2$manager_short_OI, alternative="stationary")
adf.test(soyoil2$manager_spread_OI, alternative="stationary")
tmanager_spread_OI<-diff(log10(soyoil2$manager_spread_OI))
adf.test(soyoil2$otherreportable_long_OI, alternative="stationary")
totherreportable_long_OI<-diff(log10(soyoil2$otherreportable_long_OI))
adf.test(soyoil2$otherreportable_short_OI, alternative="stationary")
adf.test(soyoil2$low_volume_traders_long, alternative="stationary")
adf.test(soyoil2$low_volume_traders_short, alternative="stationary")
tlow_volume_traders_short<-diff(log10(soyoil2$low_volume_traders_short))
adf.test(soyoil2$big8_long, alternative="stationary")
adf.test(soyoil2$big8_short, alternative="stationary")
adf.test(soyoil2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
soyoil2<-soyoil2[-c(1),]
soyoil2$swap_short_OI<-NULL
soyoil2$manager_spread_OI<-tmanager_spread_OI
soyoil2$otherreportable_long_OI<-totherreportable_long_OI
soyoil2$low_volume_traders_short_OI<-tlow_volume_traders_short
#============Generating Predictions=============
train<-soyoil2[seq(105:591),]
test<-soyoil2[seq(1:104),]
rf_soyoil = randomForest(weekly_change~., data = train)
summary(rf_soyoil)
#Obtain the OOB error rate and confusion matrix. 
print(rf_soyoil)
#Determine which features were the best
importance(rf_soyoil)
preds<-predict(rf_soyoil,test)
act<-soyoil[1:104,]$weekly_change
metrics(preds, act)