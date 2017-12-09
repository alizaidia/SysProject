#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)
library(randomForest)
#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
coffee<-read.csv('Coffee.csv')
source('Metrics.R')

#==============Data Transformations==============
coffee2<-coffee
coffee2$name<-NULL
coffee2$date<-NULL
coffee2$ind_manager_spread<-as.factor(coffee2$ind_manager_spread)
coffee2$ind_other_report_spread<-as.factor(coffee2$ind_other_report_spread)
coffee2$ind_swap_spread<-as.factor(coffee2$ind_swap_spread)

#==================ADF Tests=====================
adf.test(coffee2$OI, alternative="stationary")
adf.test(coffee2$merchant_long_OI, alternative="stationary")
tmerchant_long_OI<-diff(log10(coffee2$merchant_long_OI))
adf.test(coffee2$merchant_short_OI, alternative="stationary")
tmerchant_short_OI<-diff(log10(coffee2$merchant_short_OI))
adf.test(coffee2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(coffee2$swap_long_OI))
adf.test(coffee2$swap_short_OI, alternative="stationary")
tswap_short_OI<-diff(log10(coffee2$swap_short_OI))
adf.test(coffee2$swap_spread_OI, alternative="stationary")
coffee2$swap_spread_OI<-NULL
adf.test(coffee2$manager_long_OI, alternative="stationary")
tmanager_long_OI<-diff(log10(coffee2$manager_long_OI))
adf.test(coffee2$manager_short_OI, alternative="stationary")
tmanager_short_OI<-diff(log10(coffee2$manager_short_OI))
adf.test(coffee2$manager_spread_OI, alternative="stationary")
adf.test(coffee2$otherreportable_long_OI, alternative="stationary")
adf.test(coffee2$otherreportable_short_OI, alternative="stationary")
adf.test(coffee2$otherreportable_spread_OI, alternative="stationary")
adf.test(coffee2$low_volume_traders_long, alternative="stationary")
adf.test(coffee2$low_volume_traders_short, alternative="stationary")
adf.test(coffee2$big8_long, alternative="stationary")
adf.test(coffee2$big8_short, alternative="stationary")
tbig8_short<-diff(log10(coffee2$big8_short))
adf.test(coffee2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
coffee2<-coffee2[-c(1),]
coffee2$merchant_long_OI<-tmerchant_long_OI
coffee2$merchant_short_OI<-tmerchant_short_OI
coffee2$swap_long_OI<-tswap_long_OI
coffee2$swap_short_OI<-tswap_short_OI
coffee2$manager_long_OI<-tmanager_long_OI
coffee2$manager_short_OI<-tmanager_short_OI
coffee2$big8_short<-tbig8_short
#============Generating Predictions=============
train<-coffee2[seq(105:527),]
test<-coffee2[seq(1:104),]
rf_coffee = randomForest(weekly_change~., data = train)
summary(rf_coffee)
#Obtain the OOB error rate and confusion matrix. 
print(rf_coffee)
#Determine which features were the best
importance(rf_coffee)
preds<-predict(rf_coffee,test)
act<-coffee[1:104,]$weekly_change
metrics(preds, act)