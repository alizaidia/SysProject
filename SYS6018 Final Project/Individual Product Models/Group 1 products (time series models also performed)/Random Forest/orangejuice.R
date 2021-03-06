#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)
library(randomForest)
#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
Orangejuice<-read.csv('Orangejuice.csv')
source('Metrics.R')

#==============Data Transformations==============
Orangejuice2<-Orangejuice
Orangejuice2$name<-NULL
Orangejuice2$date<-NULL
Orangejuice2$ind_manager_spread<-as.factor(Orangejuice2$ind_manager_spread)
Orangejuice2$ind_other_report_spread<-as.factor(Orangejuice2$ind_other_report_spread)
Orangejuice2$ind_swap_spread<-as.factor(Orangejuice2$ind_swap_spread)

#==================ADF Tests=====================
adf.test(Orangejuice2$OI, alternative="stationary")
adf.test(Orangejuice2$merchant_long_OI, alternative="stationary")
adf.test(Orangejuice2$merchant_short_OI, alternative="stationary")
adf.test(Orangejuice2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(Orangejuice2$swap_long_OI))
adf.test(Orangejuice2$swap_short_OI, alternative="stationary")
adf.test(Orangejuice2$swap_spread_OI, alternative="stationary")
adf.test(Orangejuice2$manager_long_OI, alternative="stationary")
tmanager_long_OI<-diff(log10(Orangejuice2$manager_long_OI))
adf.test(Orangejuice2$manager_short_OI, alternative="stationary")
adf.test(Orangejuice2$manager_spread_OI, alternative="stationary")
adf.test(Orangejuice2$otherreportable_long_OI, alternative="stationary")
adf.test(Orangejuice2$otherreportable_short_OI, alternative="stationary")
adf.test(Orangejuice2$low_volume_traders_long, alternative="stationary")
adf.test(Orangejuice2$low_volume_traders_short, alternative="stationary")
tlow_volume_traders_short<-diff(log10(Orangejuice2$low_volume_traders_short))
adf.test(Orangejuice2$big8_long, alternative="stationary")
tbig8_long<-diff(log10(Orangejuice2$big8_long))
adf.test(Orangejuice2$big8_short, alternative="stationary")
adf.test(Orangejuice2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
Orangejuice2<-Orangejuice2[-c(1),]
Orangejuice2$swap_long_OI<-tswap_long_OI
Orangejuice2$swap_short_OI<-NULL
Orangejuice2$swap_spread_OI<-NULL
Orangejuice2$manager_long_OI<-tmanager_long_OI
Orangejuice2$low_volume_traders_short_OI<-tlow_volume_traders_short
Orangejuice2$big8_long<-tbig8_long
#============Generating Predictions=============
train<-Orangejuice2[seq(105:591),]
test<-Orangejuice2[seq(1:104),]
rf_Orangejuice = randomForest(weekly_change~., data = train)
summary(rf_Orangejuice)
#Obtain the OOB error rate and confusion matrix. 
print(rf_Orangejuice)
#Determine which features were the best
importance(rf_Orangejuice)
preds<-predict(rf_Orangejuice,test)
act<-Orangejuice[1:104,]$weekly_change
metrics(preds, act)