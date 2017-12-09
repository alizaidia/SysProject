#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)
library(randomForest)
#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
soymeal<-read.csv('Soybeanmeal.csv')
source('Metrics.R')

#==============Data Transformations==============
soymeal2<-soymeal
soymeal2$name<-NULL
soymeal2$date<-NULL
soymeal2$ind_manager_spread<-as.factor(soymeal2$ind_manager_spread)
soymeal2$ind_other_report_spread<-NULL
soymeal2$ind_swap_spread<-as.factor(soymeal2$ind_swap_spread)

#==================ADF Tests=====================
adf.test(soymeal2$OI, alternative="stationary")
tOI<-diff(log10(soymeal2$OI))
adf.test(soymeal2$merchant_long_OI, alternative="stationary")
adf.test(soymeal2$merchant_short_OI, alternative="stationary")
adf.test(soymeal2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(soymeal2$swap_long_OI))
adf.test(soymeal2$swap_short_OI, alternative="stationary")
adf.test(soymeal2$swap_spread_OI, alternative="stationary")
adf.test(soymeal2$manager_long_OI, alternative="stationary")
adf.test(soymeal2$manager_short_OI, alternative="stationary")
adf.test(soymeal2$manager_spread_OI, alternative="stationary")
adf.test(soymeal2$otherreportable_long_OI, alternative="stationary")
totherreportable_long_OI<-diff(log10(soymeal2$otherreportable_long_OI))
adf.test(soymeal2$otherreportable_short_OI, alternative="stationary")
totherreportable_short_OI<-diff(log10(soymeal2$otherreportable_short_OI))
adf.test(soymeal2$low_volume_traders_long, alternative="stationary")
adf.test(soymeal2$low_volume_traders_short, alternative="stationary")
tlow_volume_traders_short<-diff(log10(soymeal2$low_volume_traders_short))
adf.test(soymeal2$big8_long, alternative="stationary")
tbig8_long<-diff(log10(soymeal2$big8_long))
adf.test(soymeal2$big8_short, alternative="stationary")
adf.test(soymeal2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
soymeal2<-soymeal2[-c(1),]
soymeal2$OI<-tOI
soymeal2$swap_long_OI<-tswap_long_OI
soymeal2$otherreportable_long_OI<-totherreportable_long_OI
soymeal2$otherreportable_short_OI<-totherreportable_short_OI
soymeal2$big8_long<-tbig8_long
#============Generating Predictions=============
train<-soymeal2[seq(105:591),]
test<-soymeal2[seq(1:104),]
svm_soymeal <- svm(weekly_change~., train)
summary(svm_soymeal)
pred_soymeal <- predict(svm_soymeal, test)
plot(test$weekly_change,pch=16) #Plot the dataset
points(test$weekly_change, pred_soymeal, col = "blue", pch=4)
act<-soymeal[1:104,]$weekly_change
metrics(pred_soymeal, act)