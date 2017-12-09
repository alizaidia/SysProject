#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)

#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
hog<-read.csv('Leanhogs.csv')
source('Metrics.R')

#==============Data Transformations==============
hog2<-hog
hog2$name<-NULL
hog2$date<-NULL
hog2$ind_manager_spread<-NULL
hog2$ind_other_report_spread<-NULL
hog2$ind_swap_spread<-as.factor(hog2$ind_swap_spread)

#==================ADF Tests=====================
adf.test(hog2$OI, alternative="stationary")
tOI<-diff(log10(hog2$OI))
adf.test(hog2$merchant_long_OI, alternative="stationary")
adf.test(hog2$merchant_short_OI, alternative="stationary")
adf.test(hog2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(hog2$swap_long_OI))
adf.test(hog2$swap_short_OI, alternative="stationary")
adf.test(hog2$swap_spread_OI, alternative="stationary")
adf.test(hog2$manager_long_OI, alternative="stationary")
adf.test(hog2$manager_short_OI, alternative="stationary")
adf.test(hog2$manager_spread_OI, alternative="stationary")
adf.test(hog2$otherreportable_long_OI, alternative="stationary")
totherreportable_long_OI<-diff(log10(hog2$otherreportable_long_OI))
adf.test(hog2$otherreportable_short_OI, alternative="stationary")
adf.test(hog2$low_volume_traders_long, alternative="stationary")
adf.test(hog2$low_volume_traders_short, alternative="stationary")
adf.test(hog2$big8_long, alternative="stationary")
tbig8_long<-diff(log10(hog2$big8_long))
adf.test(hog2$big8_short, alternative="stationary")
adf.test(hog2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
hog2<-hog2[-c(1),]
hog2$OI<-tOI
hog2$otherreportable_long_OI<-totherreportable_long_OI
hog2$swap_long_OI<-tswap_long_OI
hog2$big8_long<-tbig8_long
#============Generating Predictions=============
train<-hog2[seq(105:591),]
test<-hog2[seq(1:104),]
svm_hog <- svm(weekly_change~., train)
summary(svm_hog)
pred_hog <- predict(svm_hog, test)
plot(test$weekly_change,pch=16) #Plot the dataset
points(test$weekly_change, pred_hog, col = "blue", pch=4)
act<-hog[1:104,]$weekly_change
metrics(pred_hog, act)