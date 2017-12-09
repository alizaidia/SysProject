#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)

#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
cotton<-read.csv('Cotton2.csv')
source('Metrics.R')

#==============Data Transformations==============
cotton2<-cotton
cotton2$name<-NULL
cotton2$date<-NULL
cotton2$ind_manager_spread<-NULL
cotton2$ind_other_report_spread<-as.factor(cotton2$ind_other_report_spread)
cotton2$ind_swap_spread<-as.factor(cotton2$ind_swap_spread)
#==================ADF Tests=====================
adf.test(cotton2$OI, alternative="stationary")
tOI<-diff(log10(cotton2$OI))
adf.test(cotton2$merchant_long_OI, alternative="stationary")
adf.test(cotton2$merchant_short_OI, alternative="stationary")
tmerchant_short_OI<-diff(log10(cotton2$merchant_short_OI))
adf.test(cotton2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(cotton2$swap_long_OI))
adf.test(cotton2$swap_short_OI, alternative="stationary")
tswap_short_OI<-diff(log10(cotton2$swap_short_OI))
adf.test(cotton2$swap_spread_OI, alternative="stationary")
tswap_spread_OI<-diff(log10(cotton2$swap_spread_OI))
adf.test(cotton2$manager_long_OI, alternative="stationary")
adf.test(cotton2$manager_short_OI, alternative="stationary")
tmanager_short_OI<-diff(log10(cotton2$manager_short_OI))
adf.test(cotton2$manager_spread_OI, alternative="stationary")
adf.test(cotton2$otherreportable_long_OI, alternative="stationary")
totherreportable_long_OI<-diff(log10(cotton2$otherreportable_long_OI))
adf.test(cotton2$otherreportable_short_OI, alternative="stationary")
totherreportable_short_OI<-diff(log10(cotton2$otherreportable_short_OI))
adf.test(cotton2$otherreportable_spread_OI, alternative="stationary")
adf.test(cotton2$low_volume_traders_long, alternative="stationary")
adf.test(cotton2$low_volume_traders_short, alternative="stationary")
tlow_volume_traders_short<-diff(log10(cotton2$low_volume_traders_short))
adf.test(cotton2$big8_long, alternative="stationary")
adf.test(cotton2$big8_short, alternative="stationary")
tbig8_short<-diff(log10(cotton2$big8_short))
adf.test(cotton2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
cotton2<-cotton2[-c(1),]
cotton2$OI<-tOI
cotton2$merchant_short_OI<-tmerchant_short_OI
cotton2$swap_long_OI<-tswap_long_OI
cotton2$swap_short_OI<-tswap_short_OI
cotton2$swap_spread_OI<-tswap_spread_OI
cotton2$manager_short_OI<-tmanager_short_OI
cotton2$otherreportable_long_OI<-totherreportable_long_OI
cotton2$otherreportable_short_OI<-totherreportable_short_OI
cotton2$low_volume_traders_short<-tlow_volume_traders_short
cotton2$big8_short<-tbig8_short
#==================Linear Model=====================
train<-cotton2[seq(105:402),]
test<-cotton2[seq(1:104),]
cottonmodel<-lm(weekly_change~.,data=train)
summary(cottonmodel)
#==================Model Adequacy===================
res<-resid(cottonmodel)
qqnorm(res)
qqline(res)
#The normality assumption holds
vals<-fitted(cottonmodel)
plot(vals,res)
#No pattern
#=========Predictions============================
preds <- vector(mode="numeric", length=104)
pred<-predict(cottonmodel,test)
act<-cotton[1:104,]$weekly_change
metrics(pred,act)
