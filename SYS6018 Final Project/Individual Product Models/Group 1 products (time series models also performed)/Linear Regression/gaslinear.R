#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)

#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
gas<-read.csv('Gasoline.csv')
source('Metrics.R')

#==============Data Transformations==============
gas2<-gas
gas2$name<-NULL
gas2$date<-NULL
gas2$ind_manager_spread<-NULL
gas2$ind_other_report_spread<-NULL
gas2$ind_swap_spread<-NULL

#==================ADF Tests=====================
adf.test(gas2$OI, alternative="stationary")
tOI<-diff(log10(gas2$OI))
adf.test(gas2$merchant_long_OI, alternative="stationary")
tmerchant_long_OI<-diff(log10(gas2$merchant_long_OI))
adf.test(gas2$merchant_short_OI, alternative="stationary")
tmerchant_short_OI<-diff(log10(gas2$merchant_short_OI))
adf.test(gas2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(gas2$swap_long_OI))
adf.test(gas2$swap_short_OI, alternative="stationary")
tswap_short_OI<-diff(log10(gas2$swap_short_OI))
adf.test(gas2$swap_spread_OI, alternative="stationary")
tswap_spread_OI<-diff(log10(gas2$swap_spread_OI))
adf.test(gas2$manager_long_OI, alternative="stationary")
tmanager_long_OI<-diff(log10(gas2$manager_long_OI))
adf.test(gas2$manager_short_OI, alternative="stationary")
tmanager_short_OI<-diff(log10(gas2$manager_short_OI))
adf.test(gas2$manager_spread_OI, alternative="stationary")
tmanager_spread_OI<-diff(log10(gas2$manager_spread_OI))
adf.test(gas2$otherreportable_long_OI, alternative="stationary")
totherreportable_long_OI<-diff(log10(gas2$otherreportable_long_OI))
adf.test(gas2$otherreportable_short_OI, alternative="stationary")
totherreportable_short_OI<-diff(log10(gas2$otherreportable_short_OI))
adf.test(gas2$low_volume_traders_long, alternative="stationary")
tlow_volume_traders_long<-diff(log10(gas2$low_volume_traders_long))
adf.test(gas2$low_volume_traders_short, alternative="stationary")
adf.test(gas2$big8_long, alternative="stationary")
adf.test(gas2$big8_short, alternative="stationary")
tbig8_short<-diff(log10(gas2$big8_short))
adf.test(gas2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
gas2<-gas2[-c(1),]
gas2$OI<-tOI
gas2$merchant_long_OI<-tmerchant_long_OI
gas2$big8_short<-tbig8_short
gas2$low_volume_traders_long<-tlow_volume_traders_long
gas2$otherreportable_short_OI<-totherreportable_short_OI
gas2$otherreportable_long_OI<-totherreportable_long_OI
gas2$manager_spread_OI<-tmanager_spread_OI
gas2$manager_short_OI<-tmanager_short_OI
gas2$manager_long_OI<-tmanager_long_OI
gas2$swap_spread_OI<-tswap_spread_OI
gas2$swap_short_OI<-tswap_short_OI
gas2$swap_long_OI<-tswap_long_OI
gas2$merchant_short_OI<-tmerchant_short_OI
#==================Linear Model=====================
train<-gas2[seq(27:121),]
test<-gas2[seq(1:26),]
gasmodel<-lm(weekly_change~.,data=train)
summary(gasmodel)
#==================Model Adequacy===================
res<-resid(gasmodel)
qqnorm(res)
qqline(res)
#The normality assumption holds
vals<-fitted(gasmodel)
plot(vals,res)
#No pattern
#=========Predictions============================
preds <- vector(mode="numeric", length=26)
pred<-predict(gasmodel,test)
act<-gas[1:26,]$weekly_change
metrics(pred,act)