#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)

#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject/Data')
Naturalgas<-read.csv('Naturalgas.csv')
source('Metrics.R')

#==============Data Transformations==============
Naturalgas2<-Naturalgas
Naturalgas2$name<-NULL
Naturalgas2$date<-NULL
Naturalgas2$ind_manager_spread<-NULL
Naturalgas2$ind_other_report_spread<-NULL
Naturalgas2$ind_swap_spread<-NULL

#==================ADF Tests=====================
adf.test(Naturalgas2$OI, alternative="stationary")
tOI<-diff(log10(Naturalgas2$OI))
adf.test(Naturalgas2$merchant_long_OI, alternative="stationary")
tmerchant_long_OI<-diff(log10(Naturalgas2$merchant_long_OI))
adf.test(Naturalgas2$merchant_short_OI, alternative="stationary")
tmerchant_short_OI<-diff(log10(Naturalgas2$merchant_short_OI))
adf.test(Naturalgas2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(Naturalgas2$swap_long_OI))
adf.test(Naturalgas2$swap_short_OI, alternative="stationary")
tswap_short_OI<-diff(log10(Naturalgas2$swap_short_OI))
adf.test(Naturalgas2$swap_spread_OI, alternative="stationary")
tswap_spread_OI<-diff(log10(Naturalgas2$swap_spread_OI))
adf.test(Naturalgas2$manager_long_OI, alternative="stationary")
tmanager_long_OI<-diff(log10(Naturalgas2$manager_long_OI))
adf.test(Naturalgas2$manager_short_OI, alternative="stationary")
tmanager_short_OI<-diff(log10(Naturalgas2$manager_short_OI))
adf.test(Naturalgas2$manager_spread_OI, alternative="stationary")
adf.test(Naturalgas2$otherreportable_long_OI, alternative="stationary")
totherreportable_long_OI<-diff(log10(Naturalgas2$otherreportable_long_OI))
adf.test(Naturalgas2$otherreportable_short_OI, alternative="stationary")
totherreportable_short_OI<-diff(log10(Naturalgas2$otherreportable_short_OI))
adf.test(Naturalgas2$low_volume_traders_long, alternative="stationary")
tlow_volume_traders_long<-diff(log10(Naturalgas2$low_volume_traders_long))
adf.test(Naturalgas2$low_volume_traders_short, alternative="stationary")
adf.test(Naturalgas2$big8_long, alternative="stationary")
adf.test(Naturalgas2$big8_short, alternative="stationary")
tbig8_short<-diff(log10(Naturalgas2$big8_short))
adf.test(Naturalgas2$weekly_change, alternative="stationary")
#=======Transforming Variables==================
Naturalgas2<-Naturalgas2[-c(1),]
Naturalgas2$OI<-tOI
Naturalgas2$merchant_long_OI<-tmerchant_long_OI
Naturalgas2$merchant_short_OI<-tmerchant_short_OI
Naturalgas2$swap_long_OI<-tswap_long_OI
Naturalgas2$swap_short_OI<-tswap_short_OI
Naturalgas2$swap_spread_OI<-tswap_spread_OI
Naturalgas2$manager_long_OI<-tmanager_long_OI
Naturalgas2$manager_short_OI<-tmanager_short_OI
Naturalgas2$otherreportable_long_OI<-totherreportable_long_OI
Naturalgas2$big8_short<-tbig8_short
#==================Linear Model=====================
train<-Naturalgas2[seq(105:591),]
test<-Naturalgas2[seq(1:104),]
Naturalgasmodel<-lm(weekly_change~.,data=train)
summary(Naturalgasmodel)
#==================Model Adequacy===================
res<-resid(Naturalgasmodel)
qqnorm(res)
qqline(res)
#The normality assumption holds
vals<-fitted(Naturalgasmodel)
plot(vals,res)
#No pattern
#=========Predictions============================
preds <- vector(mode="numeric", length=104)
pred<-predict(Naturalgasmodel,test)
act<-Naturalgas[1:104,]$weekly_change
metrics(pred,act)