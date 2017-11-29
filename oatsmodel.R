#==============Load the packages================
library(tseries)
library(vars)
library(tidyverse)

#==============Load the data====================
setwd('D:/Fall Semester/SYS6018/sysproject')
oats<-read.csv('oats.csv')
oats$date<-as.Date(oats$date)

#==============Plot the data====================
plot(oats$date,oats$OI)
plot(oats$date,oats$merchant_short_OI)
plot(oats$date,oats$manager_long_OI)
plot(oats$date,oats$swap_long_OI)
plot(oats$date,oats$swap_short_OI)
plot(oats$date,oats$swap_spread_OI)
plot(oats$date,oats$manager_long_OI)
plot(oats$date,oats$manager_short_OI)
plot(oats$date,oats$manager_spread_OI)
plot(oats$date,oats$otherreportable_long_OI)
plot(oats$date,oats$otherreportable_short_OI)
plot(oats$date,oats$otherreportable_spread_OI)
plot(oats$date,oats$low_volume_traders_long)
plot(oats$date,oats$low_volume_traders_short)
plot(oats$date,oats$big8_long)
plot(oats$date,oats$big8_short)
plot(oats$date,oats$act_change)

#==============Data Transformations==============
oats2<-oats
oats2$name<-NULL
oats2$date<-NULL
#==================ADF Tests=====================
adf.test(oats2$OI, alternative="stationary")
adf.test(oats2$merchant_long_OI, alternative="stationary")
adf.test(oats2$merchant_short_OI, alternative="stationary")
adf.test(oats2$swap_long_OI, alternative="stationary")
tswap_long_OI<-diff(log10(oats2$swap_long_OI))
adf.test(oats2$swap_short_OI, alternative="stationary")
tswap_short_OI<-diff(log10(oats2$swap_short_OI))
adf.test(oats2$swap_spread_OI, alternative="stationary")
adf.test(oats2$manager_long_OI, alternative="stationary")
adf.test(oats2$manager_short_OI, alternative="stationary")
adf.test(oats2$manager_spread_OI, alternative="stationary")
adf.test(oats2$otherreportable_long_OI, alternative="stationary")
totherreportable_long_OI<-diff(log10(oats2$otherreportable_long_OI))
adf.test(oats2$otherreportable_short_OI, alternative="stationary")
adf.test(oats2$otherreportable_spread_OI, alternative="stationary")
adf.test(oats2$low_volume_traders_long, alternative="stationary")
tlow_volume_traders_long<-diff(log10(oats2$low_volume_traders_long))
adf.test(oats2$low_volume_traders_short, alternative="stationary")
adf.test(oats2$big8_long, alternative="stationary")
adf.test(oats2$big8_short, alternative="stationary")
adf.test(oats2$act_change, alternative="stationary")
oats2<-oats2[-c(1),]
#==================VAR Model=====================
oats_model<-VARselect(oats2,lag.max=52)
oats_model

model<-VAR(oats2,type="const",lag.max=28)
causality(model,cause="act_change")$Granger