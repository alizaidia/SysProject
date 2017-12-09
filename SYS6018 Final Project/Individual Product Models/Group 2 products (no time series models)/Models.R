#Systems Final Project
#Ali Zaidi (saz8zj)

library(readr)
library(e1071)
library(leaps)
library(randomForest)

#Loading in data for each financial product:
CHF_USD <- read_csv('CHF_USD.csv')
eurodollar_libor <- read_csv('Eurodollar_Libor.csv')
USD_index <- read_csv('USD_index.csv')
AUD_USD <- read_csv('AUD_USD.csv')
BRL_USD <- read_csv('BRL_USD.csv')
US_5Ybond <- read_csv('US_5Y_bond.csv')
ZAR_USD <- read_csv('ZAR_USD.csv')
NASDAQ100 <- read_csv('NASDAQ100.csv')
NZD_USD <- read_csv('NZD_USD.csv')
RUB_USD <- read_csv('RUB_USD.csv')
MSCI_EAFE <- read_csv('MSCI_EAFE.csv')
MXN_USD <- read_csv('MXN_USD.csv')
RUSS2000 <- read_csv('RUSSELL2000.csv')
DJIA30 <- read_csv('DJIA30.csv')
EUR_GBP <- read_csv('EUR_GBP.csv')
MSCI_EMEA <- read_csv('MSCI_EMEA.csv')
Nikkei225 <- read_csv('Nikkei225.csv')
SP400mini <- read_csv('SP400mini.csv')
SP500_VIX <- read_csv('SP500_VIX.csv')
SP500 <- read_csv('SP500.csv')
US2YBonds <- read_csv('US_2Y_bond.csv')
US10YBonds <- read_csv('US_10Y_bond.csv')
CAD_USD <- read_csv('CAD_USD.csv')
EUR_USD <- read_csv('EUR_USD.csv')
GBP_USD <- read_csv('GBP_USD.csv')
JPY_USD <- read_csv('JPY_USD.csv')
BCOM_index <- read_csv('BCOM_index.csv')

cocoa <- read_csv('Cocoa.csv')
coffee <- read_csv('Coffee.csv')
copper <- read_csv('Copper.csv')
crude <- read_csv('Crude.csv')
gold <- read_csv('Gold.csv')
milk <- read_csv('Class3milk.csv')
palladium <- read_csv('Palladium.csv')
platinum <- read_csv('Platinum.csv')
silver <- read_csv('Silver.csv')

#Models for each financial product:

#AUD_USD
nrow(AUD_USD) #592
train_AUD_USD <- AUD_USD[1:392,]
test_AUD_USD = AUD_USD[393:nrow(AUD_USD),]

#Linear Regression
summary(AUD_USD)
AUD_USD<- subset(AUD_USD, select = -c(2) )
lmAUD_USD <- lm(weekly_change~. - date - OI, data = AUD_USD)
summary(lmAUD_USD) #Adjusted R-squared:  0.03097
error_AUD_USD <- lmAUD_USD$residuals
lm_error_AUD_USD <- sqrt(mean(error_AUD_USD^2)) # 1.840586

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_AUD_USD <- lm(weekly_change~1, data=AUD_USD)
s.full_AUD_USD <- lm(weekly_change~. - date - OI, data=AUD_USD)

## Forward selection
step(s.null_AUD_USD, scope=list(lower=s.null_AUD_USD, upper=s.full_AUD_USD), direction="forward")
#weekly_change ~ otherreportable_long + otherreportable_short + leverage_long + leverage_short + value_change_52

## Backward selection
step(s.full_AUD_USD, scope=list(lower=s.null_AUD_USD, upper=s.full_AUD_USD), direction="backward")
# weekly_change ~ leverage_long + leverage_short + swap_long + swap_short + manager_long + manager_short + manager_spread +
# otherreportable_long + otherreportable_short + low_volume_traders_long + 
# low_volume_traders_short + ind_swap_spread + value_change_52

## Stepwise selection
step(s.null_AUD_USD, scope=list(lower=s.null_AUD_USD, upper=s.full_AUD_USD), direction="both")
#weekly_change ~ otherreportable_long + otherreportable_short + leverage_long + leverage_short + value_change_52

#Final lm model
lmAUD_USD_final <- lm(weekly_change ~ otherreportable_long + otherreportable_short + leverage_long + leverage_short + value_change_52, data = AUD_USD)
summary(lmAUD_USD_final)
pred_lm_AUD_USD <- predict(lmAUD_USD_final, AUD_USD)

#Final svm
pred_svm_AUD_USD <- predict(svm_AUD_USD, AUD_USD)

#Final rf
pred_rf_AUD_USD <- predict(rf_AUD_USD, AUD_USD)

#SVM - https://www.r-bloggers.com/machine-learning-using-support-vector-machines/
quartz()
svm_AUD_USD <- svm(weekly_change~. - date - OI, AUD_USD)
summary(svm_AUD_USD)
pred_AUD_USD <- predict(svm_AUD_USD, AUD_USD)
plot(AUD_USD$weekly_change,pch=16) #Plot the dataset
points(AUD_USD$weekly_change, pred_AUD_USD, col = "blue", pch=4)

error_2_AUD_USD <- AUD_USD$weekly_change - pred_AUD_USD
svm_error_AUD_USD <- sqrt(mean(error_2_AUD_USD^2)) #1.697908

svm_tune <- tune(svm_AUD_USD, weekly_change ~ . - data - OI, data = train, ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9)))
print(svm_tune)

#RF
rf_AUD_USD = randomForest(weekly_change~. - date - OI, data = AUD_USD)
summary(rf_AUD_USD)

#Obtain the OOB error rate and confusion matrix. 
print(rf_AUD_USD)

#Determine which features were the best
importance(rf_AUD_USD)
##########################################################################################################################################################################
#BCOM_index - not running because not enough rows
nrow(BCOM_index) #67
summary(BCOM_index)
BCOM_index <- subset(BCOM_index, select = -c(2) )
lmBCOM_index <- lm(weekly_change~. - date - OI, data = BCOM_index)
summary(lmBCOM_index) #Adjusted R-squared:  -0.05975 
##########################################################################################################################################################################
#BRL_USD - not running because not enough rows
nrow(BRL_USD) #126
summary(BRL_USD)
BRL_USD<- subset(BRL_USD, select = -c(2) )
lmBRL_USD <- lm(weekly_change~. - date - OI, data = BRL_USD)
summary(lmBRL_USD) #Adjusted R-squared:  0.05375 
##########################################################################################################################################################################
#CAD_USD
nrow(CAD_USD) #592
train_CAD_USD <- CAD_USD[1:392,]
test_CAD_USD = CAD_USD[393:nrow(CAD_USD),]

summary(CAD_USD)
CAD_USD<- subset(CAD_USD, select = -c(2) )
lmCAD_USD <- lm(weekly_change~. - date - OI, data = CAD_USD)
summary(lmCAD_USD) #Adjusted R-squared:  -0.006852

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_CAD_USD <- lm(weekly_change~1, data=CAD_USD)
s.full_CAD_USD <- lm(weekly_change~. - date - OI, data=CAD_USD)

## Forward selection
step(s.null_CAD_USD, scope=list(lower=s.null_CAD_USD, upper=s.full_CAD_USD), direction="forward")
#weekly_change ~ otherreportable_long + value_change_13

## Backward selection
step(s.full_CAD_USD, scope=list(lower=s.null_CAD_USD, upper=s.full_CAD_USD), direction="backward")
# weekly_change ~ swap_long + low_volume_traders_long

## Stepwise selection
step(s.null_CAD_USD, scope=list(lower=s.null_CAD_USD, upper=s.full_CAD_USD), direction="both")
#weekly_change ~ otherreportable_long + value_change_13

#Final lm model
lmCAD_USD_final <- lm(weekly_change ~ otherreportable_long + value_change_13, data = CAD_USD)
summary(lmCAD_USD_final)
pred_lm_CAD_USD <- predict(lmCAD_USD_final, CAD_USD)

#Final svm
pred_svm_CAD_USD <- predict(svm_CAD_USD, CAD_USD)

#Final rf
pred_rf_CAD_USD <- predict(rf_CAD_USD, CAD_USD)

#SVM
svm_CAD_USD <- svm(weekly_change~. - date - OI, CAD_USD)
summary(svm_CAD_USD)

#RF
rf_CAD_USD = randomForest(weekly_change~. - date - OI, data = CAD_USD)
summary(rf_CAD_USD)

#Obtain the OOB error rate and confusion matrix. 
print(rf_CAD_USD)

#Determine which features were the best
importance(rf_CAD_USD)
##########################################################################################################################################################################
#CHF_USD
nrow(CHF_USD) #592
train_CHF_USD <- CHF_USD[1:392,]
test_CHF_USD = CHF_USD[393:nrow(CHF_USD),]

summary(CHF_USD)
(l <- sapply(CHF_USD, function(x) is.factor(x)))
(l <- sapply(CHF_USD, function(x) is.character(x)))
CHF_USD<- subset(CHF_USD, select = -c(2) )
lmCHF_USD <- lm(weekly_change~. - date - OI, data = CHF_USD)
summary(lmCHF_USD)

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_CHF_USD <- lm(weekly_change~1, data=CHF_USD)
s.full_CHF_USD <- lm(weekly_change~. - date - OI, data=CHF_USD)

## Forward selection
step(s.null_CHF_USD, scope=list(lower=s.null_CHF_USD, upper=s.full_CHF_USD), direction="forward")
# weekly_change ~ otherreportable_spread + rel_change_52 + ind_swap_spread

## Backward selection
step(s.full_CHF_USD, scope=list(lower=s.null_CHF_USD, upper=s.full_CHF_USD), direction="backward")
# weekly_change ~ otherreportable_spread + ind_swap_spread + rel_change_52

## Stepwise selection
step(s.null_CHF_USD, scope=list(lower=s.null_CHF_USD, upper=s.full_CHF_USD), direction="both")
#weekly_change ~ otherreportable_spread + rel_change_52 + ind_swap_spread

#Final lm model
lmCHF_USD_final <- lm(weekly_change ~ otherreportable_spread + rel_change_52 + ind_swap_spread, data = CHF_USD)
summary(lmCHF_USD_final)
pred_lm_CHF_USD <- predict(lmCHF_USD_final, CHF_USD)

#Final svm
pred_svm_CHF_USD <- predict(svm_CHF_USD, CHF_USD)

#Final rf
pred_rf_CHF_USD <- predict(rf_CHF_USD, CHF_USD)

#SVM
svm_CHF_USD <- svm(weekly_change~. - date - OI, CHF_USD)
summary(svm_CHF_USD)

#RF
rf_CHF_USD = randomForest(weekly_change~. - date - OI, data = CHF_USD)
summary(rf_CHF_USD)

#Obtain the OOB error rate and confusion matrix. 
print(rf_CHF_USD)

#Determine which features were the best
importance(rf_CHF_USD)
##########################################################################################################################################################################
#DJIA30
nrow(DJIA30) #592
train_DJIA30 <- DJIA30[1:392,]
test_DJIA30 = DJIA30[393:nrow(DJIA30),]

summary(DJIA30)
DJIA30<- subset(DJIA30, select = -c(2) )
lmDJIA30 <- lm(weekly_change~. - date - OI, data = DJIA30)
summary(lmDJIA30) #Adjusted R-squared:  0.00724

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_DJIA30 <- lm(weekly_change~1, data=DJIA30)
s.full_DJIA30 <- lm(weekly_change~. - date - OI, data=DJIA30)

## Forward selection
step(s.null_DJIA30, scope=list(lower=s.null_DJIA30, upper=s.full_DJIA30), direction="forward")
# weekly_change ~ swap_spread + ind_swap_spread + manager_short + value_change_52

## Backward selection
step(s.full_DJIA30, scope=list(lower=s.null_DJIA30, upper=s.full_DJIA30), direction="backward")
# weekly_change ~ swap_spread + ind_swap_spread + manager_short + value_change_52

## Stepwise selection
step(s.null_DJIA30, scope=list(lower=s.null_DJIA30, upper=s.full_DJIA30), direction="both")
#weekly_change ~ swap_spread + ind_swap_spread + manager_short + value_change_52

#SVM
svm_DJIA30 <- svm(weekly_change~. - date - OI, DJIA30)
summary(svm_DJIA30)

#RF
rf_DJIA30 = randomForest(weekly_change~. - date - OI, data = DJIA30)
summary(rf_CHF_USD)

#Obtain the OOB error rate and confusion matrix. 
print(rf_DJIA30)

#Determine which features were the best
importance(rf_DJIA30)
##########################################################################################################################################################################
#EUR_GBP - not running because not enough rows
nrow(EUR_GBP) #4
summary(EUR_GBP)
EUR_GBP<- subset(EUR_GBP, select = -c(2) )
lmEUR_GBP <- lm(weekly_change~. - date - OI, data = EUR_GBP)
summary(lmEUR_GBP) #Adjusted R-squared:  0.00724
##########################################################################################################################################################################
#EUR_USD 
nrow(EUR_USD) #592
train_EUR_USD <- EUR_USD[1:392,]
test_EUR_USD = EUR_USD[393:nrow(EUR_USD),]

summary(EUR_USD)
EUR_USD<- subset(EUR_USD, select = -c(2) )
lmEUR_USD <- lm(weekly_change~. - date - OI, data = EUR_USD)
summary(lmEUR_USD) #Adjusted R-squared:  0.02763 

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_EUR_USD  <- lm(weekly_change~1, data=EUR_USD )
s.full_EUR_USD  <- lm(weekly_change~. - date - OI, data=EUR_USD )

## Forward selection
step(s.null_EUR_USD, scope=list(lower=s.null_EUR_USD, upper=s.full_EUR_USD), direction="forward")
# weekly_change ~ rel_change_4 + ind_lev_spread + ind_manager_spread + low_volume_traders_short + big8_long + otherreportable_spread

## Backward selection
step(s.full_EUR_USD, scope=list(lower=s.null_EUR_USD, upper=s.full_EUR_USD), direction="backward")
# weekly_change ~ leverage_short + swap_short + otherreportable_short + otherreportable_spread + big8_long + ind_lev_spread + ind_manager_spread + 
# value_change_13 + value_change_52 + rel_change_13 + rel_change_52

## Stepwise selection
step(s.null_EUR_USD, scope=list(lower=s.null_EUR_USD, upper=s.full_EUR_USD), direction="both")
# weekly_change ~ rel_change_4 + ind_lev_spread + ind_manager_spread + low_volume_traders_short + big8_long + otherreportable_spread

#SVM
svm_EUR_USD <- svm(weekly_change~. - date - OI, EUR_USD)
summary(svm_EUR_USD)

#RF
rf_EUR_USD = randomForest(weekly_change~. - date - OI, data = EUR_USD)
summary(rf_EUR_USD)

#Obtain the OOB error rate and confusion matrix. 
print(rf_EUR_USD)

#Determine which features were the best
importance(rf_EUR_USD)
##########################################################################################################################################################################
#Eurodollar_libor
nrow(eurodollar_libor) #384
train_eurodollar_libor <- eurodollar_libor[1:250,]
test_eurodollar_libor = eurodollar_libor[251:nrow(eurodollar_libor),]

summary(eurodollar_libor)
eurodollar_libor<- subset(eurodollar_libor, select = -c(2) )
lmeurodollar_libor <- lm(weekly_change~. - date - OI, data = eurodollar_libor)
summary(lmeurodollar_libor) #Adjusted R-squared:  0.02885

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_eurodollar_libor  <- lm(weekly_change~1, data=eurodollar_libor )
s.full_eurodollar_libor  <- lm(weekly_change~. - date - OI, data=eurodollar_libor )

## Forward selection
step(s.null_eurodollar_libor, scope=list(lower=s.null_eurodollar_libor, upper=s.full_eurodollar_libor), direction="forward")
# weekly_change ~ value_change_4 + low_volume_traders_long + leverage_spread

## Backward selection
step(s.full_eurodollar_libor, scope=list(lower=s.null_eurodollar_libor, upper=s.full_eurodollar_libor), direction="backward")
# weekly_change ~ leverage_long + swap_long + swap_spread + manager_long + manager_spread + value_change_4

## Stepwise selection
step(s.null_eurodollar_libor, scope=list(lower=s.null_eurodollar_libor, upper=s.full_eurodollar_libor), direction="both")
# weekly_change ~ value_change_4 + low_volume_traders_long + leverage_spread

#SVM
svm_eurodollar_libor <- svm(weekly_change~. - date - OI, eurodollar_libor)
summary(svm_eurodollar_libor)

#RF
rf_eurodollar_libor = randomForest(weekly_change~. - date - OI, data = eurodollar_libor)
summary(rf_eurodollar_libor)

#Obtain the OOB error rate and confusion matrix. 
print(rf_eurodollar_libor)

#Determine which features were the best
importance(rf_eurodollar_libor)
##########################################################################################################################################################################
#GBP_USD
nrow(GBP_USD) #592
#RF
rf_EUR_USD = randomForest(weekly_change~. - date - OI, data = EUR_USD)
summary(rf_EUR_USD)

#Obtain the OOB error rate and confusion matrix. 
print(rf_EUR_USD)

#Determine which features were the best
importance(rf_EUR_USD)train_GBP_USD <- GBP_USD[1:392,]
test_GBP_USD = GBP_USD[393:nrow(GBP_USD),]
                       
summary(GBP_USD)
GBP_USD<- subset(GBP_USD, select = -c(2) )
lmGBP_USD <- lm(weekly_change~. - date - OI, data = GBP_USD)
summary(lmGBP_USD) #Adjusted R-squared:  -0.002118

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_GBP_USD  <- lm(weekly_change~1, data=GBP_USD )
s.full_GBP_USD  <- lm(weekly_change~. - date - OI, data=GBP_USD )

## Forward selection
step(s.null_GBP_USD, scope=list(lower=s.null_GBP_USD, upper=s.full_GBP_USD), direction="forward")
# weekly_change ~ value_change_4 + rel_change_4

## Backward selection
step(s.full_GBP_USD, scope=list(lower=s.null_GBP_USD, upper=s.full_GBP_USD), direction="backward")
# weekly_change ~ value_change_4 + rel_change_4

## Stepwise selection
step(s.null_GBP_USD, scope=list(lower=s.null_GBP_USD, upper=s.full_GBP_USD), direction="both")
# weekly_change ~ value_change_4 + rel_change_4

#SVM
svm_GBP_USD <- svm(weekly_change~. - date - OI, GBP_USD)
summary(svm_GBP_USD)

#RF
rf_GBP_USD = randomForest(weekly_change~. - date - OI, data = GBP_USD)
summary(rf_GBP_USD)

#Obtain the OOB error rate and confusion matrix. 
print(rf_GBP_USD)

#Determine which features were the best
importance(rf_GBP_USD)
##########################################################################################################################################################################
#JPY_USD - not running because not enough rows
nrow(JPY_USD) #167
summary(JPY_USD)
JPY_USD<- subset(JPY_USD, select = -c(2) )
lmJPY_USD <- lm(weekly_change~. - date - OI, data = JPY_USD)
summary(lmJPY_USD) #Adjusted R-squared:  -0.00947 
##########################################################################################################################################################################
#MSCI_EAFE - not running because not enough rows
nrow(MSCI_EAFE) #172
summary(MSCI_EAFE)
MSCI_EAFE<- subset(MSCI_EAFE, select = -c(2) )
lmMSCI_EAFE <- lm(weekly_change~. - date - OI, data = MSCI_EAFE)
summary(lmMSCI_EAFE) #Adjusted R-squared:  0.04909
##########################################################################################################################################################################
#MSCI_EMEA - not running because not enough rows
nrow(MSCI_EMEA) #172
summary(MSCI_EMEA)
MSCI_EMEA<- subset(MSCI_EMEA, select = -c(2) )
lmMSCI_EMEA <- lm(weekly_change~. - date - OI, data = MSCI_EMEA)
summary(lmMSCI_EMEA) #Adjusted R-squared:  0.03002
##########################################################################################################################################################################
#MXN_USD
nrow(MXN_USD) #592
train_MXN_USD <- MXN_USD[1:392,]
test_MXN_USD = MXN_USD[393:nrow(MXN_USD),]

summary(MXN_USD)
MXN_USD<- subset(MXN_USD, select = -c(2) )
lmMXN_USD <- lm(weekly_change~. - date - OI, data = MXN_USD)
summary(lmMXN_USD) #Adjusted R-squared:  -0.009381 

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_MXN_USD  <- lm(weekly_change~1, data=MXN_USD )
s.full_MXN_USD  <- lm(weekly_change~. - date - OI, data=MXN_USD )

## Forward selection
step(s.null_MXN_USD, scope=list(lower=s.null_MXN_USD, upper=s.full_MXN_USD), direction="forward")
# weekly_change ~ big8_long + value_change_13 + manager_short

## Backward selection
step(s.full_MXN_USD, scope=list(lower=s.null_MXN_USD, upper=s.full_MXN_USD), direction="backward")
# weekly_change ~ manager_short + value_change_13 + rel_change_52

## Stepwise selection
step(s.null_MXN_USD, scope=list(lower=s.null_MXN_USD, upper=s.full_MXN_USD), direction="both")
# weekly_change ~ big8_long + value_change_13 + manager_short

#SVM
svm_MXN_USD <- svm(weekly_change~. - date - OI, MXN_USD)
summary(svm_MXN_USD)

#RF
rf_MXN_USD = randomForest(weekly_change~. - date - OI, data = MXN_USD)
summary(rf_MXN_USD)

#Obtain the OOB error rate and confusion matrix. 
print(rf_MXN_USD)

#Determine which features were the best
importance(rf_MXN_USD)
##########################################################################################################################################################################
#NASDAQ100
nrow(NASDAQ100) #592
train_NASDAQ100 <- NASDAQ100[1:392,]
test_NASDAQ100 = NASDAQ100[393:nrow(NASDAQ100),]

summary(NASDAQ100)
NASDAQ100<- subset(NASDAQ100, select = -c(2) )
lmNASDAQ100 <- lm(weekly_change~. - date - OI, data = NASDAQ100)
summary(lmNASDAQ100) #Adjusted R-squared:  -0.007824

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_NASDAQ100  <- lm(weekly_change~1, data=NASDAQ100 )
s.full_NASDAQ100  <- lm(weekly_change~. - date - OI, data=NASDAQ100 )

## Forward selection
step(s.null_NASDAQ100, scope=list(lower=s.null_NASDAQ100, upper=s.full_NASDAQ100), direction="forward")
# weekly_change ~ manager_short + manager_spread

## Backward selection
step(s.full_NASDAQ100, scope=list(lower=s.null_NASDAQ100, upper=s.full_NASDAQ100), direction="backward")
# weekly_change ~ leverage_short + swap_short + manager_spread + big8_long + big8_short

## Stepwise selection
step(s.null_NASDAQ100, scope=list(lower=s.null_NASDAQ100, upper=s.full_NASDAQ100), direction="both")
# weekly_change ~ manager_short + manager_spread

#SVM
svm_NASDAQ100 <- svm(weekly_change~. - date - OI, NASDAQ100)
summary(svm_NASDAQ100)

#RF
rf_NASDAQ100 = randomForest(weekly_change~. - date - OI, data = NASDAQ100)
summary(rf_NASDAQ100)

#Obtain the OOB error rate and confusion matrix. 
print(rf_NASDAQ100)

#Determine which features were the best
importance(rf_NASDAQ100)
##########################################################################################################################################################################
#Nikkei225
nrow(Nikkei225) #448
train_Nikkei225 <- Nikkei225[1:300,]
test_Nikkei225 = Nikkei225[301:nrow(Nikkei225),]

summary(Nikkei225)
Nikkei225<- subset(Nikkei225, select = -c(2) )
lmNikkei225 <- lm(weekly_change~. - date - OI, data = Nikkei225)
summary(lmNikkei225) #Adjusted R-squared:  -0.009627

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_Nikkei225  <- lm(weekly_change~1, data=Nikkei225 )
s.full_Nikkei225  <- lm(weekly_change~. - date - OI, data=Nikkei225 )

## Forward selection
step(s.null_Nikkei225, scope=list(lower=s.null_Nikkei225, upper=s.full_Nikkei225), direction="forward")
# weekly_change ~ ind_swap_spread

## Backward selection
step(s.full_Nikkei225, scope=list(lower=s.null_Nikkei225, upper=s.full_Nikkei225), direction="backward")
# weekly_change ~ leverage_short + leverage_spread + swap_short + swap_spread + manager_short + otherreportable_short + otherreportable_spread + 
# low_volume_traders_short + ind_swap_spread + ind_manager_spread

## Stepwise selection
step(s.null_Nikkei225, scope=list(lower=s.null_Nikkei225, upper=s.full_Nikkei225), direction="both")
# weekly_change ~ ind_swap_spread

#SVM
svm_Nikkei225 <- svm(weekly_change~. - date - OI, Nikkei225)
summary(svm_Nikkei225)

#RF
rf_Nikkei225 = randomForest(weekly_change~. - date - OI, data = Nikkei225)
summary(rf_Nikkei225)

#Obtain the OOB error rate and confusion matrix. 
print(rf_Nikkei225)

#Determine which features were the best
importance(rf_Nikkei225)
##########################################################################################################################################################################
#NZD_USD
nrow(NZD_USD) #589
train_NZD_USD <- NZD_USD[1:392,]
test_NZD_USD = NZD_USD[393:nrow(NZD_USD),]

summary(NZD_USD)
NZD_USD<- subset(NZD_USD, select = -c(2) )
lmNZD_USD <- lm(weekly_change~. - date - OI, data = NZD_USD)
summary(lmNZD_USD) #Adjusted R-squared:  0.003798

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_NZD_USD  <- lm(weekly_change~1, data=NZD_USD )
s.full_NZD_USD <- lm(weekly_change~. - date - OI, data=NZD_USD )

## Forward selection
step(s.null_NZD_USD, scope=list(lower=s.null_NZD_USD, upper=s.full_NZD_USD), direction="forward")
# weekly_change ~ ind_manager_spread + big8_long

## Backward selection
step(s.full_NZD_USD, scope=list(lower=s.null_NZD_USD, upper=s.full_NZD_USD), direction="backward")
# weekly_change ~ leverage_long + swap_long + otherreportable_spread + ind_manager_spread + ind_other_report_spread + value_change_4 + 
# value_change_13 + value_change_52 + rel_change_4 + rel_change_13

## Stepwise selection
step(s.null_NZD_USD, scope=list(lower=s.null_NZD_USD, upper=s.full_NZD_USD), direction="both")
# weekly_change ~ ind_manager_spread + big8_long

#SVM
svm_NZD_USD <- svm(weekly_change~. - date - OI, NZD_USD)
summary(svm_NZD_USD)

#RF
rf_NZD_USD = randomForest(weekly_change~. - date - OI, data = NZD_USD)
summary(rf_NZD_USD)

#Obtain the OOB error rate and confusion matrix. 
print(rf_NZD_USD)

#Determine which features were the best
importance(rf_NZD_USD)
##########################################################################################################################################################################
#RUB_USD - not running because not enough rows
nrow(RUB_USD) #182
summary(RUB_USD)
RUB_USD<- subset(RUB_USD, select = -c(2) )
lmRUB_USD <- lm(weekly_change~. - date - OI, data = RUB_USD)
summary(lmRUB_USD) #Adjusted R-squared:  0.09498 
##########################################################################################################################################################################
#RUSS2000
nrow(RUSS2000) #482
train_RUSS2000 <- RUSS2000[1:300,]
test_RUSS2000 = RUSS2000[301:nrow(RUSS2000),]

summary(RUSS2000)
RUSS2000<- subset(RUSS2000, select = -c(2) )
lmRUSS2000 <- lm(weekly_change~. - date - OI, data = RUSS2000)
summary(lmRUSS2000) #Adjusted R-squared:  0.02404

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_RUSS2000 <- lm(weekly_change~1, data=RUSS2000 )
s.full_RUSS2000 <- lm(weekly_change~. - date - OI, data=RUSS2000 )

## Forward selection
step(s.null_RUSS2000, scope=list(lower=s.null_RUSS2000, upper=s.full_RUSS2000), direction="forward")
# weekly_change ~ ind_other_report_spread + value_change_4 + low_volume_traders_long + low_volume_traders_short + big8_long + leverage_spread + rel_change_52

## Backward selection
step(s.full_RUSS2000, scope=list(lower=s.null_RUSS2000, upper=s.full_RUSS2000), direction="backward")
# weekly_change ~ leverage_spread + low_volume_traders_long + low_volume_traders_short + big8_long + ind_other_report_spread + rel_change_13

## Stepwise selection
step(s.null_RUSS2000, scope=list(lower=s.null_RUSS2000, upper=s.full_RUSS2000), direction="both")
# weekly_change ~ ind_other_report_spread + value_change_4 + low_volume_traders_long + low_volume_traders_short + big8_long + leverage_spread + rel_change_52

#SVM
svm_RUSS2000 <- svm(weekly_change~. - date - OI, RUSS2000)
summary(svm_RUSS2000)

#RF
rf_RUSS2000 = randomForest(weekly_change~. - date - OI, data = RUSS2000)
summary(rf_RUSS2000)

#Obtain the OOB error rate and confusion matrix. 
print(rf_RUSS2000)

#Determine which features were the best
importance(rf_RUSS2000)
##########################################################################################################################################################################
#SP400mini
nrow(SP400mini) #592
train_SP400mini <- SP400mini[1:392,]
test_SP400mini = SP400mini[393:nrow(SP400mini),]

summary(SP400mini)
SP400mini<- subset(SP400mini, select = -c(2) )
lmSP400mini <- lm(weekly_change~. - date - OI, data = SP400mini)
summary(lmSP400mini) #Adjusted R-squared:  0.01054

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_SP400mini <- lm(weekly_change~1, data=SP400mini )
s.full_SP400mini <- lm(weekly_change~. - date - OI, data=SP400mini )

## Forward selection
step(s.null_SP400mini, scope=list(lower=s.null_SP400mini, upper=s.full_SP400mini), direction="forward")
# weekly_change ~ otherreportable_spread + manager_short

## Backward selection
step(s.full_SP400mini, scope=list(lower=s.null_SP400mini, upper=s.full_SP400mini), direction="backward")
# weekly_change ~ leverage_short + swap_long + otherreportable_spread + big8_long + value_change_52 + rel_change_13 + rel_change_52

## Stepwise selection
step(s.null_SP400mini, scope=list(lower=s.null_SP400mini, upper=s.full_SP400mini), direction="both")
# weekly_change ~ otherreportable_spread + manager_short

#SVM
svm_SP400mini <- svm(weekly_change~. - date - OI, SP400mini)
summary(svm_SP400mini)

#RF
rf_SP400mini = randomForest(weekly_change~. - date - OI, data = SP400mini)
summary(rf_SP400mini)

#Obtain the OOB error rate and confusion matrix. 
print(rf_SP400mini)

#Determine which features were the best
importance(rf_SP400mini)
##########################################################################################################################################################################
#SP500
nrow(SP500) #592
train_SP500 <- SP500[1:392,]
test_SP500 = SP500[393:nrow(SP500),]

summary(SP500)
SP500<- subset(SP500, select = -c(2) )
lmSP500 <- lm(weekly_change~. - date - OI, data = SP500)
summary(lmSP500) #Adjusted R-squared:  0.02309

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_SP500 <- lm(weekly_change~1, data=SP500 )
s.full_SP500 <- lm(weekly_change~. - date - OI, data=SP500 )

## Forward selection
step(s.null_SP500, scope=list(lower=s.null_SP500, upper=s.full_SP500), direction="forward")
# weekly_change ~ rel_change_13 + manager_short + value_change_52 + leverage_short

## Backward selection
step(s.full_SP500, scope=list(lower=s.null_SP500, upper=s.full_SP500), direction="backward")
# weekly_change ~ leverage_long + leverage_spread + swap_long + swap_short + swap_spread + manager_long + manager_spread + otherreportable_long + otherreportable_short + 
# low_volume_traders_long + low_volume_traders_short + ind_other_report_spread + value_change_52 + rel_change_13

## Stepwise selection
step(s.null_SP500, scope=list(lower=s.null_SP500, upper=s.full_SP500), direction="both")
# weekly_change ~ rel_change_13 + manager_short + value_change_52 + leverage_short

#SVM
svm_SP500 <- svm(weekly_change~. - date - OI, SP500)
summary(svm_SP500)

#RF
rf_SP500 = randomForest(weekly_change~. - date - OI, data = SP500)
summary(rf_SP500)

#Obtain the OOB error rate and confusion matrix. 
print(rf_SP500)

#Determine which features were the best
importance(rf_SP500)
##########################################################################################################################################################################
#SP500_VIX
nrow(SP500_VIX) #440
train_SP500_VIX <- SP500_VIX[1:300,]
test_SP500_VIX = SP500_VIX[301:nrow(SP500_VIX),]

summary(SP500_VIX)
SP500_VIX<- subset(SP500_VIX, select = -c(2) )
lmSP500_VIX <- lm(weekly_change~. - date - OI, data = SP500_VIX)
summary(lmSP500_VIX) #Adjusted R-squared:  0.0009815

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_SP500_VIX <- lm(weekly_change~1, data=SP500_VIX )
s.full_SP500_VIX <- lm(weekly_change~. - date - OI, data=SP500_VIX )

## Forward selection
step(s.null_SP500_VIX, scope=list(lower=s.null_SP500_VIX, upper=s.full_SP500_VIX), direction="forward")
# weekly_change ~ rel_change_13

## Backward selection
step(s.full_SP500_VIX, scope=list(lower=s.null_SP500_VIX, upper=s.full_SP500_VIX), direction="backward")
# weekly_change ~ leverage_long + leverage_short + leverage_spread + swap_long + swap_short + manager_long + manager_short + otherreportable_long + 
# otherreportable_short + low_volume_traders_long + low_volume_traders_short + big8_long + value_change_4 + rel_change_4

## Stepwise selection
step(s.null_SP500_VIX, scope=list(lower=s.null_SP500_VIX, upper=s.full_SP500_VIX), direction="both")
# weekly_change ~ rel_change_13

#SVM
svm_SP500_VIX <- svm(weekly_change~. - date - OI, SP500_VIX)
summary(svm_SP500_VIX)

#RF
rf_SP500_VIX = randomForest(weekly_change~. - date - OI, data = SP500_VIX)
summary(rf_SP500_VIX)

#Obtain the OOB error rate and confusion matrix. 
print(rf_SP500_VIX)

#Determine which features were the best
importance(rf_SP500_VIX)
##########################################################################################################################################################################
#US_5Ybond
nrow(US_5Ybond) #592
train_US_5Ybond <- US_5Ybond[1:392,]
test_US_5Ybond = US_5Ybond[393:nrow(US_5Ybond),]

summary(US_5Ybond)
US_5Ybond<- subset(US_5Ybond, select = -c(2) )
lmUS_5Ybond <- lm(weekly_change~. - date - OI, data = US_5Ybond)
summary(lmUS_5Ybond) #Adjusted R-squared:  0.0001172 

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_US_5Ybond <- lm(weekly_change~1, data=US_5Ybond )
s.full_US_5Ybond <- lm(weekly_change~. - date - OI, data=US_5Ybond )

## Forward selection
step(s.null_US_5Ybond, scope=list(lower=s.null_US_5Ybond, upper=s.full_US_5Ybond), direction="forward")
# weekly_change ~ leverage_spread

## Backward selection
step(s.full_US_5Ybond, scope=list(lower=s.null_US_5Ybond, upper=s.full_US_5Ybond), direction="backward")
# weekly_change ~ leverage_short + leverage_spread + swap_short + swap_spread + manager_short + manager_spread + otherreportable_short + 
# otherreportable_spread + low_volume_traders_short + big8_short

## Stepwise selection
step(s.null_US_5Ybond, scope=list(lower=s.null_US_5Ybond, upper=s.full_US_5Ybond), direction="both")
# weekly_change ~ leverage_spread

#SVM
svm_US_5Ybond <- svm(weekly_change~. - date - OI, US_5Ybond)
summary(svm_US_5Ybond)

#RF
rf_US_5Ybond = randomForest(weekly_change~. - date - OI, data = US_5Ybond)
summary(rf_US_5Ybond)

#Obtain the OOB error rate and confusion matrix. 
print(rf_US_5Ybond)

#Determine which features were the best
importance(rf_US_5Ybond)
##########################################################################################################################################################################
#US10YBonds
nrow(US10YBonds) #592
train_US10YBonds <- US10YBonds[1:392,]
test_US10YBonds = US10YBonds[393:nrow(US10YBonds),]

summary(US10YBonds)
US10YBonds<- subset(US10YBonds, select = -c(2) )
lmUS10YBonds <- lm(weekly_change~. - date - OI, data = US10YBonds)
summary(lmUS10YBonds) #Adjusted R-squared:  0.02344

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_US10YBonds <- lm(weekly_change~1, data=US10YBonds )
s.full_US10YBonds <- lm(weekly_change~. - date - OI, data=US10YBonds )

## Forward selection
step(s.null_US10YBonds, scope=list(lower=s.null_US10YBonds, upper=s.full_US10YBonds), direction="forward")
# weekly_change ~ leverage_spread + low_volume_traders_long + manager_short + otherreportable_spread + swap_short + otherreportable_long + value_change_13

## Backward selection
step(s.full_US10YBonds, scope=list(lower=s.null_US10YBonds, upper=s.full_US10YBonds), direction="backward")
# weekly_change ~ leverage_spread + swap_short + manager_short + manager_spread + otherreportable_long + otherreportable_spread + low_volume_traders_long + big8_long + value_change_13

## Stepwise selection
step(s.null_US10YBonds, scope=list(lower=s.null_US10YBonds, upper=s.full_US10YBonds), direction="both")
# weekly_change ~ leverage_spread + low_volume_traders_long + manager_short + otherreportable_spread + swap_short + otherreportable_long + value_change_13

#SVM
svm_US10YBonds <- svm(weekly_change~. - date - OI, US10YBonds)
summary(svm_US10YBonds)

#RF
rf_US10YBonds = randomForest(weekly_change~. - date - OI, data = US10YBonds)
summary(rf_US10YBonds)

#Obtain the OOB error rate and confusion matrix. 
print(rf_US10YBonds)

#Determine which features were the best
importance(rf_US10YBonds)
##########################################################################################################################################################################
#US2YBonds
nrow(US2YBonds) #592
train_US2YBonds <- US2YBonds[1:392,]
test_US2YBonds = US2YBonds[393:nrow(US2YBonds),]

summary(US2YBonds)
US2YBonds<- subset(US2YBonds, select = -c(2) )
lmUS2YBonds <- lm(weekly_change~. - date - OI, data = US2YBonds)
summary(lmUS2YBonds) #Adjusted R-squared:  0.009273

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_US2YBonds <- lm(weekly_change~1, data=US2YBonds )
s.full_US2YBonds <- lm(weekly_change~. - date - OI, data=US2YBonds )

## Forward selection
step(s.null_US2YBonds, scope=list(lower=s.null_US2YBonds, upper=s.full_US2YBonds), direction="forward")
# weekly_change ~ swap_long

## Backward selection
step(s.full_US2YBonds, scope=list(lower=s.null_US2YBonds, upper=s.full_US2YBonds), direction="backward")
# weekly_change ~ leverage_short + swap_short + manager_short + manager_spread + low_volume_traders_short + value_change_52

## Stepwise selection
step(s.null_US2YBonds, scope=list(lower=s.null_US2YBonds, upper=s.full_US2YBonds), direction="both")
# weekly_change ~ swap_long

#SVM
svm_US2YBonds <- svm(weekly_change~. - date - OI, US2YBonds)
summary(svm_US2YBonds)

#RF
rf_US2YBonds = randomForest(weekly_change~. - date - OI, data = US2YBonds)
summary(rf_US2YBonds)

#Obtain the OOB error rate and confusion matrix. 
print(rf_US2YBonds)

#Determine which features were the best
importance(rf_US2YBonds)
##########################################################################################################################################################################
#USD_index
nrow(USD_index) #592
train_USD_index <- USD_index[1:392,]
test_USD_index = USD_index[393:nrow(USD_index),]

summary(USD_index)
USD_index<- subset(USD_index, select = -c(2) )
lmUSD_index <- lm(weekly_change~. - date - OI, data = USD_index)
summary(lmUSD_index) #Adjusted R-squared:  0.01432

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_USD_index <- lm(weekly_change~1, data=USD_index )
s.full_USD_index <- lm(weekly_change~. - date - OI, data=USD_index )

## Forward selection
step(s.null_USD_index, scope=list(lower=s.null_USD_index, upper=s.full_USD_index), direction="forward")
# weekly_change ~ value_change_4 + value_change_52 + otherreportable_short + ind_other_report_spread + otherreportable_long + manager_spread + rel_change_52 + leverage_long + swap_long

## Backward selection
step(s.full_USD_index, scope=list(lower=s.null_USD_index, upper=s.full_USD_index), direction="backward")
# weekly_change ~ leverage_long + leverage_short + leverage_spread + swap_long + swap_short + swap_spread + manager_short + otherreportable_long + 
# otherreportable_short + low_volume_traders_short + ind_other_report_spread + value_change_4 + value_change_52 + rel_change_52

## Stepwise selection
step(s.null_USD_index, scope=list(lower=s.null_USD_index, upper=s.full_USD_index), direction="both")
# weekly_change ~ value_change_4 + value_change_52 + otherreportable_short + ind_other_report_spread + otherreportable_long + manager_spread + 
# rel_change_52 + leverage_long + swap_long

#SVM
svm_USD_index <- svm(weekly_change~. - date - OI, USD_index)
summary(svm_USD_index)

#RF
rf_USD_index = randomForest(weekly_change~. - date - OI, data = USD_index)
summary(rf_USD_index)

#Obtain the OOB error rate and confusion matrix. 
print(rf_USD_index)

#Determine which features were the best
importance(rf_USD_index)
##########################################################################################################################################################################
#ZAR_USD - not running because not enough rows
nrow(ZAR_USD) #44
summary(ZAR_USD)
ZAR_USD<- subset(ZAR_USD, select = -c(2) )
lmZAR_USD <- lm(weekly_change~. - date - OI, data = ZAR_USD)
summary(lmZAR_USD) #Adjusted R-squared:  -0.1473
##########################################################################################################################################################################
#Cocoa
nrow(cocoa) #528
train_cocoa <- cocoa[1:392,]
test_cocoa = cocoa[393:nrow(cocoa),]

summary(cocoa)
cocoa<- subset(cocoa, select = -c(2) )
lmcocoa <- lm(weekly_change~. - date - OI, data = cocoa)
summary(lmcocoa) #Adjusted R-squared:  0.01432

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_cocoa <- lm(weekly_change~1, data=cocoa )
s.full_cocoa <- lm(weekly_change~. - date - OI, data=cocoa )

## Forward selection
step(s.null_cocoa, scope=list(lower=s.null_cocoa, upper=s.full_cocoa), direction="forward")
# weekly_change ~ rel_change_13 + big8_long + swap_long_OI + swap_spread_OI

## Backward selection
step(s.full_cocoa, scope=list(lower=s.null_cocoa, upper=s.full_cocoa), direction="backward")
# weekly_change ~ merchant_short_OI + swap_long_OI + swap_spread_OI + manager_short_OI + low_volume_traders_short + value_change_13 + rel_change_52

## Stepwise selection
step(s.null_cocoa, scope=list(lower=s.null_cocoa, upper=s.full_cocoa), direction="both")
# weekly_change ~ rel_change_13 + big8_long + swap_long_OI + swap_spread_OI

#SVM
svm_cocoa <- svm(weekly_change~. - date - OI, cocoa)
summary(svm_cocoa)

#RF
rf_cocoa = randomForest(weekly_change~. - date - OI, data = cocoa)
summary(rf_cocoa)

#Obtain the OOB error rate and confusion matrix. 
print(rf_cocoa)

#Determine which features were the best
importance(rf_cocoa)
##########################################################################################################################################################################
#Coffee
nrow(coffee) #528
train_coffee <- coffee[1:392,]
test_coffee = coffee[393:nrow(coffee),]

summary(coffee)
coffee<- subset(coffee, select = -c(2) )
lmcoffee <- lm(weekly_change~. - date - OI, data = coffee)
summary(lmcoffee) #Adjusted R-squared:  0.01432

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_coffee <- lm(weekly_change~1, data=coffee )
s.full_coffee <- lm(weekly_change~. - date - OI, data=coffee )

## Forward selection
step(s.null_coffee, scope=list(lower=s.null_coffee, upper=s.full_coffee), direction="forward")
# weekly_change ~ otherreportable_short_OI + value_change_13 + swap_spread_OI + merchant_short_OI

## Backward selection
step(s.full_coffee, scope=list(lower=s.null_coffee, upper=s.full_coffee), direction="backward")
# weekly_change ~ merchant_short_OI + swap_short_OI + swap_spread_OI + manager_short_OI + manager_spread_OI + otherreportable_spread_OI + low_volume_traders_short + value_change_13

## Stepwise selection
step(s.null_coffee, scope=list(lower=s.null_coffee, upper=s.full_coffee), direction="both")
# weekly_change ~ otherreportable_short_OI + value_change_13 + swap_spread_OI + merchant_short_OI

#SVM
svm_coffee <- svm(weekly_change~. - date - OI, coffee)
summary(svm_coffee)

#RF
rf_coffee = randomForest(weekly_change~. - date - OI, data = coffee)
summary(rf_coffee)

#Obtain the OOB error rate and confusion matrix. 
print(rf_coffee)

#Determine which features were the best
importance(rf_coffee)
##########################################################################################################################################################################
#Copper
nrow(copper) #592
train_copper <- copper[1:392,]
test_copper = copper[393:nrow(copper),]

summary(copper)
copper<- subset(copper, select = -c(2) )
lmcopper <- lm(weekly_change~. - date - OI, data = copper)
summary(lmcopper) #Adjusted R-squared:  0.01432

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_copper <- lm(weekly_change~1, data=copper )
s.full_copper <- lm(weekly_change~. - date - OI, data=copper )

## Forward selection
step(s.null_copper, scope=list(lower=s.null_copper, upper=s.full_copper), direction="forward")
# weekly_change ~ value_change_13 + rel_change_52

## Backward selection
step(s.full_copper, scope=list(lower=s.null_copper, upper=s.full_copper), direction="backward")
# weekly_change ~ merchant_long_OI + merchant_short_OI + swap_long_OI + swap_short_OI + swap_spread_OI + manager_long_OI + manager_short_OI + 
# otherreportable_long_OI + otherreportable_short_OI + low_volume_traders_long + big8_long + rel_change_13 + rel_change_52

## Stepwise selection
step(s.null_copper, scope=list(lower=s.null_copper, upper=s.full_copper), direction="both")
# weekly_change ~ value_change_13 + rel_change_52

#SVM
svm_copper <- svm(weekly_change~. - date - OI, copper)
summary(svm_copper)

#RF
rf_copper = randomForest(weekly_change~. - date - OI, data = copper)
summary(rf_copper)

#Obtain the OOB error rate and confusion matrix. 
print(rf_copper)

#Determine which features were the best
importance(rf_copper)
##########################################################################################################################################################################
#Crude Oil
nrow(crude) #592
train_crude <- crude[1:392,]
test_crude = crude[393:nrow(crude),]

summary(crude)
crude<- subset(crude, select = -c(2) )
lmcrude <- lm(weekly_change~. - date - OI, data = crude)
summary(lmcrude) #Adjusted R-squared:  0.01432

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_crude <- lm(weekly_change~1, data=crude )
s.full_crude <- lm(weekly_change~. - date - OI, data=crude )

## Forward selection
step(s.null_crude, scope=list(lower=s.null_crude, upper=s.full_crude), direction="forward")
# weekly_change ~ otherreportable_spread_OI + swap_spread_OI + low_volume_traders_long + rel_change_4 + value_change_13 + manager_short_OI + value_change_4

## Backward selection
step(s.full_crude, scope=list(lower=s.null_crude, upper=s.full_crude), direction="backward")
# weekly_change ~ merchant_short_OI + swap_short_OI + swap_spread_OI + manager_spread_OI + otherreportable_spread_OI + value_change_4 + value_change_13 + rel_change_4

## Stepwise selection
step(s.null_crude, scope=list(lower=s.null_crude, upper=s.full_crude), direction="both")
# weekly_change ~ low_volume_traders_long + rel_change_4 + value_change_13 + manager_short_OI + value_change_4

#SVM
svm_crude <- svm(weekly_change~. - date - OI, crude)
summary(svm_crude)

#RF
rf_crude = randomForest(weekly_change~. - date - OI, data = crude)
summary(rf_crude)

#Obtain the OOB error rate and confusion matrix. 
print(rf_crude)

#Determine which features were the best
importance(rf_crude)
##########################################################################################################################################################################
#Gold
nrow(gold) #592
train_gold <- gold[1:392,]
test_gold = gold[393:nrow(gold),]

summary(gold)
gold<- subset(gold, select = -c(2) )
lmgold <- lm(weekly_change~. - date - OI, data = gold)
summary(lmgold) #Adjusted R-squared:  0.01432

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_gold <- lm(weekly_change~1, data=gold )
s.full_gold <- lm(weekly_change~. - date - OI, data=gold )

## Forward selection
step(s.null_gold, scope=list(lower=s.null_gold, upper=s.full_gold), direction="forward")
# weekly_change ~ swap_spread_OI + swap_long_OI + rel_change_4 + manager_short_OI

## Backward selection
step(s.full_gold, scope=list(lower=s.null_gold, upper=s.full_gold), direction="backward")
# weekly_change ~ merchant_long_OI + swap_spread_OI + manager_long_OI + manager_spread_OI + otherreportable_long_OI + low_volume_traders_long + rel_change_4

## Stepwise selection
step(s.null_gold, scope=list(lower=s.null_gold, upper=s.full_gold), direction="both")
# weekly_change ~ swap_spread_OI + swap_long_OI + rel_change_4 + manager_short_OI

#SVM
svm_gold <- svm(weekly_change~. - date - OI, gold)
summary(svm_gold)

#RF
rf_gold = randomForest(weekly_change~. - date - OI, data = gold)
summary(rf_gold)

#Obtain the OOB error rate and confusion matrix. 
print(rf_gold)

#Determine which features were the best
importance(rf_gold)
##########################################################################################################################################################################
#Milk
nrow(milk) #384
train_milk <- milk[1:392,]
test_milk = milk[393:nrow(milk),]

summary(milk)
milk<- subset(milk, select = -c(2) )
lmmilk <- lm(weekly_change~. - date - OI, data = milk)
summary(lmmilk) #Adjusted R-squared:  0.01432

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_milk <- lm(weekly_change~1, data=milk )
s.full_milk <- lm(weekly_change~. - date - OI, data=milk )

## Forward selection
step(s.null_milk, scope=list(lower=s.null_milk, upper=s.full_milk), direction="forward")
# weekly_change ~ low_volume_traders_long + manager_long_OI + big8_long + swap_short_OI

## Backward selection
step(s.full_milk, scope=list(lower=s.null_milk, upper=s.full_milk), direction="backward")
# weekly_change ~ merchant_long_OI + merchant_short_OI + manager_long_OI + manager_short_OI + otherreportable_short_OI + big8_long

## Stepwise selection
step(s.null_milk, scope=list(lower=s.null_milk, upper=s.full_milk), direction="both")
# weekly_change ~ manager_long_OI + big8_long + swap_short_OI

#SVM
svm_milk <- svm(weekly_change~. - date - OI, milk)
summary(svm_milk)

#RF
rf_milk = randomForest(weekly_change~. - date - OI, data = milk)
summary(rf_milk)

#Obtain the OOB error rate and confusion matrix. 
print(rf_milk)

#Determine which features were the best
importance(rf_milk)
##########################################################################################################################################################################
#Palladium
nrow(palladium) #317
train_palladium <- palladium[1:392,]
test_palladium = palladium[393:nrow(palladium),]

summary(palladium)
palladium<- subset(palladium, select = -c(2) )
lmpalladium <- lm(weekly_change~. - date - OI, data = palladium)
summary(lmpalladium) #Adjusted R-squared:  0.01432

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_palladium <- lm(weekly_change~1, data=palladium )
s.full_palladium <- lm(weekly_change~. - date - OI, data=palladium )

## Forward selection
step(s.null_palladium, scope=list(lower=s.null_palladium, upper=s.full_palladium), direction="forward")
# weekly_change ~ swap_long_OI + otherreportable_long_OI + rel_change_4 + low_volume_traders_short + low_volume_traders_long + merchant_long_OI

## Backward selection
step(s.full_palladium, scope=list(lower=s.null_palladium, upper=s.full_palladium), direction="backward")
# weekly_change ~ merchant_long_OI + swap_long_OI + swap_spread_OI + manager_long_OI + manager_spread_OI + otherreportable_long_OI + 
# otherreportable_spread_OI + low_volume_traders_long + low_volume_traders_short + value_change_13

## Stepwise selection
step(s.null_palladium, scope=list(lower=s.null_palladium, upper=s.full_palladium), direction="both")
# weekly_change ~ swap_long_OI + otherreportable_long_OI + rel_change_4 + low_volume_traders_short + low_volume_traders_long + merchant_long_OI

#SVM
svm_palladium <- svm(weekly_change~. - date - OI, palladium)
summary(svm_palladium)

#RF
rf_palladium = randomForest(weekly_change~. - date - OI, data = palladium)
summary(rf_palladium)

#Obtain the OOB error rate and confusion matrix. 
print(rf_palladium)

#Determine which features were the best
importance(rf_palladium)
##########################################################################################################################################################################
#Platinum
nrow(platinum) #409
train_platinum <- platinum[1:392,]
test_platinum = platinum[393:nrow(platinum),]

summary(platinum)
platinum<- subset(platinum, select = -c(2) )
lmplatinum <- lm(weekly_change~. - date - OI, data = platinum)
summary(lmplatinum) #Adjusted R-squared:  0.01432

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_platinum <- lm(weekly_change~1, data=platinum )
s.full_platinum <- lm(weekly_change~. - date - OI, data=platinum )

## Forward selection
step(s.null_platinum, scope=list(lower=s.null_platinum, upper=s.full_platinum), direction="forward")
# weekly_change ~ ind_swap_spread + value_change_52 + big8_long + otherreportable_short_OI

## Backward selection
step(s.full_platinum, scope=list(lower=s.null_platinum, upper=s.full_platinum), direction="backward")
# weekly_change ~ merchant_short_OI + swap_short_OI + manager_short_OI + manager_spread_OI + ind_swap_spread + value_change_52

## Stepwise selection
step(s.null_platinum, scope=list(lower=s.null_platinum, upper=s.full_platinum), direction="both")
# weekly_change ~ ind_swap_spread + value_change_52 + big8_long + otherreportable_short_OI

#SVM
svm_platinum <- svm(weekly_change~. - date - OI, platinum)
summary(svm_platinum)

#RF
rf_platinum = randomForest(weekly_change~. - date - OI, data = platinum)
summary(rf_platinum)

#Obtain the OOB error rate and confusion matrix. 
print(rf_platinum)

#Determine which features were the best
importance(rf_platinum)
##########################################################################################################################################################################
#Silver
nrow(silver) #442
train_silver <- silver[1:392,]
test_silver = silver[393:nrow(silver),]

summary(silver)
silver<- subset(silver, select = -c(2) )
lmsilver <- lm(weekly_change~. - date - OI, data = silver)
summary(lmsilver) #Adjusted R-squared:  0.01432

#Iterative model selection
#Begin by defining the models with no variables (null) and all variables (full)
s.null_silver <- lm(weekly_change~1, data=silver )
s.full_silver <- lm(weekly_change~. - date - OI, data=silver )

## Forward selection
step(s.null_silver, scope=list(lower=s.null_silver, upper=s.full_silver), direction="forward")
# weekly_change ~ otherreportable_long_OI + low_volume_traders_short + manager_spread_OI

## Backward selection
step(s.full_silver, scope=list(lower=s.null_silver, upper=s.full_silver), direction="backward")
# weekly_change ~ merchant_long_OI + merchant_short_OI + swap_long_OI + swap_short_OI + manager_long_OI + manager_short_OI + manager_spread_OI + 
# otherreportable_long_OI + low_volume_traders_long + value_change_13 + rel_change_13

## Stepwise selection
step(s.null_silver, scope=list(lower=s.null_silver, upper=s.full_silver), direction="both")
# weekly_change ~ otherreportable_long_OI + low_volume_traders_short + manager_spread_OI

#SVM
svm_silver <- svm(weekly_change~. - date - OI, silver)
summary(svm_silver)

#RF
rf_silver = randomForest(weekly_change~. - date - OI, data = silver)
summary(rf_silver)

#Obtain the OOB error rate and confusion matrix. 
print(rf_silver)

#Determine which features were the best
importance(rf_silver)
##########################################################################################################################################################################

