install.packages("e1071")
install.packages("leaps")
install.packages("caret")
install.packages("randomForest")
library(e1071)
library(leaps)
library(caret)
library(randomForest)


get_metrics <- function(data) {
  
  if (nrow(data) < 200) {
    return("Not enough data to model")
  }   else {
    
    data <- data[order(data$date, data$name, decreasing = FALSE), ]
    
    count_products = length(unique(data$name))
    
    test_range = c((nrow(data) - count_products*100 + 1):(nrow(data) - count_products))
    
    # the testing range is roughly the last 2 years
    
    
    predictions_linear <- NA
    predictions_svm <- NA
    predictions_rf <- NA
    
    data$date <- NULL
    data$name <- NULL
    
    for (i in seq(from=min(test_range), to=max(test_range), by=(max(test_range) - min(test_range) + 1))) {
      
      #Model trains only once. (Need to create very costly AWS instances!).  
      
      train <- data[1:(i-1), ]
      test <- data[i:nrow(data), -which(colnames(data)=="weekly_change")]
      
      b1.null <- lm(weekly_change~1, data=train)
      b1.full <- lm(weekly_change ~., data=train)
      b1.bs <- step(b1.full, scope=list(lower=b1.null, upper=b1.full), direction="backward")
      predictions_linear <- c(predictions_linear, predict(b1.bs, newdata = test))
      
      
      svm_tune <- tune(svm, weekly_change ~. , data = train, ranges = list(epsilon = c(0.0001, 0.01, 0.1, 1), cost = 1.5^(1:3)))
      best_svm_model <- svm_tune$best.model
      predictions_svm <- c(predictions_svm, predict(best_svm_model, newdata = test)) 
      
      
      cvCtrl = trainControl(method = "repeatedcv",number = 10, repeats = 3, search = "grid")
      newGrid = expand.grid(mtry = c(1,3))
      classifierRandomForest <- train(weekly_change ~. , data=train, trControl = cvCtrl, method="rf", tuneGrid=newGrid)
      curClassifier = classifierRandomForest
      predictions_rf = c(predictions_rf, predict(curClassifier, newdata = test))
    }
    
    pred_linear <- predictions_linear[2:length(predictions_linear)]
    pred_svm <- predictions_svm[2:length(predictions_svm)]
    pred_rf <- predictions_rf[2:length(predictions_rf)]
    
    ground_truth = data$weekly_change[(nrow(data) - length(pred_linear) + 1): nrow(data)]
    
    metrics_linear <- t(as.data.frame(metrics(ground_truth, pred_linear)))
    metrics_svm <- t(as.data.frame(metrics(ground_truth, pred_svm)))
    metrics_rf <- t(as.data.frame(metrics(ground_truth, pred_rf)))
    
    metrics_colnames <- c("Prediction_length", "RMSE", "dir_wrong_percnt", "corr_preds", "big_pos_percnt", "big_neg_percnt", "annual_return")
    colnames(metrics_linear) <- metrics_colnames
    colnames(metrics_svm) <- metrics_colnames
    colnames(metrics_rf) <- metrics_colnames
    
    combined_metrics <- rbind(metrics_linear, metrics_svm, metrics_rf)
    
    for (i in 1:nrow(combined_metrics)) {
      combined_metrics[i, 1] = (combined_metrics[i, 1])/count_products
    }
    
    return(combined_metrics)
  }
}


commodities_rem <- c(3,6,7,8,11,14,19,20,21,23,24,25,26,27,28)
commoditiesone_rem <- c(3,6,7,8,11,14,19,20,21,23,24,25)
financial_rem <- (c(3,6,7,8,9,12,15,20,21,22,23,25,26,27,28,29,30))
financialone_rem <- c(3,6,7,8,9,12,15,20,21,22,23,25,26,27)

# preparing the data sets prior to modeling

currency_s1.1 <- currency_s1.1[, -financialone_rem]
currency_s2 <- currency_s2[, -financial_rem]
currency_s3 <- currency_s3[, -financial_rem]
currency_s4 <- currency_s4[, -financial_rem]
currency_s5.1 <- currency_s5.1[, -financialone_rem]
currency_s6.1 <- currency_s6.1[, -financialone_rem]

bonds_s1.1 <- bonds_s1.1[, -financialone_rem]
bonds_s2 <- bonds_s2[, -financial_rem]
bonds_s3 <- bonds_s3[, -financial_rem]
bonds_s4 <- bonds_s4[, -financial_rem]
bonds_s5.1 <- bonds_s5.1[, -financialone_rem]
bonds_s6.1 <- bonds_s6.1[, -financialone_rem]

US_stocks_s1.1 <- US_stocks_s1.1[, -financialone_rem]
US_stocks_s2 <- US_stocks_s2[, -financial_rem]
US_stocks_s3 <- US_stocks_s3[, -financial_rem]
US_stocks_s4 <- US_stocks_s4[, -financial_rem]
US_stocks_s5.1 <- US_stocks_s5.1[, -financialone_rem]
US_stocks_s6.1 <- US_stocks_s6.1[, -financialone_rem]

all_financials_s1.1 <- all_financials_s1.1[, -financialone_rem]
all_financials_s2 <- all_financials_s2[, -financial_rem]
all_financials_s3 <- all_financials_s3[, -financial_rem]
all_financials_s4 <- all_financials_s4[, -financial_rem]
all_financials_s5.1 <- all_financials_s5.1[, -financialone_rem]
all_financials_s6.1 <- all_financials_s6.1[, -financialone_rem]

grains_s1.1 <- grains_s1.1[, -commoditiesone_rem]
grains_s2 <- grains_s2[, -commodities_rem]
grains_s3 <- grains_s3[, -commodities_rem]
grains_s4 <- grains_s4[, -commodities_rem]
grains_s5.1 <- grains_s5.1[, -commoditiesone_rem]
grains_s6.1 <- grains_s6.1[, -commoditiesone_rem]

metals_s1.1 <- metals_s1.1[, -commoditiesone_rem]
metals_s2 <- metals_s2[, -commodities_rem]
metals_s3 <- metals_s3[, -commodities_rem]
metals_s4 <- metals_s4[, -commodities_rem]
metals_s5.1 <- metals_s5.1[, -commoditiesone_rem]
metals_s6.1 <- metals_s6.1[, -commoditiesone_rem]

meats_s1.1 <- meats_s1.1[, -commoditiesone_rem]
meats_s2 <- meats_s2[, -commodities_rem]
meats_s3 <- meats_s3[, -commodities_rem]
meats_s4 <- meats_s4[, -commodities_rem]
meats_s5.1 <- meats_s5.1[, -commoditiesone_rem]
meats_s6.1 <- meats_s6.1[, -commoditiesone_rem]

softs_s1.1 <- softs_s1.1[, -commoditiesone_rem]
softs_s2 <- softs_s2[, -commodities_rem]
softs_s3 <- softs_s3[, -commodities_rem]
softs_s4 <- softs_s4[, -commodities_rem]
softs_s5.1 <- softs_s5.1[, -commoditiesone_rem]
softs_s6.1 <- softs_s6.1[, -commoditiesone_rem]

energies_s1.1 <- energies_s1.1[, -commoditiesone_rem]
energies_s2 <- energies_s2[, -commodities_rem]
energies_s3 <- energies_s3[, -commodities_rem]
energies_s4 <- energies_s4[, -commodities_rem]
energies_s5.1 <- energies_s5.1[, -commoditiesone_rem]
energies_s6.1 <- energies_s6.1[, -commoditiesone_rem]


all_commodities_s1.1 <- all_commodities_s1.1[, -commoditiesone_rem]
all_commodities_s2 <- all_commodities_s2[, -commodities_rem]
all_commodities_s3 <- all_commodities_s3[, -commodities_rem]
all_commodities_s4 <- all_commodities_s4[, -commodities_rem]
all_commodities_s5.1 <- all_commodities_s5.1[, -commoditiesone_rem]
all_commodities_s6.1 <- all_commodities_s6.1[, -commoditiesone_rem]


# block 1

sfsefsef <- get_metrics(currency_s1.1)
owwsfds <- get_metrics(bonds_s1.1)
ewnfsgvfwssfc <- get_metrics(US_stocks_s1.1)
wdwfwsd <- get_metrics(all_financials_1.1)
sfsefsef <- get_metrics(all_commodities_s1.1)


