metrics <- function(actual_change_vector, pred_change_vector) {
  
  x = actual_change_vector
  y = pred_change_vector
  
  x = as.data.frame(x)
  y = as.data.frame(y)
  
  colnames(x) <- c("act_change")
  colnames(y) <- c("pred_change")
  
  z = cbind(x,y)
  
  z$pred_direction <- ifelse(z$pred_change > 0, 1, 0)
  z$act_direction <- ifelse(z$act_change > 0, 1, 0)
  
  mismatch_count = 0
  
  for (i in 1:nrow(z)) {
    if (z$pred_direction[i] != z$act_direction[i]) {
      mismatch_count = mismatch_count + 1
    }
  }
  
  dir_wrong_percnt <- mismatch_count*100/nrow(z)
  
  # So, if dir_wrong is 50, then the model is as good as flipping a coin (random chance)
  
  # How correlated are my predicted change and the actual change
  
  corr_predictions <- cor(z$pred_change, z$act_change)
  
  # can the model predict at least big moves correctly?
  
  totallength <- nrow(z) # Total length of sample 
  
  percent.predbigpos <- nrow(subset(z, z$act_change > 1 & z$pred_change > 1))*100/nrow(subset(z, z$act_change > 1))
  # % times model predicted a big pos move
  
  percent.predbigneg <- nrow(subset(z, z$act_change < -1 & z$pred_change < -1))*100/nrow(subset(z, z$act_change < -1)) 
  # % times model predicted a big neg move
  
  z$betting <- NA
  
  for (i in 1:nrow(z)) {
    
    if (z$pred_change[i] > 0) {
      z$betting[i] <- z$act_change[i]
    } else {
      z$betting[i] <- (z$act_change[i])*(-1)
    }
    
  }
  
  z$total_return <- NA
  z$total_return[1] = 1 + (z$betting[1]/100)
  
  
  for (i in 2:nrow(z)) {
    
    z$total_return[i] = (1 + (z$betting[i]/100))*z$total_return[i - 1]
    
  }
  
  annualized_return_weekly = (z$total_return[nrow(z)])^(1/nrow(z))
  
  annualized_return = (((annualized_return_weekly)^52 - 1))*100
  
  RMSE_model = sqrt(mean((x - y)^2))
  
  metrics_list <- c(totallength, RMSE_model, dir_wrong_percnt, corr_predictions, percent.predbigpos, percent.predbigneg, annualized_return)
  
  
  write.csv(t(as.data.frame(metrics_list)), "metrics_list.csv", row.names = FALSE, col.names = FALSE)
  
  return(metrics_list)
  
  # ofcourse, the payoffs will be different for options. I will get to that stage later
  
}