# Yoni's NA function
add.nas <- function(xts.obj) {
  xts.dates <- index(xts.obj)
  dates <- seq(xts.dates[1], xts.dates[length(xts.dates)], by=1)
  na.xts <- as.xts(rep(NA, length(dates)), order.by=as.Date(dates))
  xts.w.nas <- merge(na.xts, xts.obj)
  
  # drop place-holder column
  xts.w.nas <- xts.w.nas[,!names(xts.w.nas) == "na.xts"]
  
  xts.w.nas
}

# Joe's BS linear regression function
bs_lm <- function(model_formula,
                  n = 1000,
                  p = 0.63,
                  train_data = train,
                  predict_data = future,
                  agg_by_date = FALSE){
  
    # Create prediction matrix
    predict_mat <- matrix(NA,
                         nrow = nrow(predict_data),
                         ncol = n)
    
    
    # Bootstrap
    for (i in 1:n){
      
      # Progress indicator
      cat(paste0(i, ' of ', n, '\n'))
      
      # Subset randomly
      sub_data <- train_data[sample(1:nrow(train_data), 
                                    round(p * nrow(train_data)), 
                                    replace = TRUE),]
      
      # Fit model
      sub_fit <- lm(model_formula,
                     data = sub_data)
      
      # Generate predictions
      preds <- predict(sub_fit, predict_data)
      
      # ERROR
      error_to_add <- quantile(abs(sub_fit$residuals), probs = 0.475)
      highs <- preds + error_to_add
      lows <- preds - error_to_add
      
      vec12 <- sample(c(1,2), length(preds), replace = TRUE)
      
      # Add predictions to matrix
      predict_mat[,i] <-ifelse(vec12 == 1,
                            highs,
                            lows)
      
    }
    # Get 95 % confidence quantiles for predict_mat
    cis <- apply(predict_mat, 
                 1,
                 function(x){quantile(x, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)})
    cis <- t(cis)
    
    # Point estimate and cis
    vals <- data.frame(low = cis[,1],
                       pred = cis[,2],
                       high = cis[,3])
    
#     # Bind with the info we're predicting on
    return_obj <- cbind(predict_data[,c('date', 'trap')],
                        vals)
#     return_obj <- cbind(return_obj, predict_data)
  
    # AGGREGATING BY DATE
    if(agg_by_date){
      temp <- predict_data %>%
        group_by(date, trap) %>%
        summarise(x = NA)
      temp$x <- NULL
      temp <- cbind(temp, predict_mat)
      
      # Get for each date
      agg <- data.frame(date = unique(sort(temp$date)))
      agg$low <- agg$pred <- agg$high <- NA
      
      # In each row of temp, randomly reorder the units
      # which in effect means randomly sampling
      # from each unit's possible values
      # rather than taking the entire individual model output together
      
#       temp <- apply(temp, 1, function(x){
#         x <- sample(x, 
#                     ncol(temp),#10,#length(x), # keep at size of matrix, or allow for bigger?
#                     replace = TRUE)
#       })

      for (k in 1:nrow(agg)){
        sub_data <- temp[which(temp$date == agg$date[k]),]
        
        vals <- apply(as.matrix(sub_data[,3:ncol(sub_data)]), 2, sum, na.rm = TRUE)
        
        # Get 95 % confidence quantiles
        vals <- quantile(vals, probs = c(0.025, 0.5, 0.975))
        agg$pred[k] <- vals[2]
        agg$low[k] <- vals[1]
        agg$high[k] <- vals[3]
      }
      return_obj <- agg
    }
    return(return_obj)
    
}

# # Compare rmse
rmse <- function(observed, predicted){
  error <- observed - predicted
  sqrt(mean(error^2, na.rm = TRUE))
}