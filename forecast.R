library(plyr)
#library(xts)
library(dplyr)
#library(mgcv)

# Read in most recent data
source('read_in.R')

# How many days ahead do I want to be able to forecast
forecast_ahead <- 10
today <- Sys.Date()
minimum <- abs(as.numeric(recent - today)) + forecast_ahead

# What about how far back to go with weather stuff?
minimum_weather <- 1

#####
# TRAP-SPECIFIC PREDICTIONS
#####
train <- raw_data
# Ignore by mosquito type, grouping by trap-date
train <- train %>%
  group_by(date, trap) %>%
  summarise(n = sum(n))

# Make a comprehensive dataframe with both past and future
df <- 
  expand.grid(
    date = seq(min(train$date), Sys.Date()+9, by = 1),
    trap = 1:10)

# Generate some more features
df$year <- as.numeric(format(df$date, '%Y'))
df$day_number <- as.numeric(format(df$date, '%j'))

# Get trap data
df <- left_join(x = df, 
                y = train)

# Eliminate historical rows with no observation
recent <- max(df$date[which(!is.na(df$n))])
df <- df[which(df$date > recent | !is.na(df$n)),]

# Get weather for period days prior
vars <- c('Mean_TemperatureF',
          'Min_TemperatureF',
          'Max_TemperatureF',
          'PrecipitationIn',
          'Mean_Wind_SpeedMPH')

lows <- (minimum_weather):(minimum_weather+20)
highs <- (minimum_weather+5):(minimum_weather+40)

for (var in vars){
  print(paste0('Working on ', var, '\n'))
  for(low in lows){
    for(high in highs){
      if(low < high){
        cat(paste0(var, '_', low, '_', high, '\n'))
        # Create new variable
        column_name <- paste0(var, '_', low, '_', high)
        df[,column_name] <- NA
        for (i in 1:nrow(df)){
          date <- df$date[i]
          vals <- NA
          vals <- weather[which(weather$date >= (date - high) &
                                  weather$date <= date - low),var]
          val <- mean(vals, na.rm = TRUE)
          df[i,column_name] <- val
        }
      }
    }
  }
}

# Make a previous mosquitoes per trap for previous traps
vec <- minimum:45

for (i in 1:nrow(df)){
  cat(paste0('row ', i, ' of ', nrow(df), '\n'))
  date <- df$date[i]
  trap <- df$trap[i]
  for (j in 1:length(vec)){
    
    val <-  mean(df$n[which(df$date >= (date - vec[j]) &
                              df$date < date &
                              df$trap == trap)], na.rm = TRUE) # getting 0s for impossible rows
    if(val == 0 | is.na(val)){ val <- sample(df$n, 1)}
    
    df[i,paste0('n_minus_', as.character(vec[j]))] <- 
      val 
  }
}

# Make trap a factor
df$trap <- factor(df$trap)

# Get the number of traps for each collection date
temp <- ts[,c('date', 'n_traps')]
df <- left_join(df, temp)

####
# MODEL AND PREDICT
####

# Split our training and prediction sets
train <- df[which(!is.na(df$n)),]
future <- df[which(df$date > max(train$date)),]

# Adjust future to make sure that n_traps are specified
future$n_traps <- 10

#save.image('/home/joebrew/Desktop/fast.RData')

# DEFINE MODEL FORMULA
y <- 'n'
# vars <- c(
#   'n_traps',
#   paste0('n_minus_', vec),
#   ######## ADD STUFF HERE
#   'n_minus_21',
#   'trap',
#   'Mean_TemperatureF_10_21',
#   #'Min_TemperatureF_10_21',
#   #'Max_TemperatureF_10_21',
#   'PrecipitationIn_10_21',
#   'day_number'
#   #'Mean_Wind_SpeedMPH_10_21'
# )
vars <- names(df)[which(!names(df) %in% c('date', 'year', 'n'))]
x <- paste(vars, collapse = ' + ')

model_formula <- as.formula(paste0(y, ' ~ ', x))



system.time(
  fit <- randomForest(model_formula,
                      data = train,
                      na.action = na.omit,
                      ntree = 1000,
                      predict.all = TRUE)
) 
# 1351 / 60 

# #### LENDABLE STYLE CIS
# #####
# # Build predictions with error terms - RANDOM FOREST
# ####
# 
# # Create prediction matrix for train
# obs_mat <- predict(fit, train, predict.all = TRUE)
# 
# # Create prediction matrix for future
# pred_mat <- predict(fit, future, predict.all = TRUE)
# 
# # To account for fundamental uncertainty at individual level,
# # we add randomly sampled errors to each prediction
# # Get error matrix
# temp <- as.matrix(obs_mat$individual)
# error_mat <- apply(temp, 2, function(x) {
#   x - train$n
# })
# rm(temp)
# 
# 
# # Add error terms from the observed data:
# temp <- as.matrix(pred_mat$individual)
# for (j in 1:ncol(temp)) {
#   errors <- sample(error_mat[, j], nrow(temp),
#                    replace = TRUE)
#   temp[, j] <- temp[, j] + errors
# }
# pred_mat_with_error <- temp
# rm(temp)
# 
# # Now, having introduced fundamental uncertainty,
# # we can proceed with the drawing up of individual-level
# # prediction intervals, as well as a key to link the
# # matrix of predictions iwth individuals in the future dataset
# 
# # Get point estimate
# future$predicted <- pred_mat$aggregate
# # Get prediction intervals
# future$lwr <- apply(pred_mat_with_error, 1,
#                     function(x) {
#                       quantile(x, probs = 0.025, na.rm = TRUE)
#                     })
# future$upr <- apply(pred_mat_with_error, 1,
#                     function(x) {
#                       quantile(x, probs = 0.975, na.rm = TRUE)


predictions <- predict(fit, future, predict.all = TRUE) 
predictions_ci <- apply(predictions$individual, 1, function(x){
  data.frame(predicted = mean(x, na.rm = TRUE), #+ c(-1,1) * sd(x),
             lower = quantile(x, 0.1, na.rm = TRUE),
             upper = quantile(x, 0.9, na.rm = TRUE))
})
predictions_ci <- do.call(rbind, predictions_ci)
# Join predictions to future
future <- cbind(future, predictions_ci)

#####
# GET AGGREGATED FUTURE
#####
future_agg <- future %>% 
  group_by(date) %>% 
  summarise(n = sum(predicted, na.rm = T))

# Get confidence bands 
for (i in 1:nrow(future_agg)){
  date <- future_agg$date[i]
  rows_in_question <- which(future$date == date)
  sub_predictions <- predictions$individual[rows_in_question,]
  #Reorder a bit
  for (j in 1:nrow(sub_predictions)){
    my_row <- sub_predictions[j,]
    sub_predictions[j,] <- sample(my_row, length(my_row), replace = FALSE)
  }
  vals <- apply(sub_predictions, 2, median)
  temp <- data.frame(predicted = mean(vals, na.rm = TRUE),
                     lower = quantile(vals, 0.1, na.rm = TRUE),
                     upper = quantile(vals, 0.9, na.rm = TRUE))
  row.names(temp) <- NULL
  if(i == 1){
    old_temp <- temp
  } else{
    old_temp <- rbind(old_temp, temp)
  }
}
future_agg <- cbind(future_agg, old_temp)


# Create a ts with the future predictions
future_agg$n_traps <- 10

ts_with_future <- rbind.fill(ts, future_agg)
#####
# SAVE CHECKPOINT
#####
save.image(paste0(proj_root, '/checkpoint.RData'))


#####
# EXPLORE VARIABLE IMPORTANCE
#####
varImpPlot(fit)

# #####
# # BOOTSTRAP LINEAR REGRESSION
# #####
# 
# # Source function for bootstrapping
# source('helpers.R')
# 
# # Run bootstrapped model (predicting only for date)
# temp <- bs_lm(model_formula,
#               n = 1000,
#               p = 0.63,
#               train_data = train,
#               predict_data = future,
#               agg_by_date = TRUE)
# 
# # Run the same, but generate predictions for each trap
# temp2 <- bs_lm(model_formula,
#                n = 1000,
#                p = 0.63,
#                train_data = train,
#                predict_data = future,
#                agg_by_date = FALSE)
# 
# # Run on historical data
# temp3 <- bs_lm(model_formula,
#                n = 1000,
#                p = 0.63,
#                train_data = train,
#                predict_data = train,
#                agg_by_date = TRUE)
# 
# 
# x <- ts[,c('date', 'n')]
# x <- left_join(x, temp3)
# plot(x$date, x$n, type = 'l')
# lines(x$date, x$pred, col = 'red')
# plot(x$n, x$pred)
# table(x$n >= x$low &
#         x$n <= x$high)
# rmse(x$n, x$pred)
# 
# plot(x$date, x$n, type = 'l')
# lines(x$date, x$pred, col = 'red')
# 
# fit <- lm(model_formula, data = train)
# plot(train$date, train$n, type = 'l')
# lines(train$date, predict(fit, train), col = 'red')


# # FIT MODEL
# fit <- gam(model_formula,
#           data = train)
# 
# # PREDICT
# train$predicted <- predict(fit, train)
# 
# # GET AGGREGATIONS FROM TRAIN 
# train_agg <- train %>%
#   group_by(date) %>%
#   summarise(predicted = sum(predicted, na.rm = TRUE),
#             n = sum(n, na.rm = TRUE))
# 
# # Plot
# plot(train_agg$n,
#      train_agg$predicted)
# 
# plot(train_agg$date,
#      train_agg$n,
#      type = 'l')
# 
# lines(train_agg$date,
#       train_agg$predicted,
#       col = 'red')
# summary(fit)








# 
# 
# # Interpolate or no?
# interpolate = FALSE
# 
# #
# 
# 
# ###########################
# if(interpolate){
#   # Read in Yoni's help
#   source('helpers.R')
#   
#   # Create an expanded ts with interpolated n
#   ts_expanded <- data.frame(date = 
#                               seq(min(ts$date),
#                                   max(ts$date), 
#                                   1))
#   ts_expanded <- left_join(x = ts_expanded,
#                            y = ts)
#   
#   ts_xts <- as.xts(ts_expanded[,-1],
#                    order.by = ts_expanded$date)
#   
#   # Interpolate n and n_traps
#   ts_xts$n <- na.spline(ts_xts$n)
#   ts_xts$n_traps <- na.spline(ts_xts$n_traps)
#   
#   # Write over ts
#   ts <- data.frame(ts_xts); rm(ts_xts)
#   ts$date <- as.Date(row.names(ts))
#   
# }
# 
# # Get weather for period 10-21 days prior
# vars <- c('Mean_TemperatureF',
#           'Min_TemperatureF',
#           'Max_TemperatureF',
#           'PrecipitationIn',
#           'Mean_Wind_SpeedMPH')
# 
# for (var in vars){
#   # Create new variable
#   column_name <- paste0(var, '_10_21')
#   ts[,column_name] <- NA
#   
#   for (i in 1:nrow(ts)){
#     date <- ts$date[i]
#     vals <- weather[which(weather$date >= (date - 21) &
#                            weather$date <= date - 10),var]
#     val <- mean(vals, na.rm = TRUE)
#     ts[i,column_name] <- val
#   }
# }
# 
# # Make a mosquitoes per trap var
# ts$y <- ts$n / ts$n_traps
# 
# # Make a previous mosquitoes per trap for previous traps
# vec <- 10:50
# for (i in 1:length(vec)){
#   ts[,paste0('y_minus_', as.character(vec[i]))] <- 
#     c(rep(NA, i),
#       ts$y[1:(nrow(ts) - i)])
# }
# 
# #####
# # MAKE TRAINING AND TESTING DATASETS
# #####
# ts$year <- as.numeric(format(ts$date, '%Y'))
# ts$day_number <- as.numeric(format(ts$date, '%j'))
# ts <- data.frame(ts)
# train <- ts[which(ts$year <= 2013),]
# test <- ts[which(ts$year >= 2014),]
# 
# #####
# # DEFINE MODEL FORMULA
# #####
# y <- 'y'
# x <- paste(#'n_traps',
#            #paste0('y_minus_', vec),
#            'Mean_TemperatureF_10_21',
#            'Min_TemperatureF_10_21',
#            'Max_TemperatureF_10_21',
#            'PrecipitationIn_10_21',
#            'Mean_Wind_SpeedMPH_10_21',
#            #'day_number', 
#            sep = ' + ') 
# model_formula <- as.formula(paste0(y, ' ~ ', x))
# 
# ###############
# # Linear regression
# fit_linear <- lm(model_formula,
#                  data = train)
# 
# ###############
# # Random forest
# fit_rf <- randomForest(model_formula,
#                     data = train,
#                     na.action = na.omit)
# 
# ###############
# # Generate predctions
# test$predicted_linear <- predict(fit_linear, test)
# test$predicted_rf <- predict(fit_rf, test)
# 
# #####
# # EXPLORE PREDICTIONS
# #####
# 
# # Observed
# plot(test$date,
#      test$y,
#      type = 'l',
#      lwd = 2)
# 
# # Predicted from linear model
# lines(test$date, test$predicted_linear,
#       col = 'darkgreen')
# 
# # Predicted from random forest
# lines(test$date, test$predicted_rf,
#       col = 'darkred')
# 

# 
# # linear
# rmse(observed = test$y, predicted = test$predicted_linear)
# rmse(observed = test$y, predicted = test$predicted_rf)
# # just use mean of past
# rmse(observed = test$y, predicted = rep(mean(train$y), nrow(test)))
# 
# #####
# # 10-FOLD CV
# #####
# 
# # Assign random fold
# ts$fold <- sample(1:10, nrow(ts), replace = TRUE)
# 
# ts$predicted_cv <- NA
# 
# for (i in 1:10){
#   
#   # Subset for everything except that fold
#   sub_train <- ts[which(ts$fold != i),]
#   sub_test <- ts[which(ts$fold == i),]
#   
#   # Fit model
#   sub_fit <- lm(model_formula,
#                    data = sub_train)
#   
#   # Predict
#   ts$predicted_cv[which(ts$fold == i)] <- 
#     predict(sub_fit, sub_test)
# }
# 
# # Plot 10-fold
# plot(ts$y, ts$predicted_cv)





######################################################################
######################################################################
######################################################################
# OLD FORECAST.R
######################################################################
######################################################################
######################################################################

# # YONI TIME!
# source('read_in.R')
# 
# # Number of mosquitoes observed at previous collection
# ts$previous_mosquitoes <- 
#   c(NA, ts$n[1:(nrow(ts) - 1)])
# 
# 
# # Join weather and ts
# ts <- inner_join(x = ts, 
#                  y = weather[,c('date', 
#                                 'Mean_TemperatureF', 
#                                 'PrecipitationIn',
#                                 'Min_TemperatureF', 
#                                 'Max_TemperatureF')], 
#                  by = 'date')
# 
# # Make a mosquitoes per trap var
# ts$y <- ts$n / ts$n_traps
# 
# # Make a previous mosquitoes per trap
# ts$previous_y <- 
#   c(NA, ts$y[1:(nrow(ts) - 1)])
# 
# # Make a training dataset
# train <- ts[-1,c('y', 'Mean_TemperatureF', 'PrecipitationIn',
#                  'Min_TemperatureF', 'Max_TemperatureF', 'previous_y')]
# 
# ###############
# # Random forest
# fit <- randomForest(y ~ previous_y,
#                     data = train)
# 
# summary(fit)
# plot(predict(fit), 
#      train$y,
#      xlab = 'Predicted',
#      ylab = 'Observed')
# 
# fake <- data.frame(previous_y = 1:2000)
# plot(fake$previous_y, predict(fit, fake),)
# 
# ###############
# # Linear model
# fit <- lm(y ~ previous_y, 
#           data = train)
# 
# summary(fit)
# plot(predict(fit), 
#      train$y,
#      xlab = 'Predicted',
#      ylab = 'Observed')
# 
# fake <- data.frame(previous_y = 1:2000)
# plot(fake$previous_y, predict(fit, fake),)
# 
# 
# # Previous days max/min/avg temp and precipitation
# 
# #############################################################
# #############################################################
# #############################################################
# 
# #####
# # CREATE AN AUGMENTED WEATHER DATAFRAME
# # WITH TS FEATURES
# #####
# weather_augmented <- weather
# 
# # Create dataframe for the next two weeks
# next_two_weeks <- as.data.frame(matrix(NA, 
#                                        ncol = ncol(weather),
#                                        nrow = 14))
# colnames(next_two_weeks) <- colnames(weather)
# next_two_weeks$Date <- as.Date(seq(Sys.Date(), (Sys.Date()+13), 1))
# 
# # Combine with weather_augmented
# weather_augmented <- rbind(weather_augmented, next_two_weeks)
# 
# 
# for (column in colnames(weather)[2:4]){
#   # looking only at the period 14 to 30 days before
#   for (day in 14:30){
#     weather_augmented[,paste0(column, '_minus_', day)] <- NA
#   }
# }
# 
# for (i in 1:nrow(weather_augmented)){
#   for (column in colnames(weather)[2:4]){
#     for (day in 14:30){
#       if(i > 30){
#         date <- weather_augmented$Date[i]
#         val <- weather_augmented[(i-day),column]
#         weather_augmented[i,paste0(column, '_minus_', day)] <- val
#       }
#     }
#   }
# }
# 
# # Remove the non-historic observations
# weather_augmented <- weather_augmented[,!colnames(weather_augmented) %in% c('Max_TemperatureF',  
#                                                                             'Min_TemperatureF',    
#                                                                             'PrecipitationIn')]
# 
# 
# #####
# # CREATE DATAFRAME WITH GENERATED FEATURES FOR MODEL TRAINING
# #####
# train <- left_join(ts, weather_augmented, by = 'date')
# 
# # Remove date so that it's not used in modeling
# train_date <- train$date
# train$date <- NULL
# 
# # Remove any NA columns
# train_names <- names(train)
# bads <- rep(FALSE, length(train_names))
# for (j in 1:ncol(train)){
#   column <- train[,train_names[j]]
#   if(length(which(is.na(column))) > 0){
#     bads[j] <- TRUE
#   }
# }
# train <- train[,!bads]
# 
# # Fix weird joined column issues
# train <- train[,names(train)[!grepl('[.]x', names(train))]] #get rid of .x columns
# names(train) <- gsub('[.]y', '', names(train)) #rename .y columns
# train <- train[,!duplicated(names(train))] # remove any duplicates
# 
# # Exclude non-historical observations
# train <- train[,c('n_traps', 'n', 
#                   names(train)[grepl('_minus_', names(train))])]
# 
# #####
# # TRAIN MODEL
# #####
# fit <- randomForest(n ~ . ,
#                     data = train,
#                     ntree = 5000,
#                     na.action = 'na.omit')
# 
# # Also get a predictions matrix
# obs_mat <- predict(fit,
#                    train,
#                    predict.all = TRUE)
# # And error matrix
# temp <- as.matrix(obs_mat$individual)
# error_mat <- apply(temp, 2, function(x){
#   x - train$n
# })
# rm(temp)
# 
# # Predict on train
# train$predicted <- predict(fit)
# 
# plot(train_date, train$n, type = 'l')
# lines(train_date, train$predicted, col = 'red')
# 
# #####
# # PREDICT ON CURRENT
# #####
# 
# current <- weather_augmented[which(weather_augmented$date >= (Sys.Date() - 30)),]
# current$n_traps <- 10
# 
# current$predicted <- predict(fit, current)
# 
# # Also get a matrix of each tree's results
# pred_mat <- predict(fit, current, predict.all = TRUE)
# 
# # Add error terms from the observed data:
# temp <- as.matrix(pred_mat$individual)
# # for (j in 1:ncol(temp)) {
# #   errors <- sample(error_mat[, j], nrow(temp),
# #                    replace = TRUE)
# #   # temp[,j] <- temp[,j] + errors # ! too big - currently not adding errors
# #   temp[,j] <- temp[,j] + errors
# #   
# # }
# pred_mat_with_error <- temp
# rm(temp)
# 
# # Now that we've introduced fundamental uncertainy,
# # we can draw up individual-level predicition intervals,
# 
# # Get point estimate
# current$predicted <- pred_mat$aggregate
# 
# # Get confidence bounds
# current$lwr <- apply(pred_mat_with_error, 1, 
#                      function(x){
# #                        quantile(x, probs = 0.025)
#                        quantile(x, probs = 0.1)
#                        
#                      })
# current$upr <- apply(pred_mat_with_error, 1,
#                      function(x) {
# #                        quantile(x, probs = 0.975)
#                        quantile(x, probs = 0.9)
#                        
#                      })
# 
# plot(current$date,
#      current$predicted,
#      type = 'l')
# 
# 
# #####
# # COMBINE TRAIN AND CURRENT
# #####
# train_small <- train[,'n']
# train_small$date <- train_date
# train_small$observed <- TRUE
# 
# current_small <- current[,c('predicted', 'date')]
# current_small$observed <- TRUE
# names(current_small)[1] <- 'n'
# 
# combined <- rbind(train_small, current_small)
# 
# plot(combined$date, combined$n, type = 'n')
# 
# lines(train_small$date, train_small$n)
# lines(current_small$date, current_small$n, col = 'red')
# 
# #####
# # SAVE CHECKPOINT
# #####
# save.image(paste0(proj_root, '/checkpoint.RData'))
