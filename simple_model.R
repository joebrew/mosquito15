library(xts)
#library(mgcv)

# Read in most recent data
source('read_in.R')

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

# Get weather for period 10-21 days prior
vars <- c('Mean_TemperatureF',
          'Min_TemperatureF',
          'Max_TemperatureF',
          'PrecipitationIn',
          'Mean_Wind_SpeedMPH')

for (var in vars){
  # Create new variable
  column_name <- paste0(var, '_10_21')
  df[,column_name] <- NA

  for (i in 1:nrow(df)){
    date <- df$date[i]
    vals <- NA
    vals <- weather[which(weather$date >= (date - 21) &
                            weather$date <= date - 10),var]
    val <- mean(vals, na.rm = TRUE)
    df[i,column_name] <- val
  }
}

# Make a previous mosquitoes per trap for previous traps
vec <- 10:30

for (i in 1:nrow(df)){
  cat(paste0('row ', i, ' of ', nrow(df), '\n'))
  date <- df$date[i]
  trap <- df$trap[i]
  for (j in 1:length(vec)){
    
    val <-  mean(df$n[which(df$date >= (date - vec[j]) &
                              df$date < date &
                              df$trap == trap)], na.rm = TRUE) # getting 0s for impossible rows
    if(val == 0 | is.na(val)){ val <- NA}
    
    df[i,paste0('n_minus_', as.character(vec[j]))] <- 
      val 
  }
}

# Make trap a factor
df$trap <- factor(df$trap)

####
# MODEL AND PREDICT
####

# Split our training and prediction sets
train <- df[which(!is.na(df$n)),]
future <- df[which(df$date > max(train$date)),]

# DEFINE MODEL FORMULA
y <- 'n'
vars <- c(
  #paste0('n_minus_', vec),
  'n_minus_21',
  'trap',
  'Mean_TemperatureF_10_21',
  #'Min_TemperatureF_10_21',
  #'Max_TemperatureF_10_21',
  'PrecipitationIn_10_21'
  #'day_number',
  #'Mean_Wind_SpeedMPH_10_21'
)
x <- paste(vars, collapse = ' + ')

model_formula <- as.formula(paste0(y, ' ~ ', x))

system.time(
fit <- randomForest(model_formula,
                    data = train,
                    na.action = na.omit,
                    ntree = 10000)
)
#train$predicted[which(!is.na(train$n_minus_21))] <- predict(fit)

#####
# Build predictions with error terms - RANDOM FOREST
####

# Create prediction matrix for train
obs_mat <- predict(fit, train, predict.all = TRUE)

# Create prediction matrix for future
pred_mat <- predict(fit, future, predict.all = TRUE)

# To account for fundamental uncertainty at individual level,
# we add randomly sampled errors to each prediction
# Get error matrix
temp <- as.matrix(obs_mat$individual)
error_mat <- apply(temp, 2, function(x) {
  x - train$n
})
rm(temp)


# Add error terms from the observed data:
temp <- as.matrix(pred_mat$individual)
for (j in 1:ncol(temp)) {
  errors <- sample(error_mat[, j], nrow(temp),
                   replace = TRUE)
  temp[, j] <- temp[, j] + errors
}
pred_mat_with_error <- temp
rm(temp)

# Now, having introduced fundamental uncertainty,
# we can proceed with the drawing up of individual-level
# prediction intervals, as well as a key to link the
# matrix of predictions iwth individuals in the future dataset

# Get point estimate
future$predicted <- pred_mat$aggregate
# Get prediction intervals
future$lwr <- apply(pred_mat_with_error, 1,
                      function(x) {
                        quantile(x, probs = 0.025, na.rm = TRUE)
                      })
future$upr <- apply(pred_mat_with_error, 1,
                      function(x) {
                        quantile(x, probs = 0.975, na.rm = TRUE)
                      })

future %>% group_by(date) %>% summarise(n = sum(predicted, na.rm = T))

#####
# BOOTSTRAP LINEAR REGRESSION
#####

# Source function for bootstrapping
source('helpers.R')

# Run bootstrapped model (predicting only for date)
temp <- bs_lm(model_formula,
              n = 1000,
              p = 0.63,
              train_data = train,
              predict_data = future,
              agg_by_date = TRUE)

# Run the same, but generate predictions for each trap
temp2 <- bs_lm(model_formula,
              n = 1000,
              p = 0.63,
              train_data = train,
              predict_data = future,
              agg_by_date = FALSE)

# Run on historical data
temp3 <- bs_lm(model_formula,
               n = 1000,
               p = 0.63,
               train_data = train,
               predict_data = train,
               agg_by_date = TRUE)


x <- ts[,c('date', 'n')]
x <- left_join(x, temp3)
plot(x$date, x$n, type = 'l')
lines(x$date, x$pred, col = 'red')
plot(x$n, x$pred)
table(x$n >= x$low &
        x$n <= x$high)
rmse(x$n, x$pred)

plot(x$date, x$n, type = 'l')
lines(x$date, x$pred, col = 'red')

fit <- lm(model_formula, data = train)
plot(train$date, train$n, type = 'l')
lines(train$date, predict(fit, train), col = 'red')


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
