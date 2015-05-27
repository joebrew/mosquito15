library(xts)

# Read in most recent data
source('read_in.R')

# Interpolate or no?
interpolate = TRUE

if(interpolate){
  # Read in Yoni's help
  source('helpers.R')
  
  # Create an expanded ts with interpolated n
  ts_expanded <- data.frame(date = 
                              seq(min(ts$date),
                                  max(ts$date), 
                                  1))
  ts_expanded <- left_join(x = ts_expanded,
                           y = ts)
  
  ts_xts <- as.xts(ts_expanded[,-1],
                   order.by = ts_expanded$date)
  
  # Interpolate n and n_traps
  ts_xts$n <- na.spline(ts_xts$n)
  ts_xts$n_traps <- na.spline(ts_xts$n_traps)
  
  # Write over ts
  ts <- data.frame(ts_xts); rm(ts_xts)
  ts$date <- as.Date(row.names(ts))
  
}

# Get weather for period 10-21 days prior
vars <- c('Mean_TemperatureF',
          'Min_TemperatureF',
          'Max_TemperatureF',
          'PrecipitationIn',
          'Mean_Wind_SpeedMPH')

for (var in vars){
  # Create new variable
  column_name <- paste0(var, '_10_21')
  ts[,column_name] <- NA
  
  for (i in 1:nrow(ts)){
    date <- ts$date[i]
    vals <- weather[which(weather$date >= (date - 21) &
                           weather$date <= date - 10),var]
    val <- mean(vals, na.rm = TRUE)
    ts[i,column_name] <- val
  }
}

# Make a mosquitoes per trap var
ts$y <- ts$n / ts$n_traps

# Make a previous mosquitoes per trap for previous traps
vec <- 10:50
for (i in 1:length(vec)){
  ts[,paste0('y_minus_', as.character(vec[i]))] <- 
    c(rep(NA, i),
      ts$y[1:(nrow(ts) - i)])
}

#####
# MAKE TRAINING AND TESTING DATASETS
#####
ts$year <- as.numeric(format(ts$date, '%Y'))
ts$day_number <- as.numeric(format(ts$date, '%j'))
ts <- data.frame(ts)
train <- ts[which(ts$year <= 2013),]
test <- ts[which(ts$year >= 2014),]

#####
# DEFINE MODEL FORMULA
#####
y <- 'y'
x <- paste('n_traps',
           #paste0('y_minus_', vec),
           'Mean_TemperatureF_10_21',
           'Min_TemperatureF_10_21',
           'Max_TemperatureF_10_21',
           'PrecipitationIn_10_21',
           'Mean_Wind_SpeedMPH_10_21',
           #'day_number', 
           sep = ' + ') 
model_formula <- as.formula(paste0(y, ' ~ ', x))

###############
# Linear regression
fit_linear <- lm(model_formula,
                 data = train)

###############
# Random forest
fit_rf <- randomForest(model_formula,
                    data = train,
                    na.action = na.omit)

###############
# Generate predctions
test$predicted_linear <- predict(fit_linear, test)
test$predicted_rf <- predict(fit_rf, test)

#####
# EXPLORE PREDICTIONS
#####

# Observed
plot(test$date,
     test$y,
     type = 'l',
     lwd = 2)

# Predicted from linear model
lines(test$date, test$predicted_linear,
      col = 'darkgreen')

# Predicted from random forest
lines(test$date, test$predicted_rf,
      col = 'darkred')

# Compare rmse
rmse <- function(observed, predicted){
  error <- observed - predicted
  sqrt(mean(error^2, na.rm = TRUE))
}

# linear
rmse(observed = test$y, predicted = test$predicted_linear)
rmse(observed = test$y, predicted = test$predicted_rf)
# just use mean of past
rmse(observed = test$y, predicted = rep(mean(train$y), nrow(test)))
