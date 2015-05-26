# YONI TIME!
source('read_in.R')

# Number of mosquitoes observed at previous collection
ts$previous_mosquitoes <- 
  c(NA, ts$n[1:(nrow(ts) - 1)])


# Join weather and ts
ts <- inner_join(x = ts, 
                 y = weather[,c('date', 
                                'Mean_TemperatureF', 
                                'PrecipitationIn',
                                'Min_TemperatureF', 
                                'Max_TemperatureF')], 
                 by = 'date')

# Make a mosquitoes per trap var
ts$y <- ts$n / ts$n_traps

# Make a previous mosquitoes per trap
ts$previous_y <- 
  c(NA, ts$y[1:(nrow(ts) - 1)])

# Make a training dataset
train <- ts[-1,c('y', 'Mean_TemperatureF', 'PrecipitationIn',
                 'Min_TemperatureF', 'Max_TemperatureF', 'previous_y')]

###############
# Random forest
fit <- randomForest(y ~ previous_y,
                    data = train)

summary(fit)
plot(predict(fit), 
     train$y,
     xlab = 'Predicted',
     ylab = 'Observed')

fake <- data.frame(previous_y = 1:2000)
plot(fake$previous_y, predict(fit, fake),)

###############
# Linear model
fit <- lm(y ~ previous_y, 
          data = train)

summary(fit)
plot(predict(fit), 
     train$y,
     xlab = 'Predicted',
     ylab = 'Observed')

fake <- data.frame(previous_y = 1:2000)
plot(fake$previous_y, predict(fit, fake),)


# Previous days max/min/avg temp and precipitation

#############################################################
#############################################################
#############################################################

#####
# CREATE AN AUGMENTED WEATHER DATAFRAME
# WITH TS FEATURES
#####
weather_augmented <- weather

# Create dataframe for the next two weeks
next_two_weeks <- as.data.frame(matrix(NA, 
                                       ncol = ncol(weather),
                                       nrow = 14))
colnames(next_two_weeks) <- colnames(weather)
next_two_weeks$Date <- as.Date(seq(Sys.Date(), (Sys.Date()+13), 1))

# Combine with weather_augmented
weather_augmented <- rbind(weather_augmented, next_two_weeks)


for (column in colnames(weather)[2:4]){
  # looking only at the period 14 to 30 days before
  for (day in 14:30){
    weather_augmented[,paste0(column, '_minus_', day)] <- NA
  }
}

for (i in 1:nrow(weather_augmented)){
  for (column in colnames(weather)[2:4]){
    for (day in 14:30){
      if(i > 30){
        date <- weather_augmented$Date[i]
        val <- weather_augmented[(i-day),column]
        weather_augmented[i,paste0(column, '_minus_', day)] <- val
      }
    }
  }
}

# Remove the non-historic observations
weather_augmented <- weather_augmented[,!colnames(weather_augmented) %in% c('Max_TemperatureF',  
                                                                            'Min_TemperatureF',    
                                                                            'PrecipitationIn')]


#####
# CREATE DATAFRAME WITH GENERATED FEATURES FOR MODEL TRAINING
#####
train <- left_join(ts, weather_augmented, by = c('date' = 'Date'))

# Remove date so that it's not used in modeling
train_date <- train$date
train$date <- NULL

#####
# TRAIN MODEL
#####
fit <- randomForest(tot ~ . ,
                    data = train)

# Also get a predictions matrix
obs_mat <- predict(fit,
                   train,
                   predict.all = TRUE)
# And error matrix
temp <- as.matrix(obs_mat$individual)
error_mat <- apply(temp, 2, function(x){
  x - train$tot
})
rm(temp)

# Predict on train
train$predicted <- predict(fit, train)

plot(train_date, train$tot, type = 'l')
lines(train_date, train$predicted, col = 'red')

#####
# PREDICT ON CURRENT
#####

current <- weather_augmented[which(weather_augmented$Date >= (Sys.Date() - 30)),]
current$n_traps <- 10

current$predicted <- predict(fit, current)

# Also get a matrix of each tree's results
pred_mat <- predict(fit, current, predict.all = TRUE)

# Add error terms from the observed data:
temp <- as.matrix(pred_mat$individual)
for (j in 1:ncol(temp)) {
  errors <- sample(error_mat[, j], nrow(temp),
                   replace = TRUE)
  # temp[,j] <- temp[,j] + errors # ! too big - currently not adding errors
  temp[,j] <- temp[,j] + errors
  
}
pred_mat_with_error <- temp
rm(temp)

# Now that we've introduced fundamental uncertainy,
# we can draw up individual-level predicition intervals,

# Get point estimate
current$predicted <- pred_mat$aggregate

# Get confidence bounds
current$lwr <- apply(pred_mat_with_error, 1, 
                     function(x){
                       quantile(x, probs = 0.025)
                     })
current$upr <- apply(pred_mat_with_error, 1,
                     function(x) {
                       quantile(x, probs = 0.975)
                     })

plot(current$Date,
     current$predicted,
     type = 'l')


#####
# COMBINE TRAIN AND CURRENT
#####
train_small <- train[,'tot']
train_small$Date <- train_date
train_small$observed <- TRUE

current_small <- current[,c('predicted', 'Date')]
current_small$observed <- TRUE
names(current_small)[1] <- 'tot'

combined <- rbind(train_small, current_small)

plot(combined$Date, combined$tot, type = 'n')

lines(train_small$Date, train_small$tot)
lines(current_small$Date, current_small$tot, col = 'red')


#####
# PREDICT ON WEATHER AUGMENTED
#####
weather_augmented <- left_join(weather_augmented, train)


# Get number of traps
weather_augmented$n_traps[1] <- 6
for (i in 1:nrow(weather_augmented)){
  while(is.na(weather_augmented$n_traps[i])){
    weather_augmented$n_traps[i] <- weather_augmented$n_traps[i-1]
  }
}

weather_augmented$predicted <- predict(fit, weather_augmented)

plot(weather_augmented$Date, weather_augmented$predicted, type = 'l',
     col = adjustcolor('black', alpha.f = 0.3),
     ylim = c(0, 8000),
     xlim = as.Date(c('2012-05-01', '2015-05-30')),
     xlab = 'Date',
     ylab = 'Number of trapped mosquitos',
     cex.axis = 0.6)
lines(train_small$Date, train_small$tot,
      col = adjustcolor('darkred', alpha.f = 0.6))
abline(v = Sys.Date(), lty = 3)

legend('topright',
       lty = c(1, 1, 3),
       col = c(adjustcolor('black', alpha.f = 0.3),
               adjustcolor('darkred', alpha.f = 0.6),
               'black'),
       legend = c('Predicted',
                  'Observed',
                  'Today'),
       cex = 0.8)
