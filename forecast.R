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
train <- left_join(ts, weather_augmented, by = 'date')

# Remove date so that it's not used in modeling
train_date <- train$date
train$date <- NULL

# Remove any NA columns
train_names <- names(train)
bads <- rep(FALSE, length(train_names))
for (j in 1:ncol(train)){
  column <- train[,train_names[j]]
  if(length(which(is.na(column))) > 0){
    bads[j] <- TRUE
  }
}
train <- train[,!bads]

# Fix weird joined column issues
train <- train[,names(train)[!grepl('[.]x', names(train))]] #get rid of .x columns
names(train) <- gsub('[.]y', '', names(train)) #rename .y columns
train <- train[,!duplicated(names(train))] # remove any duplicates

# Exclude non-historical observations
train <- train[,c('n_traps', 'n', 
                  names(train)[grepl('_minus_', names(train))])]

#####
# TRAIN MODEL
#####
fit <- randomForest(n ~ . ,
                    data = train,
                    ntree = 5000,
                    na.action = 'na.omit')

# Also get a predictions matrix
obs_mat <- predict(fit,
                   train,
                   predict.all = TRUE)
# And error matrix
temp <- as.matrix(obs_mat$individual)
error_mat <- apply(temp, 2, function(x){
  x - train$n
})
rm(temp)

# Predict on train
train$predicted <- predict(fit)

plot(train_date, train$n, type = 'l')
lines(train_date, train$predicted, col = 'red')

#####
# PREDICT ON CURRENT
#####

current <- weather_augmented[which(weather_augmented$date >= (Sys.Date() - 30)),]
current$n_traps <- 10

current$predicted <- predict(fit, current)

# Also get a matrix of each tree's results
pred_mat <- predict(fit, current, predict.all = TRUE)

# Add error terms from the observed data:
temp <- as.matrix(pred_mat$individual)
# for (j in 1:ncol(temp)) {
#   errors <- sample(error_mat[, j], nrow(temp),
#                    replace = TRUE)
#   # temp[,j] <- temp[,j] + errors # ! too big - currently not adding errors
#   temp[,j] <- temp[,j] + errors
#   
# }
pred_mat_with_error <- temp
rm(temp)

# Now that we've introduced fundamental uncertainy,
# we can draw up individual-level predicition intervals,

# Get point estimate
current$predicted <- pred_mat$aggregate

# Get confidence bounds
current$lwr <- apply(pred_mat_with_error, 1, 
                     function(x){
#                        quantile(x, probs = 0.025)
                       quantile(x, probs = 0.1)
                       
                     })
current$upr <- apply(pred_mat_with_error, 1,
                     function(x) {
#                        quantile(x, probs = 0.975)
                       quantile(x, probs = 0.9)
                       
                     })

plot(current$date,
     current$predicted,
     type = 'l')


#####
# COMBINE TRAIN AND CURRENT
#####
train_small <- train[,'n']
train_small$date <- train_date
train_small$observed <- TRUE

current_small <- current[,c('predicted', 'date')]
current_small$observed <- TRUE
names(current_small)[1] <- 'n'

combined <- rbind(train_small, current_small)

plot(combined$date, combined$n, type = 'n')

lines(train_small$date, train_small$n)
lines(current_small$date, current_small$n, col = 'red')

#####
# SAVE CHECKPOINT
#####
save.image(paste0(proj_root, '/checkpoint.RData'))
