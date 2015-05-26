library(plyr)
library(dplyr)
library(tidyr)
library(weatherData)
library(randomForest)

#####
# DEFINE WORKING DIRECTORIES
#####
proj_root <- getwd()
trap_dir <- paste0(proj_root, "/trap_data")
weather_dir <- paste0(proj_root, "/weather_data")

setwd(trap_dir)

####
# READ IN ALL DATA INTO LIST
####
csvs = list.files(pattern="*.csv")
dfs <- list()

for (i in 1:length(csvs)){
  # Get date
  date <- gsub('.csv', '', csvs[i])
  # Read in, skipping first line
  temp <- read.csv(csvs[i], 
                   skip = 1,
                   stringsAsFactors = FALSE)
  # Get rid of blank columns
  temp <- data.frame(temp[,!names(temp) %in% c('X', paste0('X.', 1:10))])
  # Clean up column names
  temp
  # Make date column
  temp$date <- date
  # Make number of traps column
  temp$n_traps <- length(which(grepl('X', names(temp))))
  # Get dataframe into list  
  dfs[[i]] <- temp
}

####
#CBIND ALL ELEMENTS OF THE MYFILES LIST INTO ONE DF
####
raw_data <- rbind.fill(dfs)

# Remove unecessary garbage
rm(temp, csvs, date, dfs, i)

# Clean up column names in raw_data
names(raw_data) <- gsub('X', 'trap', names(raw_data))
names(raw_data)[which(!grepl('trap', names(raw_data)))] <- 
  c('mosquito', 'total', 'code', 'date')

# Arrange column names
raw_data <- raw_data[,sort(names(raw_data))]

####
# FIX SOME CODES
####

# Others = 100
raw_data$code[which(grepl("other", 
                         tolower(raw_data$mosquito)))] <-  100

# Canadensis = 200
raw_data$code[which(grepl('canad',
                          tolower(raw_data$mosquito)))] <- 200

#####
# CLEAN UP
#####

# Populate NAs with 0s if the trap existed at the time
for (i in 1:nrow(raw_data)){
  n_traps <- raw_data$n_traps[i]
    columns <- paste0('trap', 1:n_traps)
    for (j in 1:length(columns)){
      val <- raw_data[i, columns[j]]
      if(is.na(val)){
        raw_data[i, columns[j]] <- 0
    }
  }
}

# Clean up format a bit
raw_data$mosquito <- NULL
raw_data$total <- NULL

# "Gather data" by a new variable named "trap", 
#create a new variable named n (value)
# and gather variables trap1 through trap9
raw_data <- raw_data %>%
  gather(key = trap, 
         value =  n, 
         trap1:trap9)

# Clean up trap column
raw_data$trap <- as.numeric(gsub('trap', '', raw_data$trap))

# Get mosquito type
diseases <- read.csv(paste0(proj_root, '/diseases/id.csv'))
raw_data <- left_join(x = raw_data,
                y = diseases[,c('code', 'name')])

# Format date
raw_data$date <- as.Date(raw_data$date)

#####
# MAKE AN AGGREGATED TIME SERIES DATAFRAME
#####
ts <- raw_data %>%
  group_by(date) %>%
  summarise(n = sum(n, na.rm = TRUE),
            n_traps = mean(n_traps))

# Remove some junk
rm(columns, i, j, n_traps, val)
############

#####
# GET RAINFALL DATA
#####

setwd(weather_dir)
if(file.exists('weather.csv')){
  weather <- read.csv('weather.csv')
} else {
  
  weather_list <- list()
  min_year <- as.numeric(format(min(ts$date), '%Y'))
  max_year <- as.numeric(format(Sys.Date(), '%Y'))
  year_seq <- min_year:max_year
  for (i in 1:length(year_seq) ){
    
    # Define end dates
    if(year_seq[i] == max_year){
      end <-  Sys.Date() - 1
    } else {
      end <-  paste0(year_seq[i], '-12-31')
    }
    
    # Get data
    rf <- getSummarizedWeather("GNV", 
                               start_date = paste0(year_seq[i], '-01-01'), 
                               end_date = end,
                               opt_all_columns = TRUE)
    
    rf$date <- as.character(rf$Date)
    rf$Date <- NULL
    # Put into list
    weather_list[[i]] <- rf
    print(year_seq[i])
  }
  
  # Combine all weather into one dataframe
  weather <- rbind.fill(weather_list)
  rm(min_year, max_year, year_seq, i, weather_list, rf)
  
  # Write csv
  setwd(weather_dir)
  write.csv(weather, 'weather.csv', row.names = FALSE)
  weather <- read.csv('weather.csv')
}

# Format date
weather$date <- as.Date(weather$date)


#####
# GET MORE RECENT WEATHER IF NEEDED
#####
if(max(weather$date) < (Sys.Date() - 1)){
  new_weather <- getSummarizedWeather("GNV", 
                                      start_date = max(weather$date) +1, 
                                      end_date = Sys.Date() - 1,
                                      opt_all_columns = TRUE)
  new_weather$date <- as.Date(new_weather$Date)
  new_weather$Date <- NULL
  weather <- rbind.fill(weather, new_weather)
  
  # Clean up
  # Format date
  weather$Date <- as.Date(weather$Date)
  
  
  # Write csv
  setwd(weather_dir)
  write.csv(weather, 'weather.csv', row.names = FALSE)
  weather <- read.csv('weather.csv')
}


# Clean up weather a bit more

# Fix precipitation 
weather$PrecipitationIn <- suppressWarnings(
  as.numeric(as.character(weather$PrecipitationIn)))
weather$PrecipitationIn[which(is.na(weather$PrecipitationIn))] <- 0

# At this point you've got 
#### diseases: a simple dictionary of which mosquitoes carry which diseases
#### raw_data: number of mosquitoes by date, mosquito type, and trap
#### ts: time series of total mosquitoes (independent of type/trap) for each date
#### weather: daily weather observations

# Pretty plot
par(mfrow = c(2,2))
plot(ts$date, ts$n / ts$n_traps, 
     xlab = 'Date',
     ylab = 'Mosquitoes per trap',
     typ = 'l',
     col = adjustcolor('darkgreen', alpha.f = 0.6))
title(main = 'Skeeters')

plot(weather$date, weather$Max_TemperatureF,
     xlab = 'Date',
     ylab = 'Temperature (farenheit)',
     ylim = c(0, 110),
     type = 'l',
     col = adjustcolor('darkred', alpha.f = 0.6))
lines(weather$date, weather$Min_TemperatureF,
      col = adjustcolor('darkblue', alpha.f = 0.6))
legend('bottom',
       col = adjustcolor(c('darkred', 'darkblue'), alpha.f = 0.6),
       legend = c('Maximum', 'Minimum'), 
       lty = 1,
       bty = 'n',
       cex = 0.7)
title(main = 'Temperature')

weather$week <- as.numeric(format(weather$date, '%U'))
temp <- weather %>%
  group_by(week) %>%
  summarise(rain = sum(PrecipitationIn),
            humidity = mean(Mean_Humidity))
plot(temp$week, temp$rain,
     xlab = 'Week',
     ylab = 'Inches of rain',
     main = 'Precipitation',
     type = 'l',
     col = adjustcolor('darkorange', alpha.f = 0.6),
     ylim = c(0, 30))

plot(temp$week, temp$humidity,
     xlab = 'Week',
     ylab = 'Mean humidity',
     main = 'Humidity',
     type = 'l',
     col = adjustcolor('purple', alpha.f = 0.6),
     ylim = c(60, 100))
weather$week <- NULL


par(mfrow = c(1,1))
