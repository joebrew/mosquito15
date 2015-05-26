library(gdata)
library(xlsx)

#####
# Set working directory to the original spreadsheets
#####
setwd('original_spreadsheets/')

#####
# Read in each year's data
#####
year <- as.character(2008)
setwd(year)
april08 <- read.xlsx("2008 April Trap Data Sheet.xls", 1)

