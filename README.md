# Time Series Forecast
Uses time series decomposition to find trends, and Amira to forecast

############################### Time Series Decomp & Forecast #########################################

# Use trended date to find seasonal trends and forecast

# Project Name:

# Data source: 

############################################################################################

# begin by setting the working directory for the files
setwd("C:/Users/cdobbe/Documents/R")
outputDir = 'C:/Users/cdobbe/Documents/R/output_csv_files/'

#Load the packages and libraries, or d/l any using install.packages("???")
library(readr)
library(stringr)
library(ggplot2)
library(ggfortify)
library(forecast)
library(tseries)
library(lubridate)
library(dplyr)
library(magrittr)

# Begin by loading in the dataset using the appropriate method

#data1 <- file.choose() # Allows for manual selection from folder location, paired with below
#data <- read_csv(data1)  
data1 <- read_csv("file_name.csv") #loads in the CSV to a tibble, easier to work with
#data1 <- read.csv("file_name.tsv", sep = "\t", header = TRUE)
#data1 <- read.xls("file_name.xlsx")
#data1 <- XLGetRange(sheet = "sheet1", range = "A1:B21", header = TRUE) #only used when Excel workbook is open

# Name the output forecast file
today <- Sys.Date()                  #this will append todays date to the file name you create below
outputFN <- "Name_of_the_File"                   #change the file name as needed
  outputFN <- paste( paste( outputFN, today, sep = "_"), ".csv", sep = '')

# Open the data view
View(data1)

### Basic summary functions to understand the data
names(data1)
head(data1)
str(data1)
summary(data1)
colnames(data1) <- c("month", "sessions")            #change column names for simplicity

### For GA date values, some manipulation is required
year <- str_sub(data1$month, start = 1, end = 4)
month <- str_sub(data1$month, start = 5, end = 6)
#day <- str_sub(data1$month, start = 7, end = 8)              #update when using full GA date
date <- str_c("01", "-", month, "-", year)                    #if full date is used, swap "01" for "day"

data2 <- data.frame(date, data1$sessions)
head(data2)                                                  #quick check that date changes were made correctly

# some manipulation of the new data frame
colnames(data2) <- c("date", "sessions")
data2 <- na.omit(data2)  #remove any rows with NA values
sum(is.na(data2))  #Check: should return a 0 if all NA values are removed

### Convert data into time series

data2$date <- dmy(data2$date)
attach(data2)
str(data2)                  #check that it has converted to date format
datats <- ts(data2$sessions, c(2014,1), c(2017,9), 12)                 #this is for monthly data, change dates and '12' if needed
str(datats)                 #check that it is time-series data now

datats  #view the data set, should be set up in table format

# check the frequency of the time series
frequency(datats)                 #should return a 12 for monthly
cycle(datats)                     #shows a calendar view of the time series, aligning with date frequency
summary(datats)

### plot the data to show the trended view using a line graph 
autoplot(datats) + labs(x = "Date", y = "Sessions", title = "Name Of the Graph")

# now a boxplot to look for seasonal effects
boxplot(datats~cycle(datats), xlab = "Date", ylab = "Sessions", main = "Name of the Graph")

### Now we will decompose the time series to look for seasonal trends
decompose_datats <- decompose(datats, "multiplicative")
autoplot(decompose_datats)

decompose_datats

### Test stationarity of the Time Series 
# A stationary time series has the conditions that the mean, variance, & co-variance are not functions of time
# The following code tests for this condition in 2 ways
# We operate under the Null hypothesis that the time series is non-stationary

adf.test(datats)
# We look for a small p-value to proivde evidence against the null hypothesis
# If p<0.05 then we reject the null, and accept the alt. that the time series is stationary

# The next method uses autocorrelation, which plots the correlatoin between a series and its lags (prev. observations)
# with a 95% confidence interval (shown in blue in the graph)

autoplot(acf(datats, plot=FALSE))+ labs(title = "Correlogram of Monthly Sessions")
# if autocorrelation crosses the blue line, then the specific lag is signif. correlation with current series

# Review the random times series for any missing values
decompose_datats$random

# Autoplot the random time series which exclude the NA values
autoplot(acf(na.remove(decompose_datats$random), plot = FALSE)) + labs(title = "Correlogram of Monthly sessions")
# We want to look for the residuals centered around 0


### Fit a time series model ###
# We'll use an ARIMA model to fit the best model and coefficients, given the default parameters including seasonilty = TRUE
# Full disclouse I know little about the AMIRA model, so I do not understand what happens in the next few lines of code 

arima_datats <- auto.arima(datats)
arima_datats

ggtsdiag(arima_datats)
# I'm not really sure what all this means????

### Calcualte Forecasts

forecast_datats <- forecast(arima_datats, level = c(95), h = 12)            #c is confidence, h = forecast period
autoplot(forecast_datats)              #plot the trended with forecast cone
forecast_datats               #print results if you want to view the data and see the actual forecast with hi/low values

as.data.frame(forecast_datats)  #convert the forecast to a data.frame so it can be exported to CSV
write.csv(forecast_datats, paste(outputDir, outputFN, sep= ""))

