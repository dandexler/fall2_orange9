# September 08, 2019
# HW 4 Time Series 
# Daily Mean PM2.5 Concentration - Particulate Matter smaller than 2.5 micrometers Forecast

install.packages(c("caschrono", "TSA", "imputeTS"))
library(forecast)
library(tsa)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(imputeTS)
library(xts)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tseries)

# Set the working directory
setwd("/Users/CathyTran/Documents/Fall I 2019/Time Series/Part I/")

# Read CSV into R
data <- read.csv(file="HW2_PM_2_5_Raleigh2.csv", header=TRUE, sep=",")

# set strings as factors to false
options(stringsAsFactors = FALSE)

# Convert character string to date format
data$Date <- as.Date(data$Date, format="%m/%d/%Y")

# Get the range of dates covered
DateRange <- seq(min(data$Date), max(data$Date), by = 1)

# Calculate missing values - 353
length(DateRange[!DateRange %in% data$Date])

# Create a z object to check for missing value after aggregated month
z <- zoo(data$Daily.Mean.PM2.5.Concentration, data$Date)
monthavg <- aggregate(z, as.yearmon, mean)

# Create Training (1300 obs 18 vars) & Validation (173 obs 18 vars) Data Set
data.train <- data[data$Date >= "2014-01-01" & data$Date <= "2018-06-30",]
data.valid <- data[data$Date >= "2018-07-01" & data$Date <= "2018-12-31",]

# Aggregate by month using Daily Mean PM2.5 Concentration
months.train <- data.train %>%
  group_by(year=year(Date), month=month(Date)) %>%
  summarise(mean=mean(Daily.Mean.PM2.5.Concentration))

months.valid <- data.valid %>%
  group_by(year=year(Date), month=month(Date)) %>%
  summarise(mean=mean(Daily.Mean.PM2.5.Concentration))

# Time Series Object#
ts.months.train <- ts(months.train$mean, start = 2014, frequency =12)

ts.months.valid <- ts(months.valid$mean, start = 2018, frequency = 12)

# Time Series Decomposition ...STL#
decomp_stl <- stl(ts.months.train, s.window = 7)
plot(decomp_stl)

# Seems like seasonality pattern exists

# Checking to see if seasonal differences is needed
# Returned 0, which means we don't need to take a seasonal difference
nsdiffs(ts.months.train,test='ch')

# Use dummy variables

#  Factor month into 12 levels
month.tx=factor(months.train$month)
# A matrix with dummy variables. First column is the intercept value of 1
reg.tx=model.matrix(~month.tx[1:54])
reg.tx=reg.tx[,-1]  # Remove intercept

#### Using dummy variables
# Fitting ARIMA model with dummy variables
# xreg - a numerical vector or matrix of external regressors, 
# which must have the same number of rows as y. It should not be a data frame.
tx.seas=Arima(ts.months.train,xreg=reg.tx)
summary(tx.seas)

# Plot the residuals plot
plot(tx.seas$residuals[1:54], xlab='Year 2014-2018',ylab='Residuals',main='Residuals Plot',type='l')

# Returns Residual Plot, ACF, and PACF
tsdisplay(residuals(tx.seas))

# The residual plot suggests that we might have a trend

# Check the stationarity ADF Test going back up to 3 lags
# We have stationarity about the trend!
# H0: ø=1 random walk
# HA: |ø| <1 stationarity about the trend
adf.test(ts.months.train, alternative = "stationary", k = 0) # p-value = 0.01 
adf.test(ts.months.train, alternative = "stationary", k = 1) # p-value = 0.01
adf.test(ts.months.train, alternative = "stationary", k = 2) # p-value = 0.0171

# Fit Linear Regression
arima.trend=Arima(ts.months.train, xreg=reg.tx,order=c(0,0,0))

# Plot the residuals plot
plot(arima.trend$residuals[1:54], xlab='Number of Observations',ylab='Residuals',main='Residuals Plot',type='l')

ts.resid <- ts(arima.trend$residuals[1:54], start = 2014, frequency =12)

# Why does my residuals plot before and after fitting the linear trend the same?

# check ADF test after fitting the trend with residuals values
# Stationary about the zero mean bc the residuals plot look like it's centered around the zero mean
adf.test(ts.resid, alternative = "stationary", k = 0) # p-value = 0.01 


# Ljung-Box Test No MA or AR term#
Acf(ts.resid, lag=24,main = " Autocorrelation Plot")$acf # suggests MA 2
Pacf(ts.resid, lag=24, main = "Partial Correlation Plot")$acf # suggests AR (1)

# Let's try 1 AR term, 2 MA terms

arima.trend1=Arima(ts.months.train, xreg=reg.tx, order=c(1,0,2))

# Ljung-Box Test 1 AR 2 MA terms #
Acf(arima.trend1$residuals, lag=24,main = " ACF ARMA(1,2)")$acf
Pacf(arima.trend1$residuals, lag=24, main = "PACF ARMA(1,2)")$acf

# Note, at lag 12, both ACF and PACF plots are outside of the confidence intervals

# Pull out the p-values to be used for Ljung Test
# What should be the df?
White.LB <- rep(NA, 24)
for(i in 1:24){
  White.LB[i] <- Box.test(arima.trend1$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

# H0: White Noise, No Autocorrelation
# HA: One or more autocorrelation up to lag m are not 0

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# Conclusion: We have white noise

# Automatic AR, MA term Selection Technique 
# uses spline as the imputation technique
pm<-ts.months.train%>% na_interpolation(option = "spline")
auto.arima(pm) # ARIMA(0,1,0) suggests taking a difference
eacf(ts.months.train,ar.max=24,ma.max=24)

auto.arima(ts.months.train) # ARIMA(0,1,0) suggests taking a difference

# Forecast
# Should I repeat these steps before forecasting? 
#  Factor month into 12 levels
month.tx=factor(months.valid$month)
# A matrix with dummy variables. First column is the intercept value of 1
reg.tx=model.matrix(~month.tx[1:54])
reg.tx=reg.tx[,-1]  # Remove intercept

# No data in the table
new.reg.tx=model.matrix(~month.tx[55:60])

# Forecast 6 years a head
# What did you set xreg= to ?
test.forecast=forecast(arima.trend1, xreg=reg.tx,h = 6)
plot(forecast(arima.trend1, h = 6))

# Compare the predicted values to the validation data set
compare=months.valid$mean - test.forecast$mean
plot(compare)

# Forecast n.ahead = 6 forecast 6 periods
xnew=seq(55,60)
arima.forecast <- predict(arima.trend1, n.ahead=6, newxreg=xnew)

# Compare these the predicted values to the validation data set
months.valid$mean - arima.forecast$pred

# For later Calculate MAE and MAPE
# On Validation Data Set
# MAE         MAPE
# 1.888392    0.1810493

x = unclass(ts.months.valid)
y = unclass(test.results$mean)

# compare how the model that I built which contains the predicted values against the validation data set
error=x-y
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(x))
print(MAE)
print(MAPE)


