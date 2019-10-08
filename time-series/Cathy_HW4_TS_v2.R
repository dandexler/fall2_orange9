# Cathy Tran
# September 08, 2019
# HW 4 Time Series 
# Daily Mean PM2.5 Concentration - Particulate Matter smaller than 2.5 micrometers Forecast

# Install necessary packages and libraries
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

# don't convert character vectors to factors
options(stringsAsFactors = FALSE)

# Convert character string to date format
data$Date <- as.Date(data$Date, format="%m/%d/%Y")

# Get the range of dates covered
DateRange <- seq(min(data$Date), max(data$Date), by = 1)

# Calculate missing values in Date col- 353
length(DateRange[!DateRange %in% data$Date])

# Create a z object that has only the Date (YYYY-MM-DD) for each date from 2014-01-01 to 2018-12-31 
# and Daily Mean PM2.5
z <- zoo(data$Daily.Mean.PM2.5.Concentration, data$Date)

# Aggregate from Jan 2014 to Dec 2018 by month
# Value of Daily Mean PM2.5 is the mean
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

months.full <- data %>%
  group_by(year=year(Date), month=month(Date)) %>%
  summarise(mean=mean(Daily.Mean.PM2.5.Concentration))

# Time Series Object for Training and Validation#
ts.months.train <- ts(months.train$mean, start = 2014, frequency =12)

ts.months.valid <- ts(months.valid$mean, start = 2018, frequency = 12)

# Time Series Decomposition ...STL#
decomp_stl <- stl(ts.months.train, s.window = 7)
plot(decomp_stl)

# Saw seasonality pattern and trend

# Checking to see if seasonal differences is needed
# Returned 0, which means we don't need to take a seasonal difference
# Used training dataset as ts obj
nsdiffs(ts.months.train,test='ch')
nsdiffs(ts.months.train)

############################
#   Fit dummy variables   #
############################

#  Factor month into 12 levels
month=factor(months.full$month)

# A matrix of dummy variable from the full dataset obs 1-60
dummy_matrix=model.matrix(~month)

# Remove the intercept
dummy_matrix = dummy_matrix[,-1] 

training=dummy_matrix[1:54,]

# Fitting ARIMA model with dummy variables
# xreg - a numerical vector or matrix of external regressors, 
# which must have the same number of rows as y. It should not be a data frame.
tx.seas=Arima(ts.months.train,xreg=training)
summary(tx.seas)

# New variable for tx.seas$residuals
ts.seas.resid <- tx.seas$residuals

# Plot the residuals plot
plot(tx.seas$residuals, xlab='Year 2014-2018',ylab='Residuals',main='Residuals Plot',type='l')

# Another way of getting the residuals plot, PACF and ACF
# tsdisplay(residuals(tx.seas))

# The residual plot suggests that we might have a trend

################################################################
# Check Deterministic Trend or Stochastic (take a difference)  #
################################################################

# Check the stationarity ADF Test going back up to 3 lags
# We have stationarity about the trend!
# H0: ø=1 random walk
# HA: |ø| <1 stationarity about the trend
adf.test(ts.seas.resid, alternative = "stationary", k = 0) # p-value = 0.01 
adf.test(ts.seas.resid, alternative = "stationary", k = 1) # p-value = 0.02055
adf.test(ts.seas.resid, alternative = "stationary", k = 2) # p-value = 0.02688

# Fit Linear Regression
x <- seq(1,54)
arima.trend=Arima(ts.seas.resid, xreg=x, order=c(0,0,0))
summary(arima.trend)

# Plot the residuals plot
plot(arima.trend$residuals, xlab='Number of Observations',ylab='Residuals',main='Residuals Plot',type='l')

# New residual variable
ts.resid <- arima.trend$residuals

# Check ADF test after fitting the trend with residuals values
# Stationary about the zero mean bc the residuals plot looks like it's centered around the zero mean
adf.test(ts.resid, alternative = "stationary", k = 0) # p-value = 0.01 

#######################################
# Determining AR and MA terms         #
#######################################

# Regular AR and MA terms Characteristics
# AR(1)
# 1. ACF decreases exponentially
# 2. PACF  has a significant spike at the 1st lag, followed by nothing after
# 3. IACF has a significant spike at the 1st lag, followed by nothing after
 
# AR(q)
# 1.	ACF variety of patterns
# 2.	PACF has significant spike up to p lags, nothing after
# 3.	IACF has significant spike up to p lags, nothing after

# MA(1) Characteristics
# 1.	ACF significant spike at 1st lag, nothing after
# 2.	As # of lags inc, PACF  decreases exponentially
# 3.	As # of lags inc, IACF  decreases exponentially
 
# MA(q) model
# 1.	ACF spikes at the significant lags up to lag q, nothing after
# 2.	PACF variety of patterns
# 3.	IACF variety of patterns

# Ljung-Box Test No MA or AR term #
# spike at lag 1
# lag = 60 - going 5 seasons back
# not a pureexponential decay in the seasonal lags of the ACF;

Acf(ts.resid, lag=60,main = " Autocorrelation Plot")$acf 

# spikes at lag 1 and lag 12 in the PACF, which suggest seasonal AR 1?

Pacf(ts.resid, lag=60, main = "Partial Correlation Plot")$acf 

# Check white noise No MA or AR term 
# Pull out p-values
White.LB <- rep(NA, 60)
for(i in 1:60){
  White.LB[i] <- Box.test(ts.resid, lag = i, type = "Ljung", fitdf = 0)$p.value
}

# H0: White Noise, No Autocorrelation
# HA: One or more autocorrelation up to lag m are not 0

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# We don't have white noise!
# There is still some correlation left on the left so I need to model that

# Set my new xreg so the Arima model can regress on both the dummary variable and the trend component
matrix <- cbind(training,x)

# Automatic AR, MA term Selection Technique 
auto.arima(ts.months.train, xreg = matrix) # ARIMA(1,0,0)(1,0,0)[12] errors 

#######################################
# Model 1                             # 
# ARIMA(1,0,0)(1,0,0)[12] errors      #
#######################################

# Create ARIMA(1,0,0)(1,0,0)[12] errors model
arima.trend.season=Arima(ts.months.train, xreg=matrix,order=c(1,0,0), seasonal=c(1,0,0))
summary(arima.trend.season) 
# MAPE on validation . On average, my model is off by __ %"

Acf(arima.trend.season$residuals, lag=50,main = " ARIMA(1,0,0)(1,0,0)[12] errors ")$acf
Pacf(arima.trend.season$residuals, lag=50, main = "ARIMA(1,0,0)(1,0,0)[12] errors ")$acf

# Pull out the p-values to be used for Ljung Test
White.LB <- rep(NA, 24)
for(i in 1:24){
  White.LB[i] <- Box.test(arima.trend.season$residuals, lag = i, type = "Ljung", fitdf = 2)$p.value
}

# H0: White Noise, No Autocorrelation
# HA: One or more autocorrelation up to lag m are not 0

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# Model 1: White Noise achieved with ARIMA(1,0,0)(1,0,0)[12] errors

#######################################
# Model 2                             # 
# ARIMA(1,0,0)(0,0,1)[12] errors      #
#######################################

# Model 2 AR(1), seasonal MA(1), dummy variable & linear matrix
model2=Arima(ts.months.train, xreg=matrix,order=c(1,0,0), seasonal=c(0,0,1))

Acf(model2$residuals, lag=48,main = " ARIMA(1,0,0)(1,0,0)[12] errors ")$acf
Pacf(model2$residuals, lag=48, main = "ARIMA(1,0,0)(1,0,0)[12] errors ")$acf

# Pull out the p-values to be used for Ljung Test
# What should be the df?
White.LB <- rep(NA, 48)
for(i in 1:48){
  White.LB[i] <- Box.test(model2$residuals, lag = i, type = "Ljung", fitdf = 2)$p.value
}

# H0: White Noise, No Autocorrelation
# HA: One or more autocorrelation up to lag m are not 0

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# Model 2: White Noise achieved with (1,0,0)(0,0,1)[12][12] errors

#######################################
# Model 3                             # 
# ARIMA(1,0,1)(1,0,0)[12] errors      #
#######################################

# Model 3 AR(1), MA(1), seasonal MA(1), dummy variable & linear matrix
model3=Arima(ts.months.train, xreg=matrix,order=c(1,0,1), seasonal=c(0,0,1))

Acf(model3$residuals, lag=48,main = " ARIMA(1,0,1)(0,0,1)[12] errors ")$acf
Pacf(model3$residuals, lag=48, main = "ARIMA(1,0,1)(0,0,1)[12] errors ")$acf

# Pull out the p-values to be used for Ljung Test
White.LB <- rep(NA, 48)
for(i in 1:48){
  White.LB[i] <- Box.test(model3$residuals, lag = i, type = "Ljung", fitdf = 3)$p.value
}

# H0: White Noise, No Autocorrelation
# HA: One or more autocorrelation up to lag m are not 0

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# Model 3: White Noise achieved with (1,0,1)(0,0,1)[12] errors


#######################################
# Model 4                             # 
# Fourier                             #
# ARIMA(1,0,2)(1,0,0)[12] errors
#######################################

# Fourier model 4 sine and 4 cosine
fourier.model4 <-Arima(ts.months.train,order=c(0,0,0),xreg=fourier(ts.months.train,K=4))
summary(fourier.model4)

# Fourier regressor (xreg)
f.xreg= fourier(ts.months.train,K=4)

# Set my new f.xreg so the Arima model can regress on both the fourier model and the trend component
f.matrix <- cbind(f.xreg,x)

# Before fitting any MA or AR terms in fourier
# regular MA 2?
Acf(fourier.model4$residuals, lag=48, main="ACF Fourier No AR or MA")$acf

# AR(1), seasonal AR(1)?
Pacf(fourier.model4$residuals, lag=48, main="PACF Fourier No AR or MA")$acf

# Create fourier model account for both linear trend and seasonality
model4.tr.seas=Arima(ts.months.train, xreg=f.matrix,order=c(1,0,2), seasonal=c(1,0,0))

# Create a new variable for residuals
f.res <- model4.tr.seas$residuals

Acf(f.res, lag=48,main = " whatever model ")$acf
Pacf(f.res, lag=48, main = "whatever model ")$acf

# Pull out the p-values to be used for Ljung Test
# What should be the df?
White.LB <- rep(NA, 48)
for(i in 1:48){
  White.LB[i] <- Box.test(f.res, lag = i, type = "Ljung", fitdf = 3)$p.value
}

# H0: White Noise, No Autocorrelation
# HA: One or more autocorrelation up to lag m are not 0

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# Model 4: White Noise achieved with (1,0,1)(0,0,1)[12] errors

#######################################
# Forecasting                         # 
#                                     #
#######################################

# Generate a sequence from 55 to 60
x1 = seq(55,60)

# test: A matrix of dummy variable obs 55-60 (12 months)
# Note: dummy_matrix is a dummy variable from the full dataset obs 1-60
test = dummy_matrix[55:60,]

# f.test: A matrix of fourier obs 55-60 (12 months)
# Note: dummy_matrix is a dummy variable from the full dataset obs 1-60
f.test = dummy_matrix[55:60,]

# Combine test and x1 to get a matrix of dummy variable and obs from 55 to 60 
# so that we can use in the xreg of the forecast function
new_xreg= cbind(test,x1)

#####################################################################
# Model 1    ARIMA(1,0,0)(1,0,0)[12] errors                         #
# Forecasting                                                       #
#####################################################################

# Warning message: xreg contains different column names from the xreg used in training. 
# Please check that the regressors are in the same order
test.forecast=forecast(arima.trend.season, xreg=new_xreg,h = 6)
summary(test.forecast)

# Same confidence interval width in comparison to model 2
# Lower forecast estimates than model 2
plot(test.forecast)

# Compare the predicted values to the validation data set
compare=months.valid$mean - test.forecast$mean
plot(compare)

actual = unclass(ts.months.valid)
pred = unclass(test.forecast$mean)

# compare how the model that I built which contains the predicted values against the validation data set
error=actual-pred
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(actual))
print(MAE)
print(MAPE)

# On Validation Data Set Model 1: ARIMA(1,0,0)(1,0,0)[12] errors
# MAE         MAPE
# 2.113552    0.2388704

################################################
# Model 2 ARIMA(1,0,0)(0,0,1)[12] errors       #
# Forecasting                                  #
################################################

model2.forecast = forecast(model2, xreg=new_xreg,h = 6)
summary(model2.forecast)

# Same confidence interval width in comparison to model 1
# Higher forecast estimates than model 1
plot(model2.forecast)

actual = unclass(ts.months.valid)
pred = unclass(model2.forecast$mean)

# Model 2 MAE and MAPE
error=actual-pred
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(actual))
print(MAE)
print(MAPE)

# Model 2 MAPE is worse than model 1
# MAE         MAPE
# 2.419793    0.2648123

#######################################
# Model 3                             #
# Forecasting                         #
#######################################

model3.forecast = forecast(model3, xreg=new_xreg,h = 6)
summary(model3.forecast)

# Same confidence interval width in comparison to model 1
# Higher forecast estimates than model 1
plot(model3.forecast)

actual = unclass(ts.months.valid)
pred = unclass(model3.forecast$mean)

# Model 3 MAE and MAPE
error=actual-pred
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(actual))
print(MAE)
print(MAPE)

# Model 3 MAPE is worse than Model 1 and Model 2
# MAE         MAPE
# 2.500045    0.2727406


#######################################
# Model 4                              #
# Forecasting Fourier
# ARIMA(1,0,2)(1,0,0)[12] errors
#######################################

model4.forecast = forecast(model4.tr.seas, xreg=new_xreg,h = 6)
summary(model4.forecast)

# Same confidence interval width in comparison to model 1
# Higher forecast estimates than model 1
plot(model4.forecast)

actual = unclass(ts.months.valid)
pred = unclass(model4.forecast$mean)

# Model 4 MAE and MAPE
error=actual-pred
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(actual))
print(MAE)
print(MAPE)

# Model 4
# MAE         MAPE
# 2.500045    0.2727406
