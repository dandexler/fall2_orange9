# Time Series Homework 4

#------------------------------------#
#           Time Series              #
#           Homework 4               #
#           David Andexler           #
#                                    #
#------------------------------------#

library(dplyr)
library(lubridate)
library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(caschrono)
library(TSA)
library(imputeTS)
library(fpp2)

# Data preparation
# Read in CSV file with PM2.5 data
data <- read.csv("C:\\Users\\dande\\Desktop\\MAIN\\Documents\\NCState Advanced Analytics\\Fall 2019\\Fall 1\\Time Series I\\Data\\PM_2_5_Raleigh2.csv")

# Converts factor column Date to date type, combine with daily PM2.5
dates_list <- as.Date(data[,1], format = '%m/%d/%Y')
p_matter <- data[,5]
month_pm2 <-data.frame(dates_list, p_matter)
colnames(month_pm2)<- c("Date","Amount")

# Aggregate daily PM2.5 and calculate mean for each month
data <- month_pm2 %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

monthly_pm2 <- data %>%
  group_by(yr = year(Date),mon = month(Date)) %>% 
  summarise(Amount = mean(Amount))







# Training and validation sets
pm2_t <- monthly_pm2[1:54,]
pm2_v <- monthly_pm2[55:60,]

#Testing, combined training and validation
pm2_full <- monthly_pm2[1:60,]









# Creation of Time Series objects from training and validation sets of mean monthly PM2.5, 12 month frequency
# Testing, change back to pm2_t and pm2_v
pm2_t.ts <- ts(pm2_full$Amount[1:54], frequency = 12)
pm2_v.ts <- ts(pm2_full$Amount[54:60], frequency = 12)

# Creation of Time Series object if there are missing values
# pm2_t.ts2<-pm2_t.ts%>% na_interpolation(option = "spline")
# pm2_v.ts2<-pm2_v.ts%>% na_interpolation(option = "spline")


# STL decomposition of training set
decomp_stl <- stl(pm2_t.ts, s.window = 7)
plot(decomp_stl)
#***************************************************
# Result: There appears to be seasonality and trend
#***************************************************






#------------------------------------#
#           Model Building           #
#                                    #
#                                    #
#                                    #
#------------------------------------#

###########################
# Manual selection of model
###########################


# Accounting for seasonality with dummy variables for training set
month=factor(pm2_t$mon)
reg=model.matrix(~month)
reg=reg[,-1]

reg_t <- reg_full[1:54,]
reg_v <- reg_full[54:60, 7:11]

# Training model adjusted for seasonality with dummy variables
dummy.sarima=Arima(pm2_t.ts,xreg = reg_t)
summary(dummy.sarima)

# Validation set adjusted for seasonality in same way as training *DO NOT USE FOR MODEL*
dummy.sarima.v <- Arima(pm2_v.ts, xreg = reg_v)

# Exploration of data accounting for seasonality
tsdisplay(dummy.sarima$residuals)

#**************************************
# Result: Adjusting for seasonality only does not take
# care of correlation structure. There is also a trend 
# that must be accounted for.
#**************************************


########################
# Determining Trend
########################

# ADF at lag 0, lag 1, lag 2 to confirm trend
# H0: Stochastic trend
# HA: Deterministic trend

# Lag 0 - -4.3745, reject null, p < 0.01
adf.test(dummy.sarima$residuals, alternative = "stationary", k=0)
# Lag 1 - -3.8989, reject null, p = 0.02055
adf.test(dummy.sarima$residuals, alternative = "stationary", k=1)
# Lag 2 - -3.7733, reject null, p = 0.02688
adf.test(dummy.sarima$residuals, alternative = "stationary", k=2)

#*******************************************************
# Result: There is evidence to suggest that there is a 
# deterministic trend remaining at all three lags tested.
#*******************************************************

###############################################
# Accounting for Trend (Applying Stationarity)
###############################################

# De-trending by regression
x <- seq(1, 54)
y <- dummy.sarima$residuals
dummy.sarima.trend=Arima(y, xreg=x,order=c(0,0,0))
tsdisplay(dummy.sarima.trend$residuals)
# New model: dummy.sarima.trend ---- Seasonality, trend

# Adjust validation for trend *DO NOT USE FOR MODEL*
x_v <- seq(54, 60)
y_v <- dummy.sarima.v$residuals
dummy.sarima.trend.v=Arima(y_v, xreg=x_v,order=c(0,0,0))

# Evaluating autocorrelation structure
acf(dummy.sarima.trend$residuals, lag.max = 25)
pacf(dummy.sarima.trend$residuals, lag.max = 25)

# Ljung-Box test for autocorrelation
# H0: No autocorrelation
# HA: Autocorrelation
Box.test(dummy.sarima.trend$residuals, type = "Ljung-Box")
# Result: Reject null, p-value = 0.002911, autocorrelation

# Visual representation of Ljung-Box Test
White.LB <- rep(NA, 48)
for(i in 1:48){
  White.LB[i] <- Box.test(dummy.sarima.trend$residuals, lag = i, type = "Lj", fitdf = 0)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

#*********************************************************
# Result: Trend removed by regression. Evidence to suggest
# that autocorrelation continues to exist in the residual 
# relationship. ARMA terms must be investigated.
#*********************************************************

###############################################
# Accounting for ARMA/Seasonal ARMA terms
###############################################

# Auto ARIMA to give us an idea of what ARMA terms to add
auto.arima(dummy.sarima.trend$residuals)
# Result: ARIMA(1, 0, 0)(1, 0, 0)

# Why do we do this? Must explore.
new_xreg <- cbind(x, reg)

# Adding AR(1), sAR(1)
dummy.sarima.trend.sarma <- Arima(dummy.sarima.trend$residuals, xreg = new_xreg,  order=c(1,0,0),season=c(1, 0, 0))
summary(dummy.sarima.trend.sarma)
tsdisplay(dummy.sarima.trend.sarma$residuals, lag.max = 48)
# New model: dummy.sarima.trend.sarma --- Seasonality, trend, AR/sAR

# Adding AR(1) and sAR(1) to validation *DO NOT USE FOR MODEL*
# Doesn't work!
new_v <- cbind(x_v, reg_v)
dummy.sarima.trend.sarma.v <- Arima(dummy.sarima.trend.v$residuals, xreg = new_v,  order=c(1,0,0),season=c(1, 0, 0))

# Adjusting validation for AR/sAR terms
new_xreg_v <- cbind(x_v, reg_v)

# Adding AR(1), sAR(1) for validation
dummy.sarima.trend.sarma.v <- Arima(dummy.sarima.trend.v$residuals, xreg = reg_v,  order=c(1,0,0),season=c(1, 0, 0))


# Ljung-Box test for autocorrelation
# H0: No autocorrelation
# HA: Autocorrelation
Box.test(dummy.sarima.trend.sarma$residuals, type = "Ljung-Box")
# Result: p-value = 0.5982, fail to reject null. No autocorrelation. White noise.


# Visualization of Ljung-Box 
White.LB <- rep(NA, 48)
for(i in 1:48){
  White.LB[i] <- Box.test(dummy.sarima.trend.sarma$residuals, lag = i, type = "Lj", fitdf = 2)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# Match

#*****************************************************
# Result: AR/sAR terms added to new model. No apparent 
# autocorrelation remaining in data.
#*****************************************************






###############################################
# Forecasting - Final model from training: 
# dummy.sarima.trend.sarma
###############################################

# Forecasted values for 6 time values out from final model 
forecast_t=forecast(dummy.sarima.trend.sarma, xreg=new_xreg,h = 6)
summary(forecast_t)
plot(forecast_t)

# Perform similar functions to validation set

# Dummy variables for validation for seasonality
val_x = seq(55,60)
val_xreg= cbind(val_x,val_reg)


val_month=factor(pm2_v$mon)
val_reg=model.matrix(~val_month)
val_reg=reg[,-1]
val_xreg= cbind(val_x,val_reg)


# Needs discussion with group
# I think we need to make the same adjustments to the validation data set as we did to test data
# Perhaps this is built into Cathy's code?


# Compare the predicted values to the validation data set
compare=months.valid$mean - test.forecast$mean
plot(compare)

actual = unclass(ts.months.valid)
pred = unclass(test.forecast$mean)

error=actual-pred
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(actual))
print(MAE)
print(MAPE)

# On Validation Data Set Model 1: ARIMA(1,0,0)(1,0,0)[12] errors
# MAE         MAPE
# 2.113552    0.2388704