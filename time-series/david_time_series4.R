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
pm2_full.ts <- ts(pm2_full)

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
obs_val.ts <- ts(dummy.sarima.trend.v$residuals)

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
# Doesn't work! Testing
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

tsdisplay(dummy.sarima.trend.sarma$residuals)

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
# dummy.sarima.trend.sarma --- Forecasting not yet functional!
###############################################

# Forecasted values for 6 time values out from final model 
forecast_t=forecast(dummy.sarima.trend.sarma, xreg=new_xreg,h = 6)
summary(forecast_t)
plot(forecast_t)

# Needs discussion with group
# I think we need to make the same adjustments to the validation data set as we did to test data
# Perhaps this is built into Cathy's code?


# Compare the predicted values to the validation data set
compare=pm2_v$Amount - forecast_t$mean
plot(compare)

actual = dummy.sarima.trend.v$residuals
pred = forecast_t$residuals

error=actual-pred
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(actual))
print(MAE)
print(MAPE)

# On Validation Data Set Model 1: ARIMA(1,0,0)(1,0,0)[12] errors
# MAE         MAPE
# 2.113552    0.2388704



################################
# 
#Fit QUADRATIC Regression
# 
################################
xsq <- x^2
quad_matrix<-cbind(x,xsq)

# Use the time series object from the seasonal model
arima.trend.quad=Arima(ts.seas.resid, xreg=quad_matrix, order=c(0,0,0))
summary(arima.trend.quad)


# New variable for quadratic trend residuals
quad.res <- arima.trend.quad$residuals

# Quadratic trend residuals plot
# 2014-2016 period residuals shifted down to center around 0. 
# 2017-2018 period residuals shifted up to center around 0.
# Looks GREAT!
plot(quad.res, xlab='Number of Observations',ylab='Residuals',main='Residuals Plot After Fitting Quadratic Trend',type='l')

# Spike at lag 1 and a small spike lag 12 --> AR(1)
Acf(quad.res, lag=36,main = " ACF No MA or AR terms")$acf 

# spikes at lag 1 and lag 12 in the PACF, which suggest seasonal AR 1?
Pacf(quad.res, lag=36, main = "PACF No MA or AR terms")$acf 

# Check white noise No MA or AR term 
# Pull out p-values
White.LB <- rep(NA, 60)
for(i in 1:60){
  White.LB[i] <- Box.test(quad.res, lag = i, type = "Ljung", fitdf = 0)$p.value
}

# H0: White Noise, No Autocorrelation
# HA: One or more autocorrelation up to lag m are not 0

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# Automatic selection quadratic trend
# Suggestion ARIMA(1,0,0)(1,0,0)[12] errors 
# Didn't get great MAE and MAPE on validation data. 
auto.arima(quad.res, xreg = sq_new_xreg)

# New xreg - dummy matrix and quadratic trend 
sq_new_xreg <- cbind(training, quad_matrix)

# Create the model with quadratic trend
quad.tr.model = Arima(ts.months.train, xreg=sq_new_xreg,order=c(1,0,0), seasonal=c(0,0,1))
summary(quad.tr.model) 


Acf(quad.tr.model$residuals, lag=36,main = " ACF (1,0,0)(0,0,1) ")$acf
Pacf(quad.tr.model$residuals, lag=36, main = "PACF (1,0,0)(0,0,1) ")$acf

# Pull out the p-values to be used for Ljung Test
White.LB <- rep(NA, 24)
for(i in 1:24){
  White.LB[i] <- Box.test(quad.tr.model$residuals, lag = i, type = "Ljung", fitdf = 2)$p.value
}

# H0: White Noise, No Autocorrelation
# HA: One or more autocorrelation up to lag m are not 0

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# Yes, white noise - quad, seasonality



# FORECASTING QUADRATIC
# ARIMA(1,0,0)(0,0,1)
quad.forecast = forecast(quad.tr.model, xreg=nreg_qmatrix,h = 6)
summary(quad.forecast)


Acf(quad.forecast$residuals, lag=36,main = " ACF (1,0,0)(0,0,1) ")$acf
Pacf(quad.forecast$residuals, lag=36, main = "PACF (1,0,0)(1,0,1) ")$acf


# Same confidence interval width in comparison to model 1
# Higher forecast estimates than model 1
plot(quad.forecast)

actual = unclass(ts.months.valid)
pred = unclass(quad.forecast$mean)

# MAE and MAPE
error=actual-pred
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(actual))
print(MAE)
print(MAPE)

# MAE         MAPE
# 1.347296    0.1684877

# plot of forecast values with actual values
months <- c("July2018"=7, "August 2018"=8, "September 2018"=9, "October 2018"=10, "November 2018"=11, "December 2018"=12)
month_names <- c("July 2018", "August 2018", "September 2018", "October 2018", "November 2018", "December 2018")

forecast_value <- as.vector(quad.forecast$mean)
forecast_upper <- as.vector(quad.forecast$upper)
forecast_lower <- as.vector(quad.forecast$lower)
actual_value <- as.vector(ts.months.valid)


df <- data.frame(months, actual_value=actual_value, forecast_value=forecast_value)

p <- ggplot(df, aes(x=months))+
  xlab("Months - 2018")+
  ylab("Average Monthly Particulate Matter 2.5")+
  geom_line(aes(y=actual_value), size=2, color="#00BFC4", alpha=0.8)+
  geom_line(aes(y=forecast_value), size=2, color="#F8766D", alpha=0.8)+
  geom_point(x=months, y=actual_value, shape=15, size=6, color="#00BFC4", alpha=0.8)+
  geom_point(x=months, y=forecast_value, shape=15, size=6, color="#F8766D", alpha=0.8)+
  geom_text(x=12, y= 13.1, label="Actual Values", color="#00BFC4")+
  geom_text(x=11.9, y= 11.9, label="Forecasted Values", color="#F8766D")
p