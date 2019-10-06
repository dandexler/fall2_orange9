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









# Creation of Time Series objects from training and validation sets of mean monthly PM2.5, 12 month frequency
pm2_t.ts <- ts(pm2_t$Amount, frequency = 12)
pm2_v.ts <- ts(pm2_v$Amount, frequency = 12)

# Creation of Time Series object if there are missing values
# pm2_t.ts2<-pm2_t.ts%>% na_interpolation(option = "spline")
# pm2_v.ts2<-pm2_v.ts%>% na_interpolation(option = "spline")


# STL decomposition of training set
decomp_stl <- stl(pm2_t.ts, s.window = 7)
plot(decomp_stl)
# Result: There appears to be seasonality and trend






#------------------------------------#
#           Model Building           #
#                                    #
#                                    #
#                                    #
#------------------------------------#

# Automatic selection technique, auto.arima. Will not use because client expects seasonal ARIMA.
# pm2_t_auto <- auto.arima(pm2_t.ts)
# Result: ARIMA(0, 1, 0)


# Manual selection of model

# Accounting for seasonality with dummy variables
month=factor(pm2_t$mon)
reg=model.matrix(~month)
reg=reg[,-1]

# Model
dummy.sarima=Arima(pm2_t.ts,xreg=reg)
summary(dummy.sarima)
Box.test(dummy.sarima$residuals, lag=10, type = "Ljung-Box")
# Result: Correlation structure remains








# Exploration of data accounting for seasonality
plot(dummy.sarima$residuals)
acf(dummy.sarima$residuals)
pacf(dummy.sarima$residuals)

# Results: There still appears to be a trend that must be accounted for
# AR/MA terms: There are significant spikes at ACF lag 1, 2, and 9?
# Also at PACF 1 and 13
# ACF function itself has slightly different determinations - difference in algorithm?









# Since there is visual trend, must run ADF to confirm

# Lag 0 - -4.3745, reject null, p < 0.01
adf.test(dummy.sarima$residuals, alternative = "stationary", k=0)
# Lag 1 - -3.8989, reject null, p = 0.02055
adf.test(dummy.sarima$residuals, alternative = "stationary", k=1)
# Lag 2 - -3.7733, reject null, p = 0.02688
adf.test(dummy.sarima$residuals, alternative = "stationary", k=2)
# Result: There is evidence to suggest that there is a deterministic trend remaining







# De-trending by regression

# De-trending by regression
dummy.resid.ts <- ts(dummy.sarima$residuals)
dummy.sarima.trend=Arima(dummy.resid.ts, xreg=reg,order=c(0,0,0))
# Result: There does not seem to be a change? This is unusual. Exploring alternative de-trending methods.

# De-trending by differencing, exploration
ndiffs(dummy.sarima$residuals)
# Result: 1, indicating 1 difference should be taken

# First-order differencing for linear trends
dummy.sarima.diff <- diff(dummy.sarima$residuals)
dummy.sarima.diff.ts <- ts(dummy.sarima.diff)
tsdisplay(dummy.sarima.diff.ts)
# Result: Deterministic trend has been removed
# Evidence remains of correlation structures at ACF lag 12 and PACF 12

# Ljung-Box test for autocorrelation
Box.test(dummy.sarima.diff.ts, type = "Ljung-Box")
# Result: we are not quite at white noise but much improved.







# Forecasting
forecast(dummy.sarima.diff.ts, h = 10)
plot(forecast(dummy.sarima.diff.ts, h = 10))

