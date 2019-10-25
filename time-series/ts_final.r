##Neural Networks

# Needed Libraries for Analysis #
library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(caschrono)
library(TSA)
library(quantmod)


# Saving File Locations and Uploading SAS File #
file.dir <- "C:/Users/zachm/Documents/Fall/Module 2/Time Series II/Final Project/"
input.file1 <- "monthly_x.sas7bdat"

pm <- read_sas(paste(file.dir, input.file1,sep = ""))

pm.train <- pm[pm$t <= 54,]
pm.valid <- pm[pm$t > 54,]

# Creating Time Series Data Objects #
ts.months.train <- ts(pm.train$monthly_mean, start = 2014, frequency =12)
ts.months.valid <- ts(pm.valid$monthly_mean, start = 2018, frequency = 12)
ts.months.full <- ts(pm$monthly_mean, start = 2014, frequency = 12)

# Building ARNN
# Create dummy variables and quadratic trend variables from SAS data set
xreg1 <- cbind(pm.train$jan, pm.train$feb, pm.train$mar, pm.train$apr,
               pm.train$may, pm.train$jun, pm.train$jul, pm.train$aug,
               pm.train$sep, pm.train$oct, pm.train$nov, pm.train$t, pm.train$t_sq)
# Create ARIMA model with trend and seasonality fit
Model.four <- Arima(ts.months.train,order = c(0, 0, 0), xreg = xreg1)

# Get ACF and PACF Plots
Acf(NN.Model2$residuals, lag = 48, main = "RAW ACF")$acf
Pacf(NN.Model2$residuals, lag = 48, main = "RAW PACF")$acf

# Perform Neural Network Analysis
NN.Model2<-nnetar(Model.four$residuals, p = 1, P = 1, size = 1)
# Fit 1 AR and 1 seasonal AR due to PACF spikes
# Size set to one using formula Nodes = half of k(p+ P)

# Check white noise No MA or AR term 
# Pull out p-values
White.LB <- rep(NA, 36)
for(i in 1:36){
  White.LB[i] <- Box.test(NN.Model2$residuals, lag = i, type = "Ljung", fitdf = 2)$p.value
}

# H0: White Noise, No Autocorrelation
# HA: One or more autocorrelation up to lag m are not 0

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# Create predictions for validation data set based on trend and seasonal components of the model
xreg2 <- cbind(pm.valid$jan, pm.valid$feb, pm.valid$mar, pm.valid$apr,
               pm.valid$may, pm.valid$jun, pm.valid$jul, pm.valid$aug,
               pm.valid$sep, pm.valid$oct, pm.valid$nov, pm.valid$t, pm.valid$t_sq)

Base.forecast<-forecast(Model.four,xreg=xreg2,h=6)
pm.Forecast2 <- Base.forecast$mean+NN.Forecast2$mean

# Convert forecasts for validation set into a time series
pm.Forecast2 <- ts(pm.Forecast2, start = c(2018, 7), frequency = 12)

# Plot validation data overlaid on actual values
plot(ts.months.full, main = "PM 2.5 Concentration", xlab = "Date", ylab = "Concentration (ug/m3)", xlim = c(2014, 2019))
lines(pm.Forecast2, col = "blue")
abline(v = 2018.5, col = "red", lty = "dashed")

# MAPE AND MAE CALCULATIONS
actual = unclass(ts.months.valid)
pred = unclass(pm.Forecast2)

error=actual-pred
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(actual))
print(MAE)
print(MAPE)
