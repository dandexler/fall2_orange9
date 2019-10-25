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

# Autoregressive Neural Network Model #
NN.Model <- nnetar(ts.months.train, p = 1, P = 0, size = 4)
NN.Forecast <- forecast(NN.Model, h = 6)
plot(NN.Forecast)


xreg1 <- cbind(pm.train$jan, pm.train$feb, pm.train$mar, pm.train$apr,
               pm.train$may, pm.train$jun, pm.train$jul, pm.train$aug,
               pm.train$sep, pm.train$oct, pm.train$nov, pm.train$t, pm.train$t_sq)
Model.four <- Arima(ts.months.train,order = c(0, 0, 0), xreg = xreg1)

Acf(Model.four$residuals, lag = 48, main = "RAW ACF")$acf
Pacf(Model.four$residuals, lag = 48, main = "RAW PACF")$acf
NN.Model2<-nnetar(Model.four$residuals, p = 1, P = 1, size = 1)
# Fit 1 AR and 1 seasonal AR due to PACF spikes
# Size set to one using formula Nodes = half of k(p+ P)

NN.Forecast2<-forecast(NN.Model2, h = 6)
plot(NN.Forecast2)
summary(NN.Forecast2)
pm.Forecast <- rep(NA, 6)

for(i in 1:6){
  pm.Forecast[i] <- ts.months.valid[i] + forecast(NN.Model, h = 6)$mean[i]
}

pm.Forecast <- ts(pm.Forecast, start = c(2018, 7), frequency = 12)


plot(ts.months.full, main = "PM 2.5 Concentration", xlab = "Date", ylab = "Concentration (ug/m3)", xlim = c(2014, 2019))
lines(pm.Forecast2, col = "blue")
abline(v = 2018.5, col = "red", lty = "dashed")

xreg2 <- cbind(pm.valid$jan, pm.valid$feb, pm.valid$mar, pm.valid$apr,
               pm.valid$may, pm.valid$jun, pm.valid$jul, pm.valid$aug,
               pm.valid$sep, pm.valid$oct, pm.valid$nov, pm.valid$t, pm.valid$t_sq)

Base.forecast<-forecast(Model.four,xreg=xreg2,h=6)
pm.Forecast2 <- Base.forecast$mean+NN.Forecast2$mean

pm.Forecast2 <- ts(pm.Forecast2, start = c(2018, 7), frequency = 12)


# MAPE AND MAE CALCULATIONS
actual = unclass(ts.months.valid)
pred = unclass(pm.Forecast2)

error=actual-pred
MAE=mean(abs(error))
MAPE=mean(abs(error)/abs(actual))
print(MAE)
print(MAPE)
