#This file forecast in three different way: ARIMA, ANN and hibrid methology will be used

#packages
library(tseries)
library(forecast)
library(ModelMetrics)
library(neuralnet)
library(xts)
library(gets)

#IMPORT TIMESERIES
#you have to import the phil_data.R file
#the output variable is the inflation; there is two given option, based on GDP-deflator and PCE deflator
y_GDP <- phil_data[,2]
y_PCE <- phil_data[,3]

#the regressors are the columns 3 to 9
x_reg <- phil_data[,3:9]


#making training, test and out-of-sample sets
training <- RawData[1:950]
test <- RawData[951:1000]
in_sample <- RawData[1:1000]
out_sample <- RawData[1001:1010]


#1. ARIMA MODEL
#ADF test, number of differencies
adf.test(in_sample)
ndiffs(in_sample)

#making a stationary timeseries from the original one
stationary <- data.frame()
stationary <- diff(in_sample, differences = 1)
stationary[1] <- 0

stationary_out <- diff(out_sample, differences = 1)
stationary_out[1] <- 0

#let's have a look at the correlograms
acf(stationary)
pacf(stationary)

#you should source the arima_prediction function
#I will use an ARX modell, so the MA member should be 0
model_arima <- arima_prediction(training = in_sample, test = out_sample, ar = 7, ma = 0, d = 1)

#select the rowes where p-value are larger than 0.05
model_arima <- model_arima[which(model_arima[,4] > 0.05), ]

#the best model is where MSE is the smallest
model_arima[which.min(model_arima[,3]),]

#to have an ARX model you can use arx function ('gets' package)
arx <- arx


#NEURAL NETWORKS METHODOLOGY
#you should source neural_netwrok function
model_ann <- neural_network(training = training, test = test, dimension = 3, nodes = 3)
model_ann[which.min(model_ann[,3]),]

proba <- model_ann[which.min(model_ann[,3]),]

#forecasting for out-of-sample set


#HYBRID METHODOLOGY
#ARIMA model on the original timeseries
#ANN model on the residuals of original time series
fcast_hybrid <- hybrid_forecast(in_sample = in_sample, h = 10, ar = 2, ma = 0, d = 1, p = 2, size = 3, brake = 0.95)



