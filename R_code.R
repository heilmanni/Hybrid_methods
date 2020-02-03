#This file forecast in three different ways: ARIMA, ANN and hibrid methology will be used

#packages
library(tseries)
library(forecast)
library(ModelMetrics)
library(neuralnet)
library(xts)
library(gets)
library(zoo)

#IMPORT TIMESERIES
#phil_data.R file has to be imported
#there are missing values in the last row
phil_data <- phil_data[1:243,]

#the output variable is the inflation; there is two options, based on GDP deflator and PCE deflator
y_GDP <- phil_data[,2]
y_GDP <- y_GDP[[1]]
y_PCE <- phil_data[,3]
y_PCE <- y_PCE[[1]]

#the regressors are the columns 3 to 9
x_reg <- phil_data[,4:9]
x_reg <- cbind(x_reg[[1]], x_reg[[2]], x_reg[[3]], x_reg[[4]], x_reg[[5]], x_reg[[6]])

#1. ARX MODEL
glm(y_GDP ~ xreg)
Box.test(x = resid(glm(y_GDP ~ xreg)), type = "Ljung")
#autocorrelation is presented

#you should source arx_forecast function
arx_forecast(lisd = 230, losd = 13, y = y_GDP, xreg = x_reg, max_ar = 5)
arx_forecast(lisd = 230, losd = 13, y = y_PCE, xreg = x_reg, max_ar = 5)

#the best model is that where the MSE is the smallest and the p-value of Ljung-Box test are larger than 0.05
#in both cases this is when phi = 1

#MSE = 0.1295500

forecast_arx <- arx(y = y_in_sample, ar = phi, mxreg = x_reg_in_sample, mc = T)


#NEURAL NETWORKS METHODOLOGY
#ann_forecast function has to be sourced
model_ann <- ann_forecast(ltrain = 200, ltest = 30, y = y_PCE, xreg = x_reg, max_dim = 4, max_nodes = 4)

#forecasting on the out-of-sample horizon with the best combination of dimensions and nodes
model_ann[which.min(model_ann[,3]),]
#now dim = 3, nodes = 4

#for comparing a forecast and error value must be given for the out-of-sample data
ann_out_forecast(lisd = 230, losd = 13, dim = 1, nodes = 4, y = y_GDP, xreg = x_reg)

#MSE = 0.143295

#HYBRID METHODOLOGY
#required functions: arx_forecast, ann_forecast
hybrid_forecast(y = y_GDP, xreg = x_reg, lisd = 230, losd = 13, phi = 1, brake = 0.85, max_dim = 2, max_nodes = 2)

MSE_hybrid <- 0.1149234
