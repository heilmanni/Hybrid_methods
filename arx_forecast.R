#this function calculates MSE values for different ARX models and shows the predicted values
#lisd is the length of in_sample timeseries
#losd is the length of out_of_sample timeseries
#lisd and losd should be positive integers; lisd + losd cannot be longer than y
#y is the result time-series in vector format
#xreg is a vector or matrix which consists of the regressors
#max_ar shows the maximum number of AR lags - it should be a positive integer

arx_forecast <- function(lisd, losd, y, xreg, max_ar)
{
  #creating a dataframe for the final results
  info <- data.frame()
  forecasting <- data.frame()
  
  for (phi in 1:max_ar)
  {
    #creating a dataframe for the predicted values
    predict <- c()
    
    #making a rolling window with the same length as the out_of_sample
    for (pred in 0:(losd-1))
    {
      data_y <- y[(1+pred):(lisd+pred)] #y has always the same length, but the start and ending points are different
      data_x <- xreg[(1+pred):(lisd+pred),] #x has always the same length, but the start and ending points are different
      arx_model <- gets::arx(y = data_y, ar = 1:phi, mxreg = data_x, mc = T) #calculating the exact parameters of the arx model
      fcast <- as.numeric(forecast::forecast(object = fitted(arx_model), h = 1)[[2]]) #forecasting in one-horizon length
      predict <- c(predict, fcast) #binding the earlier and new forecasts
    }
    full_y <- data.frame() #creating a dataframe
    full_y <- c(data_y[(1:lisd)], predict) #binding the original in_sample timeseries with the forecasts
    stationarity_p <- as.numeric(tseries::adf.test(x = full_y, k = phi)[[4]])
    autocorrelation_p <- stats::Box.test(resid(gets::arx(y = full_y, ar = 1:phi, mxreg = xreg)), type = "Ljung") #computing the p-value of Ljung-Box autocorrelation test
    autocorrelation_p <- as.numeric(autocorrelation_p[3]) #defining p-value as a numeric variable
    MSE_ARX <- ModelMetrics::mse(actual = y[(lisd+1):(lisd+losd)], predicted = predict) #computing the MSE value between out-of-sample and forecasted values
    info <- rbind(info, cbind(phi, MSE_ARX, stationarity_p, autocorrelation_p)) #binding the most important pieces of information
    forecasting <- rbind(forecasting, predict) #binding the predicted values
  }
  final <- cbind(info, forecasting)
  colnames(final) <- c("phi", "MSE_ARX", "ADF-test p-value", "Ljung-Box test p-value", c(1:losd))
  return(final)
}
