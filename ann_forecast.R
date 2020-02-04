#this function calculate MSE values for neural network models with different embedding dimensions 
#and with different numbers of nodes
#ltrain means the length of training set - it must be a positive integer
#ltest means the length of test set - it must be a positive integer
#y is the result timeseries - it must be a ts or vector
#xreg is vector or matrix which contains the regressors
#max_dim shows the maximum number of embedding dimensions for testing - it must be a positive integer; 0 to max_dim the results will be  calculated
#max_nodes shows the maximum number of nodes for testing - it must be a positive integer; 0 to max_nodes the results will be calculated


ann_forecast <- function(ltrain, ltest, y, xreg, max_dim, max_nodes)
{
  #creating a dataframe for the most important information
  info <- data.frame()
  
  for (dim in 1:max_dim)
  {
    for (nodes in 1:max_nodes)
    {
      #creating a dataframe for the predicted values
      predict <- c()
      
      #making a rolling window with the same length as the test set
      for (pred in 0:(ltest-1))
      {
        data_y <- y[(1+pred):(ltrain+pred)] #y has always the same length, but the start and ending points are different
        data_x <- xreg[(1+pred):(ltrain+pred),] #x has always the same length, but the start and ending points are different
        ann_model <- nnetar(data_y, p = dim, size = nodes, repeats = 10, xreg = data_x) #calculating the exact parameters of the ann model
        fcast <- as.numeric(forecast(object = fitted(ann_model), h = 1)[[2]]) #forecasting in one-horizon length
        predict <- c(predict, fcast) #binding the earlier and new forecasts
      }
      
      MSE_ANN <- mse(actual = y[(ltrain+1):(ltrain+ltest)], predicted = predict) #computing the MSE value between out-of-sample and forecasted values
      info <- rbind(info, cbind(dim, nodes, MSE_ANN)) #binding the most important information
    }
  }
  return(info)
}

#this function is a simplier version of ann_forecast function
#here the number of embedding dimensions and nodes must be given (as positive integers)
#this function shows an MSE value for an out-of-sample set and the predicted values

ann_out_forecast <- function(lisd, losd, dim, nodes, y, xreg)
{
  #creating a dataframe for the predicted values
  predict <- c()
  
  #making a rolling window with the same length as the out-of-sample set
  for (pred in 0:(losd-1))
  {
    data_y <- y[(1+pred):(lisd+pred)] #y has always the same length, but the start and ending points are different
    data_x <- xreg[(1+pred):(lisd+pred),] #x has always the same length, but the start and ending points are different
    ann_model <- nnetar(data_y, p = dim, size = nodes, repeats = 10, xreg = data_x) #calculating the exact parameters of the ann model
    fcast <- as.numeric(forecast(object = fitted(ann_model), h = 1)[[2]]) #forecasting in one-horizon length
    predict <- c(predict, fcast) #binding the earlier and new forecasts
  }
  MSE_ANN_out <- mse(actual = y[(lisd+1):(lisd+losd)], predicted = predict) #computing the MSE value between out-of-sample and forecasted values
  final <- c(MSE_ANN_out, predict)
  
  return(final)
}
