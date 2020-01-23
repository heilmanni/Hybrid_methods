#this function test different embedding dimensions and nodes combination on a training set
#and shows an MSE value for a test set
#training and test should be given as timeseries
#dimension means the maximum number of embedding dimensions - it will be calculated from 0 to this number
#nodes refer to the number of nodes in the hidden layer - it will be also calculated from 0 to this number
#dimension and nodes should be non-negative integers

neural_network <- function(training, test, dimension, nodes){
  
  #creating a dataframe for the most important information
  fit <- data.frame()
  
  for (p in 1:dimension)
  {
    for (size in 1:nodes)
    {
      full <- training
      for (pred in 1:length(test))
      {
        fcast <- forecast(object = nnetar(full, p = p, size = size, repeats = 5), h = 1) #forecast for one period, parameters of the nn are p and size
        full <- c(full, fcast$mean) #making a longer full time series by adding the new value of forecast
      }
      mse <- mse(actual = test, predicted = full[(length(full) - length(test) + 1 ):length(full)]) #calculating mean squared error
      
      fit <- rbind(fit, cbind(p, size, mse)) #summarising the most important information
      
    }
  }
  return(fit)
}