#this function forecasts values based on a hybrid methods
#the best arma model could be calculated with arima_prediction function, please use the same parameters (ar, ma, d)
#you have to source the neural_network function
#in_sample should be a timeseries on which the function will predict
#h is lenght of the forecast; recommended choosing the lenght of out-of-sample timeseries
#p means the maximum number of embedding dimensions - it will be calculated from 0 to this number
#size refers to the number of nodes in the hidden layer - it will be also calculated from 0 to this number
#brake should be given between 0 and 1 - it refers to the proportion of training set used for ann models, based on the residuals of arma model

hybrid_forecast <- function(in_sample, h, ar, ma, d, p, size, brake)
{
  #if the in_sample data are not given in a vector format we have to change it
  in_sample <- as.vector(in_sample)
  
  #calculating the best parameters for the forecasting
  arma_model <- arima(in_sample, order = c(ar, d, ma)) #an arma model with given parameters
  arma_res <- arma_model$residuals #residuals of the model
  
  #creating a test and training set at a brakepoint (which gives the proportion of the training set to the whole)
  border <- as.integer(length(arma_model$residuals)*brake) 
  arma_res_training <- arma_res[1:border]
  arma_res_test <- arma_res[(border+1):length(arma_res)]
  
  #searching for the best ann modell on the residuals of arma_model
  #required function: neural_network
  arma_res_ann <- neural_network(training = arma_res_training, test = arma_res_test, dimension = p, nodes = size)
  
  #choosing the best model based on mse
  best_param <- arma_res_ann[which.min(arma_res_ann[,3]),]
  best_p <- as.numeric(best_param[1])
  best_size <- as.numeric(best_param[2])
  
  #forecasting with the given params
  for (i in 1:h)
  {
    arma_fcast <- as.numeric(forecast(arima(in_sample, order = c(ar, d, ma)), h = 1)$mean) #arma forecast for the next period
    nn_fcast <- forecast(nnetar(arma_res, p = best_p, size = best_size, repeats = 5), h = 1)$scalex$center #nn forecast for the next period's residual
    fcast <- arma_fcast + nn_fcast #the forecast is the sum of two predictions
    in_sample <- c(in_sample, fcast) #broaden the in_sample with the fcast
    arma_res <- c(arma_res, nn_fcast) #broaden the arma_res with the nn_fact as the next residual
  }
  
  return(in_sample)
}


