#this function calculates an MSE value based on a hybrid method and shows the predicted values
#y is the result time-series - it must be a ts or vector
#xreg is a vector or a matrix which contains the regressors
#lisd is the length of in_sample time-series
#losd is the length of out_of_sample time-series
#lisd and losd should be positive integers; lisd + losd cannot be longer than y
#phi is the optimal number of ar lag - the best arx model can be calculated with arx_forecast function, please use the same parameters as in the first part of analysis
#brake should be given between 0 and 1 - it refers to the proportion of the training set to the whole in the ann model, based on the residuals of arx model
#max_dim shows the maximum number of embedding dimensions for testing - it must be a positive integer; from 0 to max_dim the results will be  calculated
#max_nodes shows the maximum number of nodes for testing - it must be a positive integer; from 0 to max_nodes the results will be calculated


#further required functions: arx_forecast, ann_forecast
source(file = "https://raw.githubusercontent.com/heilmanni/Hybrid_methods/master/arx_forecast.R")

hybrid_forecast <- function(y, xreg, lisd, losd, phi, brake, max_dim, max_nodes)
{

  final <- data.frame() #creating a data frame for the final results
  losd_number <- c(1:losd) #a vector from 1 up to the length of the out-of-sample
    
  for (dim in 1:max_dim)
  {
    print(dim)
    
    for (nodes in 1:max_nodes)
    {
      
      #creating a vector for the forecast
      predict <- c()
      
      #making a rolling window with the same length as the out_of_sample
      for (pred in 0:(losd-1))
      {
        y_in_sample <- y[(1+pred):(lisd+pred)] #y has always the same length, but the start and ending points are different
        x_reg_in_sample <- xreg[(1+pred):(lisd+pred),] #x has always the same length, but the start and ending points are different
        
        
        #calculating the best parameters for the forecasting
        arx_model <- gets::arx(y = y_in_sample, ar = phi, mxreg = x_reg_in_sample, mc = T) #an arx model with a given ar lag
        arx_res <- arx_model$residuals #residuals of the model
        
        #creating a test and training set at a brakepoint (which gives the proportion of the training set to the whole)
        border <- as.integer(length(arx_res)*brake)
        
        #forecasting with the given params
        arx_fcast <- as.numeric(forecast::forecast(object = fitted(arx_model), h = 1)[[2]]) #arx forecast for the next period
        nn_fcast <- as.numeric(forecast::forecast(object = fitted(forecast::nnetar(y = arx_res, xreg = x_reg_in_sample[(phi+1):lisd,],
                                                                                   p = dim, size = nodes, repeats = 10)),
                                                  h = 1)[[2]]) #nn forecast for the residual of the next period
        fcast <- arx_fcast + nn_fcast #the forecast is the sum of the two predictions
        
        predict <- c(predict, fcast) #binding the earlier and new forecasts
        
      }
      
      MSE_hybrid <- ModelMetrics::mse(actual = y[(lisd+1):(lisd+losd)], predicted = predict) #MSE-value of the predicted values
      final <- rbind(final, c(dim, nodes, MSE_hybrid, predict)) #binding the most important information
      colnames(final) <- c("dim", "nodes", "MSE_hybrid", losd_number)
    }
  }
  
  #choosing the best model based on mse
  best_model <- final[which.min(final[,3]),]
  
  return(best_model)
}
