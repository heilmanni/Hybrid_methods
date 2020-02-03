#this function calculates an MSE value based on a hybrid method
#y is the result timeseries - it must be a ts or vector
#xreg is vector or matrix which contains the regressors
#lisd is the length of in_sample timeseries
#losd is the length of out_of_sample timeseries
#lisd and losd should be positive integers; lisd + losd cannot be longer than y
#phi is the optimal number of ar lag - the best arx model can be calculated with arx_forecast function, please use the same parameters as in the first part of analysis
#brake should be given between 0 and 1 - it refers to the proportion of the training set to the whole in the ann model, based on the residuals of arx model
#max_dim shows the maximum number of embedding dimensions for testing - it must be a positive integer; 0 to max_dim the results will be  calculated
#max_nodes shows the maximum number of nodes for testing - it must be a positive integer; 0 to max_nodes the results will be calculated
#required functions: arx_forecast, ann_forecast

hybrid_forecast <- function(y, xreg, lisd, losd, phi, brake, max_dim, max_nodes)
{
  #creating a vector for the forecasts
  predict <- c()
  
  for (pred in 0:(losd-1))
  {
    y_in_sample <- y[(1+pred):(lisd+pred)] #y has always the same length, but the start and ending points are different
    x_reg_in_sample <- xreg[(1+pred):(lisd+pred),] #x has always the same length, but the start and ending points are different
    
    
    #calculating the best parameters for the forecasting
    arx_model <- arx(y = y_in_sample, ar = phi, mxreg = x_reg_in_sample, mc = T) #an arx model with a given ar lag
    arx_res <- arx_model$residuals #residuals of the model
    
    #creating a test and training set at a brakepoint (which gives the proportion of the training set to the whole)
    border <- as.integer(length(arx_res)*brake)
    
    #searching for the best ann modell on the residuals of arx_model
    #required function: ann_forecast
    arx_res_ann <- ann_forecast(ltrain = border, ltest = (length(arx_res)-border), y = arx_res, 
                                xreg = x_reg_in_sample, max_dim = max_dim, max_nodes = max_nodes)
    
    #choosing the best model based on mse
    best_param <- arx_res_ann[which.min(arx_res_ann[,3]),]
    best_p <- as.numeric(best_param[1])
    best_size <- as.numeric(best_param[2])
    
    #forecasting with the given params
    arx_fcast <- as.numeric(forecast(object = fitted(arx_model), h = 1)[[2]]) #arx forecast for the next period
    nn_fcast <- as.numeric(forecast(object = fitted(nnetar(y = arx_res, xreg = x_reg_in_sample[(phi+1):lisd,],
                                                           p = best_p, size = best_size, repeats = 10)),
                                    h = 1)[[2]]) #nn forecast for the next period's residual
    fcast <- arx_fcast + nn_fcast #the forecast is the sum of two predictions
    
    predict <- c(predict, fcast) #binding the earlier and new forecasts
    
    print(pred)
  }
  
  MSE_hybrid <- mse(actual = y[(lisd+1):(lisd+losd)], predicted = predict)
  
  return((MSE_hybrid))
}
