#this function shows the followings: mean, variance, p-value of the ADF-test
#as an input, a matrix (or vector) should be given which contains time-series
#the variables should be in the columns, the time should be in the rows

statistics <- function(input)
{
  final <- data.frame() #creating a data frame for the final results
  ncol_input <- ncol(input)
  
  for (i in 1:ncol_input) {
    
    final[1, i] <- format(round(min(input[,i]), 2)) #minimum
    final[2, i] <- format(round(max(input[,i]), 2)) #maximum
    final[3, i] <- format(round(mean(input[,i]), 2)) #mean
    final[4, i] <- format(round(sd(input[,i]), 2)) #standard deviaton
    final[5, i] <- format(round(tseries::adf.test(input[,i])[[4]], 4)) #p-value of the ADF-test
    final[6, i] <- forecast::ndiffs(x = input[,i], test = "adf") #order of the integration
  }
  
  row.names(final) <- c("min", "max", "mean", "sd", "ADF p", "ndiff") #name of the rows
  colnames(final) <- colnames(input) #name of the columns
  
  return(final)
}