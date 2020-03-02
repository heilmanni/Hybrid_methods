#this function give a matrix as result which contains the summed errors in each timepoint
#res_data should be a matrix which contains residuals of time-series


sum_res <- function(res_data)
{
  ntm <- nrow(res_data) #number of rows is the number of the tested methods
  losd <- ncol(res_data) #number of columns is the length of out-of-sample data
  
  sum_res <- data.frame() #creating a dataframe
  
  for (j in 1:ntm)
  {
    for (i in 1:losd)
    {
      sum_res[j,i] <- sum(abs(res_GDP[j,1:i])) #sum the absolute value of the residuals in each row
    }
  }
  colnames(sum_res) <- c(1:losd)
  return(sum_res)
}
