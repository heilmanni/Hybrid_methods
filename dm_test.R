#this function gives the p-values of the Diebold-Mariano test and the p-values of the ADF test of loss differntials
#the 2 imputmatrix should be given as a matrix, with four columns (ARX, ANN, Averaging, Hybrid) 
#number of the rows of the two matrixes has to be the same
#the alternative denotes the alternative hypothesis at the Diebold-Mariano test; options: "two.sided", "less", "greater"

dm_test <- function(imput_matrix_1, imput_matrix_2, alternative)
{
  nrow <- nrow(imput_matrix_1) #number of the rows
  options <- rbind(c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4)) #all of the combination to compare each model
  
  #matrix for the results
  dm_eredmeny <- data.frame()
  
  for (i in 1:nrow(options))
  {
    
    dm_eredmeny[i, 1] <- as.numeric(forecast::dm.test(e1 = imput_matrix_1[options[i,1],], e2 = imput_matrix_1[options[i,2],], alternative = alternative)[[4]]) #p-value of the DM-test
    dm_eredmeny[i, 2] <- as.numeric(forecast::dm.test(e1 = imput_matrix_2[options[i,1],], e2 = imput_matrix_2[options[i,2],], alternative = alternative)[[4]]) #p-value of the DM-test
    dm_eredmeny[i, 3] <- tseries::adf.test(imput_matrix_1[options[i,1],]^2 - imput_matrix_1[options[i,2],]^2)[[4]]
    dm_eredmeny[i, 4] <- tseries::adf.test(imput_matrix_2[options[i,1],]^2 - imput_matrix_2[options[i,2],]^2)[[4]]

  }
  
  colnames(dm_eredmeny) <- c("DM GDP", "DM PCE", "ADF GDP", "ADF PCE")
  row.names(dm_eredmeny) <- c("ARX - ANN", "ARX - Averaging", "ARX - Hybrid", "ANN - Averaging", "ANN - Hybrid", "Averaging - Hybrid")
  
  return(format(round(dm_eredmeny, 4)))
  
}
