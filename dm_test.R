#this function ...

dm_test <- function(imput_matrix_1, imput_matrix_2, alternative)
{
  nrow <- nrow(imput_matrix_1)
  options <- rbind(c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4))
  
  #matrix for the results
  dm_eredmeny <- data.frame()
  
  for (i in 1:nrow(options))
  {
    
    dm_eredmeny[i, 1] <- as.numeric(forecast::dm.test(e1 = imput_matrix_1[options[i,1],], e2 = imput_matrix_1[options[i,2],], alternative = alternative)[[4]])
    dm_eredmeny[i, 2] <- as.numeric(forecast::dm.test(e1 = imput_matrix_2[options[i,1],], e2 = imput_matrix_2[options[i,2],], alternative = alternative)[[4]])
    #row.names(dm_eredmeny[i, ]) <- knitr::combine_words(words = cbind(row.names(imput_matrix_1[options[i,1],])[1], row.names(imput_matrix_1[options[i,2],])[2]), and = " - ")
  }
  
  colnames(dm_eredmeny) <- c("GDP deflator", "PCE deflator")
  row.names(dm_eredmeny) <- c("ARX - ANN", "ARX - Hybrid", "ARX - Real", "ANN - Hybrid", "ANN - Real", "Hybrid - Real")
  
  return(format(round(dm_eredmeny, 4)))
  
}


#options <- (nrow-1)*(nrow-2)
#options <- gimme::expand.grid.unique(p = 1:2, q = 1:2)
