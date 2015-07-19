pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
    
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
    
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
    
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
    resultVec <- c()
    id <- as.integer(id)
    for(i in id)
    {
        if(i < 10)
        {
            temp <- paste("00", i, sep = "")
            temp <- paste(temp, ".csv", sep = "")
            temp <- paste(directory, temp, sep = "")
        }
        else if(i < 100)
        {
            temp <- paste("0", i, sep = "")
            temp <- paste(temp, ".csv", sep = "")
            temp <- paste(directory, temp, sep = "")
        }
        else
        {
            temp <- paste(directory, i, sep = "")
            temp <- paste(temp, ".csv", sep = "")
        }
        
        data <- read.csv(temp)
        
        if(pollutant == "sulfate")
        {
            polVec <- c(data[, 2])
        }
        else if(pollutant == "nitrate")
        {
            polVec <- c(data[, 3])
        }
        resultVec <- c(resultVec, polVec)
        temp <- ""
    }
    result <- mean(resultVec, na.rm = TRUE)
    result
}