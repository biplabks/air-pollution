corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    data <- complete(directory)
    
    upThreshold <- subset(data, data$nobs > threshold)
    id <- c(upThreshold[,1])
    result <- c()
    
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
        tempData <- read.csv(temp)
        tempData <- na.omit(tempData)
        corrResult <- cor(tempData$sulfate, tempData$nitrate)
        result <- c(result, corrResult)
    }
    
    result
}