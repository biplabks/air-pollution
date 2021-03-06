complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    id <- as.integer(id)
    result <- data.frame(id = rep(NA, length(id)), nobs = rep(NA, length(id)))
    index <- 0
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
        index <- index+1
        data <- read.csv(temp)
        nobs <- complete.cases(data)
        correctNobs <- sum(nobs)
        result[index, ] <- c(i, correctNobs)
        temp <- ""
    }
    
    result
}