pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files.
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used.
        
        ## Return the mean of the pollutant across all monitors listed
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result.
        
        ## Read files
        ## Ignore missing values
        ## Compute mean
        
        id_length = 3 ## CONSTANT
        
        #Initialize data frame. Will append each file to it.
        df <- data.frame()
        
        for (i in id) {
                #Build file name
                pad <- paste(rep("0", id_length - nchar(i)), collapse = "")
                fil <- paste(pad, i, ".csv", sep = "")
                
                #Add directory
                path <- paste(directory, "/", fil, sep = "")
        
                #Read file
                df <- rbind(df, read.csv(path))
        }
        
        # Testing...
        # print(nrow(df)) # Number of rows. Don't use length() for that.
        # Output test file
        # write.csv(df, file = "test_pollutant_mean.csv")
        
        # Identify missing values
        pollutant_na = is.na(df[[pollutant]])
        
        # Compute mean
        pollutant_vals <- df[[pollutant]][!pollutant_na]
        # Note about subsetting.
        # The double square brackets pull out a vector. That vector can then be
        # indexed. In this case, I'm using a logical index. That's how to pick off
        # the present values from a column in a data frame.
        # mean(pollutant_vals)
        
        # I like knowing how to build a logical vector and then to subset a column
        # based on it. However, mean() already has a parameter for ignoring NA.
        mean(df[[pollutant]], na.rm = TRUE)
}
