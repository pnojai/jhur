complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files.
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used.
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases (number of observations).
        
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

        # Identify complete cases
        df_is_complete <- complete.cases(df$sulfate, df$nitrate)
        
        # Testing
        df[df_is_complete, ]
}
