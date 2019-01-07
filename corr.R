corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files.
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of complete observations (on all variables) required
        ## to compute the correlation between nitrate and sulfate; the
        ## default is 0.
        
        ## return a vector of numeric correlations.
        ## Note: do not round the result.
        
        id_length <- 3  ## CONSTANT. String length for the data file names.
        id <- 1:332     ## CONSTANT. Sequence of data monitors with files.
        
        #Initialize a vector to contain the correlations.
        corr_vect <- vector("numeric", 0)
        
        for (i in id) {
                #Build file name
                pad <- paste(rep("0", id_length - nchar(i)), collapse = "")
                fil <- paste(pad, i, ".csv", sep = "")
                
                #Add directory
                path <- paste(directory, "/", fil, sep = "")
        
                #Read file. One time use. Process each file separately
                df <- read.csv(path)
                
                # Approach #1: Scrub the incomplete cases before computing.
                # Approach #2: Use the filtering in cor() to ignore incomplete cases
                # Toggle is_scrub to choose.
                # Tested both, and each works.
                
                is_scrub <- 0
                
                if (is_scrub == 1) { # then scrub incomplete cases
                        # Identify complete cases.
                        df_is_complete <- complete.cases(df$sulfate, df$nitrate)
                        df_scrub <- df[df_is_complete, ]
                        
                        if (nrow(df_scrub) > threshold) {
                                # Compute correlation.
                                result <- cor(x = df_scrub[ , 2 ], y = df_scrub[ , 3 ])
                                
                                # Append result to output vector.
                                corr_vect <- c(corr_vect, result)
                        }
                } else {
                        if (sum(complete.cases(df)) > threshold) {
                                # Compute correlation
                                result <- cor(x = df[ , 2], y = df[ , 3], use = "complete.obs")
                        
                                # Append result to output vector.
                                corr_vect <- c(corr_vect, result)
                        }
                }
                
                # break # Just do one file to test
        }
        corr_vect
}
