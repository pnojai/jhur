rankall <- function(outcome, num = "best") {
        # Initialization
        # Valid values for outcome and column positions for their data.
        # Store in a data frame and maintain here if the data code book changes.
        # outcome: valid outcome arguments.
        # pos: location in file of the 30-day mortality for the associated outcome.
        valid_outcomes <- data.frame(
                outcome = c("heart attack",
                            "heart failure",
                            "pneumonia"),
                pos = as.numeric(c(11,
                                   17,
                                   23))
        )
        
        # Read outcome data
        # The numeric values commingle with text saying "Not Available". The default behavior is to
        # read it as categorical data and assign them factors. If you try to coerce the factors to
        # numeric, it won't yield what you expect. The numeric value will not be the face value of
        # the data point, it will be the factor. Therefore, read the numeric data in as character
        # data so you can coerce it the way you want.
        fil <- read.csv("./hospdata/outcome-of-care-measures.csv", colClasses = "character")
        
        ## Scrub data
        ## Coerce mortality data to numeric
        ## Suppress warnings for conversions to NA
        for (pos in valid_outcomes[ , 2]) {
                suppressWarnings(fil[ , pos] <- as.numeric(fil[ , pos]))
        }
        
        ## Check outcome is valid
        if (sum(outcome == valid_outcomes[ , 1]) == 0) { stop("invalid outcome") }
        
        ## Set outcome position in file.
        ## Logical vectors are cool for showing what you want to pick off.
        outcome_lookup <- valid_outcomes[1] == outcome
        outcome_col_pos <- as.numeric(valid_outcomes[outcome_lookup][2])
        
        ## Subset the hospitals that report an outcome.
        outcome_is_na <- is.na( fil[, outcome_col_pos] )
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        hospitals <- fil[!outcome_is_na, c(2, 7, outcome_col_pos)]
        
        split(hospitals, hospitals[2])
}