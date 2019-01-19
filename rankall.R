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
        hospitals <- fil[!outcome_is_na, c(2, 7, outcome_col_pos)]
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        ## Initialize an accumlator table. Define the columns by setting them to empty vectors.
        ## You can't set the columns to NULL because that won't create a column.
        hospitals_out <- data.frame(hospital = character(), state = character())
        
        ## Get a list of the states. Do this with table() instead of split.
        ## Eventually you will remember that strings default to factors in data frames. Please set that to false.
        states <- as.data.frame( table( hospitals[2]), stringsAsFactors = FALSE)
        colnames(states) <- c("state", "freq")
        
        ## Assign rank for each state.
        for ( state in states$state ) {
                # print(state)

                # Pick off the hospitals for one state.
                hospitals_1_state <- hospitals[ hospitals[2] == state, ]
                
                # Order them by outcome and hospital name. Ties in outcome are broken
                # by the alphabetic ordering of the hospital name.
                hospitals_ordered <- order( hospitals_1_state[3], hospitals_1_state[1] )
                hospitals_1_state <- hospitals_1_state[ hospitals_ordered, ]
                
                # Rank them by appending a sequence column.
                hospitals_1_state$rank <- 1:nrow(hospitals_1_state)
                
                # Set the target rank based on the num parameter.
                if (num == "best") {
                        hospital_rank <- min(hospitals_1_state$rank)
                } else if (num == "worst") {
                        hospital_rank <- max(hospitals_1_state$rank)
                } else {
                        hospital_rank <- num
                }
                
                # Get the output for the target rank.
                # Initializing the output data frame is unnecessary because each loop iteration reassigns it.
                # Also, frustratingly, the assignment changes the column names anyway. Might as well assign
                # the output and then assign the names all over again.
                hospitals_1_state_output <- hospitals_1_state[ hospitals_1_state$rank == hospital_rank, 1:2]
                names(hospitals_1_state_output) <- c("hospital", "state")
                
                # If there are no hospitals found, insert NA for this state.
                # Important: appending to a data frame requires consistency of column names.
                if (nrow(hospitals_1_state_output) == 0) {
                        hospitals_1_state_output <- rbind(hospitals_1_state_output, data.frame(hospital = NA,
                                                                                               state = state))
                }
                
                hospitals_out <- rbind(hospitals_out, hospitals_1_state_output)
                
        }
        
        hospitals_out
        
}