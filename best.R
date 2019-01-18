best <- function(state, outcome) {
        # Initialization
        valid_outcomes <- c("heart attack", "heart failue", "pneumonia")
        
        outcome_col_pos_heart_attack <- 11
        outcome_col_name_heart_attack <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        outcome_col_pos_heart_failure <- 17
        outcome_col_name_heart_failure <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        outcome_col_pos_pneumonia <- 23
        outcome_col_name_pneumonia <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"

        ## Check that state and outcome are valid
        if (sum(fil[7] == state) == 0) { stop("invalid state") }
        if (sum(outcome == valid_outcomes) == 0) { stop("invalid outcome") }
        
        ## Read outcome data
        fil <- read.csv("./hospdata/outcome-of-care-measures.csv")
   
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
}