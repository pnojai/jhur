# 1. Plot the 30-day mortality rate for heart attacks.
outcome <- read.csv("./hospdata/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

ncol(outcome)
nrow(outcome)
names(outcome)

# 30-day death rate
outcome[ , 11] <- as.numeric(outcome[ , 11])
hist(outcome[ , 11])

# 2. Finding the best hospital by state.
