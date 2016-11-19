## 'best' reads the hospital outcome data from an outcomes file and determines 
## the best hospital in the state for illness 'outcome' based on lowest 30-day 
## mortality rate

## 'state' is a two-character abbreviation for the state of interest
## 'outcome' is the type of event (e.g., "heart attack", "pneumonia", etc.)

best <- function(state, outcome) {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
	outcome_cols <- c(11, 17, 23)
	names(outcome_cols) <- valid_outcomes
	
	## Check that 'state' and 'outcome' are valid
	if(!state %in% data$State)
		stop("invalid state")
	if(!outcome %in% valid_outcomes)
		stop("invalid outcome")
	
	## create a data frame with Names = hospital names from state, 
	hospitals <- suppressWarnings(data.frame(
		Names = as.character(data$Hospital.Name[data$State == state]), 
		Outcomes = as.numeric(data[,outcome_cols[outcome]][data$State == state]),
		stringsAsFactors = FALSE))
	## strip out NAs from the Outcomes column
	hospitals <- hospitals[!is.na(hospitals$Outcomes),]
	
	## Return hospital name in that state with lowest 30-day death rate
	best_hospitals <- hospitals[hospitals$Outcomes == min(hospitals$Outcomes),1]
	## In case of ties, return first alphabetical name
	sort(best_hospitals)[1]
	
	
}