## rankhospital returns the hospital in 'state' with the 'num' rank in 30-day
## mortality for disease 'outcome'
## 'state' is a two-character abbreviation of the state
## 'outcome' is "heart attack", "heart failure", or "pneumonia"
## 'num' takes values of "best", "worst", or a ranking number

rankhospital <- function(state, outcome, num = "best") {
	## read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
	outcome_cols <- c(11, 17, 23)
	names(outcome_cols) <- valid_outcomes
	
	## validate state and outcome values
	if(!state %in% data$State)
		stop("invalid state")
	if(!outcome %in% valid_outcomes)
		stop("invalid outcome")## create a data frame with Names = hospital names from state, 
	hospitals <- suppressWarnings(data.frame(
		Names = as.character(data$Hospital.Name[data$State == state]), 
		Outcomes = as.numeric(data[,outcome_cols[outcome]][data$State == state]),
		stringsAsFactors = FALSE))
	## strip out NAs from the Outcomes column
	hospitals <- hospitals[!is.na(hospitals$Outcomes),]
	hospitals <- hospitals[order(hospitals$Outcomes, hospitals$Names),]
	hospitals <- data.frame(hospitals, Rank = seq_along(hospitals$Names))
	
	## Convert the special values 'best' and 'worst' to the appropriate ranks
	if(num == "best") num <- 1
	else if(num == "worst") num <- max(hospitals$Rank)
	
	## return the result, or NA if an invalid 'num' has been entered
	result <- hospitals[hospitals$Rank == num,]$Names
	if(length(result) == 0)
		return(NA)
	else
		result
}
	
	