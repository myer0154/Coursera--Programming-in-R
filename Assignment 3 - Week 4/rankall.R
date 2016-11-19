## helper function to take a list of results (x), order them, and
## return the appropriate rank (num)
rankStateHelper <- function(x, num) {
	x <- x[order(x$Outcomes, x$Names),]
	if(num == "best") num <- 1
	else if(num == "worst") num <- length(x$Names)
	x <- x$Names[num]
	if(length(x) == 0)
		x <- NA
	return(x)
}

rankall <- function(outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcome_cols <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
	state_list <- sort(unique(data$State))
	state_list
	
	## Check that outcome is valid
	if(!outcome %in% names(outcome_cols))
		stop("invalid outcome")
	
	## build a data frame of hospital name, state, and selected outcome mortality
	hospitals <- suppressWarnings(data.frame(
		Names = data$Hospital.Name, 
		States = data$State,
		Outcomes = as.numeric(data[,outcome_cols[outcome]]),
		stringsAsFactors = FALSE))
	
	## strip out NA values, split by state, then apply the helper function to each state
	hospitals <- hospitals[!is.na(hospitals$Outcomes),]
	hospitals <- split(hospitals, hospitals$States)
	hospitals <- lapply(hospitals, rankStateHelper, num)
	
	## construct the output frame from the hospital names (unlisted) and states
	data.frame(hospital = unlist(hospitals), state = names(hospitals))
}