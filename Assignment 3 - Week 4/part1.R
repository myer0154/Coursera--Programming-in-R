heart_attack_hist <- function() {
	outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcome[,11] <- as.numeric(outcome[,11])
	hist(outcome[,11], main = "30-Day Death Rate from Heart Attacks",
		 xlab = "Mortality Rate")
}