## 'directory' is a character vector of length 1 indicating the location of the
## CSV files

## 'id' is an integer vector indicating the monitor ID numbers to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the number of complete data

complete <- function(directory, id = 1:332) {
	filenames <- list.files(directory, full.names = T)
	complete_frame <- data.frame()
	for(i in id) {
		data <- read.csv(filenames[i])
		data <- data[complete.cases(data),]
		complete_frame <- rbind(complete_frame, data.frame(id = i, nobs = nrow(data)))
	}
	complete_frame
}