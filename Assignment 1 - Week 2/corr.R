## 'directory' is a character vector of length 1 indicating the location of the
## CSV files

## 'threshold' is a numeric vector of length 1 indicating the number of 
## completely observed observations (on all variables) required to computer the
## correlation between nitrate and sulfate; the default is zero

## Return a numeric vector of correlations

corr <- function(directory, threshold = 0) {
	filenames <- list.files(directory, full.names = T)
	cor_results <- vector("numeric")
	for(i in 1:length(filenames)) {
		data <- read.csv(filenames[i])
		data <- data[complete.cases(data),]
		if(nrow(data) > threshold) {
			cor_results <- c(cor_results, cor(data$sulfate, data$nitrate))
		}
	}
	cor_results
}