pollutantmean <- function(directory, pollutant, id = 1:332) {
	filenames <- list.files(file.path(".", directory), full.names = T)[id]
	pollut.count <- pollut.sum <- 0
	for(i in 1:length(filenames)) {
		data <- read.csv(filenames[i])
		data <- data[pollutant]
		data <- data[!is.na(data)]
		pollut.count <- pollut.count + length(data)
		pollut.sum <- pollut.sum + sum(data)
	}
	pollut.sum/pollut.count
}