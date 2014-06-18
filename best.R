best <- function(state, outcome) {

	# invalid outcome 
	if (outcome == "heart attack") { 
		outcome.index <- 11
	} else if (outcome == "heart failure") {
		outcome.index <- 17
	} else if (outcome == "pneumonia") {
		outcome.index <- 23
	} else {
		stop("invalid outcome")
	}

	data <- read.csv("outcome-of-care-measures.csv", 
					 na.strings = "Not Available",
					 stringsAsFactors = FALSE)
					 
	# data.state <- data[data$State == state, c(2, 7, outcome.index)]
	data.state <- na.omit(data[data$State == state, c(2, 7, outcome.index)])

	# invalid state							
	if (nrow(data.state) == 0) { 
  		stop("invalid state")
  	}

	# answer <- subset(data.state, data.state[[3]] == min(data.state[[3]], na.rm = TRUE))
	answer <- data.state[data.state[[3]] == min(data.state[[3]]), 1]	

	return(answer[1]) 
}
