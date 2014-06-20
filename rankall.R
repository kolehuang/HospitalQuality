rankall <- function(outcome, num = "best") {
  answer <- data.frame()  
## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",
                   na.strings = "Not Available",
                   stringsAsFactors = FALSE)  

## Check that state and outcome are valid
  outcome.index <- switch(outcome, `heart attack` = 11,
                                   `heart failure` = 17,
                                   `pneumonia` = 23,
                                   stop("invalid outcome"))
  data <- data[, c(2, 7, outcome.index)]  # not yet handling NA
  data <- na.omit(data.state)
  data <- data[order(data$State, data[,3]), ]
  #bystate <- split(data, data$State)
  if (nrow(data) == 0) {
    stop("invalid state") 
  }

## For each state, find the hospital of the given rank
  #call.bystate <- bystate[do.call(order, bystate), ]
  #hos.ranking <- call.bystate[order(call.state[, 3]), 1:2]
  #answer <- hos.ranking[num, ]

  for (i in unique(data$State)) {
    bystate <- data[data$State %in% i, c(1, 2)]
    final <- bystate[num, ]
    answer <- rbind(answer, final)
  }
  #if (num == "best") {
  #  answer <- hos.ranking[1]
  #} else if (num == "worst") {
  #  hos.ranking <- data.state[order(data.state[, 3], decreasing = TRUE), 1]
  #  answer <- hos.ranking[1]
  #} else {
  #  answer <- hos.ranking[num] 
  #}   
  return(answer)
}