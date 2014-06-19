best <- function(state, outcome) {
  # Invalid outcome
  outcome.index <- switch(outcome, `heart attack` = 11,
                                   `heart failure` = 17,
                                   `pneumonia` = 23,
                                   stop("invalid outcome"))

  data <- read.csv("outcome-of-care-measures.csv",
                   na.strings = "Not Available",
                   stringsAsFactors = FALSE)

  # Use %in% to properly handle NA
  data.state <- data[data$State %in% state, c(2, 7, outcome.index)]

  # Invalid state
  if (nrow(data.state) == 0) {
    stop("invalid state")
  }

  # which.min() returns the first one that matches min.
  # To get all that match min, use data.state[[3]] %in% min(data.state[[3]]).
  return(data.state[which.min(data.state[[3]]), 1])
}
