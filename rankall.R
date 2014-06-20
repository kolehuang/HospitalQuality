rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",
                     na.strings = "Not Available",
                     stringsAsFactors = FALSE)  
  
    ## Check that state and outcome are valid
    outcome.index <- switch(outcome, `heart attack` = 11,
                                     `heart failure` = 17,
                                     `pneumonia` = 23,
                                     stop("invalid outcome"))
    data <- data[, c(2, 7, outcome.index)] 
    #data <- na.omit(data.state)
    data <- data[order(data$State, data[[3]], data[[1]], na.last = NA), ]
    #bystate <- split(data, data$State)
    if (nrow(data) == 0) {
        stop("invalid state") 
    }

    ## For each state, find the hospital of the given rank
    answer <- data.frame()  
    for (i in unique(data$State)) {
        bystate <- data[data$State %in% i, 1:2]
        if (num == "best") {
            final <- bystate[1, ]
        } else if (num == "worst") { 
            final <- tail(bystate, 1)
        } else {
            final <- bystate[num, ]
        }
        final$State <- i
        answer <- rbind(answer, final)
    }
    names(answer) <- c("hospital", "state")   
    return(answer)
}