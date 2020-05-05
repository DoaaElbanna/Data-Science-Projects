# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument.
# If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA.



rankhospital <- function(state, outcome, num = "best")
{
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #First check outcome if is valid
  expected_outcome <- c("heart attack", "heart failure", "pneumonia")
  check_outcome <- outcome %in% expected_outcome
  if (check_outcome == FALSE )
  {
    stop("invalid outcome")
  }
  #check state if is valid 
  check_state <- state %in% data$State
  if (check_state == FALSE )
  {
    stop("invalid state")
  }
  
  
  #change columns names
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  data <- data[data$State == state & data[outcome] != "Not Available",]
  
  #Ranking
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data$name, data[outcome] ,decreasing = FALSE), ]

  outcome_data <- data[, outcome]  #Read the outcome data
  
  if( num == "best" ) {
    index <- which.min(outcome_data)
  } else if( num == "worst" ) {
    index <- which.max(outcome_data)
  } else {
    index <- num
  }
  
  return(data[index, ]$name)
  
}