# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. 
# The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome
# in that state. 
# The hospital name is the name provided in the Hospital.Name variable. 
# The outcomes can be one of "heart attack", "heart failure", or "pneumonia". 
# Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.

# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message "invalid state". If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message "invalid outcome".


best <- function(state, outcome)
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
  outcome_data <- data[, outcome]  #Read the outcome data
  
  
  
  #Return the best hospital
  index <- which.min(outcome_data)  #return index of the minmum value
  data[index, ]$name
}
  
  
 

