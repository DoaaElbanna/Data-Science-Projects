# Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital 
# ranking (num).
# The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num.

# For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates.

# The function should return a value for every state (some may be NA).
# Thefirst column in the data frame is named hospital, which contains
# the hospital name, and the second column is named state, which contains the 2-character abbreviation for
# the state name

# NOTE: For the purpose of this part of the assignment (and for eficiency), your function should NOT call
# the rankhospital function from the previous section.


rankall <- function(outcome, num = "best")
{
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #First check outcome if is valid
  expected_outcome <- c("heart attack", "heart failure", "pneumonia")
  check_outcome <- outcome %in% expected_outcome
  if (check_outcome == FALSE )
  {
    stop("invalid outcome")
  }
  
  #change columns names
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  data <- data[data[outcome] != "Not Available",]

  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data$name, data[outcome] ,decreasing = FALSE), ]
  
  #Helper Function
  hospital_rank <- function(df, s, n) 
  {
    df <- df[df$state==s, ]
    outcome_data <- df[,outcome]
    if( n == "best" ) {
      index <- which.min(outcome_data)
    }else if( n == "worst" ) {
      index <- which.max(outcome_data)
    }else {
      index <- n
    }
    df[index, ]$name
  }
  
  ## For each state, find the hospital of the given rank
  states <- data[, 2]
  states <- unique(states)
  ranking_data <- data.frame(hospital = c(), state = c())
  for(st in states) {
    hosp <- hospital_rank(data, st, num)
    ranking_data <- rbind(ranking_data, data.frame(hospital = hosp, state = st))
  }

  ## Return a data frame(arranged) with hospital name and state name
  ranking_data <- ranking_data[order(ranking_data['state'], decreasing = FALSE), ]
  return(ranking_data)
  
}