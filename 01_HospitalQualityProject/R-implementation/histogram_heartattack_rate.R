data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data[,11] <- as.numeric(data[, 11])
outcome_data <- data[, 11]
hist(outcome_data, main = "The 30-day mortality rates for heart attack",
     xlab="Death rates from heart attack",
     col = "darkmagenta")