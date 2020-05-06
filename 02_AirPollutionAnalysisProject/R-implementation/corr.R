corr <- function(directory, threshold = 0)
{
  filename <- list.files(directory)
  correlations <- c()
  
  for (i in seq_along(filename))
  {
    filepath <- paste(directory, "/", filename[i], sep="")
    data = read.csv(filepath, header = TRUE)
    complete_data <- data[complete.cases(data),]
    count_cases <- nrow(complete_data)
    if(count_cases >= threshold)
    {
      correlations <- c(correlations,(cor(complete_data$nitrate, complete_data$sulfate)))
    }
    
  }
  
  correlations
  
}