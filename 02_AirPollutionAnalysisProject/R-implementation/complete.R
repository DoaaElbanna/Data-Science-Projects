complete <- function(directory, id=1:332)
{
  nobs <- c()  #store number of complete cases
  ids <- c()
  filename <- list.files(directory)
  for(i in id)
  {
    filepath <- paste(directory, "/", filename[i], sep="")
    data <- read.csv(filepath, header = TRUE)
    # Store the complete cases subset in a new data frame
    data_complete <- data[complete.cases(data),]
    ids <- c(ids, i)
    nobs <- c(nobs, nrow(data_complete))

  }
  
  return(data.frame(id=id, nobs=nobs))
}