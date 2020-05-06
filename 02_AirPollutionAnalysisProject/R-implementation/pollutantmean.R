pollutantmean <- function(directory, pollutant, id = 1:332)
{
    pollutatnts = c()  #empty vector to store pollutants
    filenames = list.files(directory) 
    
    for(i in id)
    {
      filepath = paste(directory, "/", filenames[i], sep="")
      data = read.csv(filepath, header = TRUE)
      pollutatnts = c(pollutatnts, data[,pollutant])
      
    }
    
    pollutatnts_mean = mean(pollutatnts, na.rm = TRUE)
    
    return(pollutatnts_mean)
    
}

