Introduction
------------

In this project I created three functions that interact with a dataset
is contained in a zip file **specdata.zip**.

The zip file contains 332 comma-separated-value (CSV) files containing
pollution monitoring data for fine particulate matter (PM) air pollution
at 332 locations in the United States. Each file contains data from a
single monitor and the ID number for each monitor is contained in the
file name. For example, data for monitor 200 is contained in the file
“200.csv”. Each file contains three variables:

-   **Date**: the date of the observation in YYYY-MM-DD format
    (year-month-day)
-   **sulfate**: the level of sulfate PM in the air on that date
    (measured in micrograms per cubic meter)
-   **nitrate**: the level of nitrate PM in the air on that date
    (measured in micrograms per cubic meter)

**Note:**In each file you’ll notice that there are many days where
either sulfate or nitrate (or both) are missing (coded as NA). This is
common with air pollution monitoring data in the United States.

Part1:
------

The function named “pollutantmean” that calculates the mean of a
pollutant (sulfate or nitrate) across a specified list of monitors, it
takes three arguments: ‘directory’, ‘pollutant’, and ‘id’

-   **directory:** character verctor of length 1 indicates the location
    of data(CSV files).
-   **pollutant:** character vector of length 1 indicates the name of
    pollutant which we want to calculate the mean, either **nitrate** or
    **sulfate**
-   **id:** integer vector that indicates the ID numbers to be used

**The Return of this function:** Return the mean of pollutant across all
monitors list in the “id” vector(Ignoring the NA values)

### implementaion

    pollutantmean <- function(directory, pollutant, id = 1:332)
    {
        pollutatnts = c()  #empty vector to store pollutants
        filenames = list.files(directory)  #list all files in the passed directory 
        
        for(i in id)
        {
          filepath = paste(directory, "/", filenames[i], sep="")
          data = read.csv(filepath, header = TRUE)
          pollutatnts = c(pollutatnts, data[,pollutant])
          
        }
        
        pollutatnts_mean = mean(pollutatnts, na.rm = TRUE)
        
        return(pollutatnts_mean)
        
    }

**Test cases:**

    pollutantmean("specdata", "sulfate", 1:10)

    ## [1] 4.064128

    pollutantmean("specdata", "nitrate", 50:70)

    ## [1] 0.8479408

—————————————————————————–
--------------------------

Part2:
------

The function named “complete”, that reads a directory full of files and
returns the number of complete cases in each data file.

**Takes two arguments**:

-   **directory:** haracter verctor of length 1 indicates the location
    of data(CSV files).

-   **id:** integer vector that indicates the ID numbers to be used.

**The Return of this function:** The function should return a data frame
where the first column is the name of the file and the second column is
the number of complete cases.

### Implementation

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

**Test cases**

    complete("specdata", 1)

    ##   id nobs
    ## 1  1  117

    complete("specdata", c(2, 5, 8, 7, 12))

    ##   id nobs
    ## 1  2 1041
    ## 2  5  402
    ## 3  8  192
    ## 4  7  442
    ## 5 12   96

    complete("specdata", 30:25)

    ##   id nobs
    ## 1 30  932
    ## 2 29  711
    ## 3 28  475
    ## 4 27  338
    ## 5 26  586
    ## 6 25  463

—————————————————————————–
--------------------------

Part3
-----

This function named “corr”, it takes a directory of data files and a
threshold for complete cases and calculates the correlation between
sulfate and nitrate for monitor locations where the number of completely
observed cases (on all variables) is greater than the threshold.

**Takes two arguments**:

-   **directory:** haracter verctor of length 1 indicates the location
    of data(CSV files).

-   **threshold:** is a numeric vector of length 1 indicating the number
    of completely observed observations, **Note:** Required to compute
    the correlation between nitrate and sulfate

**The Return of this function:** Return a numeric vector of correlations
If no monitors meet the threshold requirement, then the function should
return a numeric vector of length 0.

### Implementation

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

**Test cases**

    cr <- corr("specdata", 150)
    head(cr)

    ## [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814

    summary(cr)

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -0.21057 -0.05147  0.09333  0.12401  0.26836  0.76313

    cr <- corr("specdata", 5000)
    summary(cr)

    ## Length  Class   Mode 
    ##      0   NULL   NULL

    length(cr)

    ## [1] 0
