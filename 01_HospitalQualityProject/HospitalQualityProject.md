Introduction
------------

In this project I work with dataset from The [Hosptial Compare web
site](http://hospitalcompare.hhs.gov/) run by the U.S. Department of
Health and Human Services. The web site provide data and information
about the quality of care at over 4,000 Medicare-certied hospitals in
the U.S.

For this project we use only a subset of data which the web site
contains.

There are three files:

-   **outcome-of-care-measures.csv**: Contains information about 30-day
    mortality and readmission rates for heart attacks, heart failure,
    and pneumonia for over 4,000 hospitals.
-   **hospital-data.csv**: Contains information about each hospital.
-   **Hospital\_Revised\_Flatfiles.pdf**: Descriptions of the variables
    in each file (i.e the code book).

### what we are going to do in this assignment:

1- Plot the 30-day mortality rates for heart attack

2- Finding the best hospital in a state

3- Ranking hospitals by outcome in a state

4- Ranking hospitals in all states

————————————————————————————————-
---------------------------------

1-Histogram for the 30-day mortality rates for heart attack
-----------------------------------------------------------

In this step I read the data from outcome-of-care-measures.csv ,then
plot a histogram for column(11) which is contain the data for the 30-day
mortality rates for heart attack.

    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data[,11] <- as.numeric(data[, 11])

    ## Warning: NAs introduced by coercion

    outcome_data <- data[, 11]
    hist(outcome_data, main = "The 30-day mortality rates for heart attack",
         xlab="Death rates from heart attack",
         col = "darkmagenta")

![](HospitalQualityProject_files/figure-markdown_strict/unnamed-chunk-1-1.png)

————————————————————————————————
--------------------------------

2-Function returns the best hospital in a state
-----------------------------------------------

A function called “best” that take two arguments: the 2-character
abbreviated name of a state and an outcome name. The function reads the
outcome-of-care-measures.csv file and returns a character vector with
the name of the hospital that has the best (lowest) 30-day mortality for
the specified outcome in that state.

**Note:** The outcomes can be one of “heart attack”, “heart failure”, or
“pneumonia”.

### Implementation:

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
      return(data[index, ]$name)
    }

**Test cases:**

    best("TX", "heart attack")

    ## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"

    best("TX", "heart failure")

    ## [1] "FORT DUNCAN MEDICAL CENTER"

    best("BB", "heart attack")

    ## Error in best("BB", "heart attack"): invalid state

    best("NY", "hert attack")

    ## Error in best("NY", "hert attack"): invalid outcome

————————————————————————————————
--------------------------------

3-Function ranking hospitals by outcome in a state
--------------------------------------------------

A function called rankhospital that takes three arguments: the
2-character abbreviated name of a state (state), an outcome (outcome),
and the ranking of a hospital in that state for that outcome (num).

The function returns a character vector with the name of the hospital
that has the ranking specified by the num argument.

### Implementation:

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

**Test cases:**

    rankhospital("TX", "heart failure", 4)

    ## [1] "ATLANTA MEMORIAL HOSPITAL"

    rankhospital("MD", "heart attack", "worst")

    ## [1] "HARFORD MEMORIAL HOSPITAL"

    rankhospital("MN", "heart attack", 5000)

    ## [1] NA

————————————————————————————————
--------------------------------

4-Function ranking hospitals in all states
------------------------------------------

A function called rankall that takes two arguments: an outcome name
(outcome) and a hospital ranking (num), returns a 2-column data frame
containing the hospital in each state that has the ranking specified in
num.

### Implementation

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

**Test cases:**

    head(rankall("heart attack", 20), 10)

    ##                                  hospital state
    ## 12                                   <NA>    AK
    ## 27                 FAYETTE MEDICAL CENTER    AL
    ## 23 NORTH ARKANSAS REGIONAL MEDICAL CENTER    AR
    ## 22    JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
    ## 8       CENTINELA HOSPITAL MEDICAL CENTER    CA
    ## 24          NORTH COLORADO MEDICAL CENTER    CO
    ## 42             ROCKVILLE GENERAL HOSPITAL    CT
    ## 51                                   <NA>    DC
    ## 38                                   <NA>    DE
    ## 34                CAPE CANAVERAL HOSPITAL    FL

    tail(rankall("pneumonia", "worst"), 3)

    ##                                      hospital state
    ## 23 MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
    ## 46                     PLATEAU MEDICAL CENTER    WV
    ## 49           NORTH BIG HORN HOSPITAL DISTRICT    WY

    tail(rankall("heart failure"), 10)

    ##                                                             hospital state
    ## 34                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
    ## 4                                         FORT DUNCAN MEDICAL CENTER    TX
    ## 19 VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
    ## 38                                          SENTARA POTOMAC HOSPITAL    VA
    ## 52                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
    ## 47                                              SPRINGFIELD HOSPITAL    VT
    ## 36                                         HARBORVIEW MEDICAL CENTER    WA
    ## 22                                    AURORA ST LUKES MEDICAL CENTER    WI
    ## 46                                         FAIRMONT GENERAL HOSPITAL    WV
    ## 49                                        CHEYENNE VA MEDICAL CENTER    WY
