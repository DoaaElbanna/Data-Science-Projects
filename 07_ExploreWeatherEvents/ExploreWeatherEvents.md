1.Synopsis
----------

Storms and other severe weather events can cause both public health and
economic problems for communities and municipalities. Many severe events
can result in fatalities, injuries, and property damage, and preventing
such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and
Atmospheric Administration’s (NOAA) storm database. This database tracks
characteristics of major storms and weather events in the United States,
including when and where they occur, as well as estimates of any
fatalities, injuries, and property damage.

------------------------------------------------------------------------

2.Data Processing
-----------------

### 2.1 Data

-   The data for this project come in the form of a
    comma-separated-value file compressed via the bzip2 algorithm to
    reduce its size.

-   You can download the [Storm
    Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2).

-   If you need more information about the Storm database:

    -   National Weather Service [Storm Data
        Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf).

    -   National Climatic Data Center Storm Events
        [FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf).

-   The events in the database start in the year 1950 and end in
    November 2011.

-   In the earlier years of the database there are generally fewer
    events recorded, most likely due to a lack of good records,More
    recent years should be considered more complete.

### 2.2 Data Processing

#### 2.2.1 Loading the Data

Loading neccessarry packages.

```r
    library(dplyr)
```    
    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

```r
    library(ggplot2)
    library(gridExtra)
```
    ## Warning: package 'gridExtra' was built under R version 3.6.2

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

Loading DataSet

```r
    before <- Sys.time()  # To determine the time before reading data
    storm_data <- read.csv("repdata_data_StormData.csv")
    after <- Sys.time()  #  determine the time after reading data

    loading_time <- after - before

    print(paste("The time was taking to reading data is ", loading_time))
```
    ## [1] "The time was taking to reading data is  1.36408506631851"

#### 2.2.2

#### Data processing

```r
    head(storm_data)
```
    ##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
    ## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
    ## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
    ## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
    ## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
    ## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
    ## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
    ##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
    ## 1 TORNADO         0                                               0
    ## 2 TORNADO         0                                               0
    ## 3 TORNADO         0                                               0
    ## 4 TORNADO         0                                               0
    ## 5 TORNADO         0                                               0
    ## 6 TORNADO         0                                               0
    ##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
    ## 1         NA         0                      14.0   100 3   0          0
    ## 2         NA         0                       2.0   150 2   0          0
    ## 3         NA         0                       0.1   123 2   0          0
    ## 4         NA         0                       0.0   100 2   0          0
    ## 5         NA         0                       0.0   150 2   0          0
    ## 6         NA         0                       1.5   177 2   0          0
    ##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
    ## 1       15    25.0          K       0                                    
    ## 2        0     2.5          K       0                                    
    ## 3        2    25.0          K       0                                    
    ## 4        2     2.5          K       0                                    
    ## 5        2     2.5          K       0                                    
    ## 6        6     2.5          K       0                                    
    ##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
    ## 1     3040      8812       3051       8806              1
    ## 2     3042      8755          0          0              2
    ## 3     3340      8742          0          0              3
    ## 4     3458      8626          0          0              4
    ## 5     3412      8642          0          0              5
    ## 6     3450      8748          0          0              6

In this analysis, we only focus on analyzing the effects of weather
events to public health and economy, So we Select columns related to
these.

```r
    storm_data <- storm_data[, c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", 
        "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
```

### First, According United States We want to to determine which types of events are most harmful with respect to population health?

**The following lines of code summarize the injury and fatality data per
event.**

```r
    event_fatality <- storm_data %>% group_by(EVTYPE) %>% summarize(sum(FATALITIES,na.rm=TRUE))
    colnames(event_fatality) <- c("EVTYPE","FATALITIES")
    event_injuries <- storm_data %>% group_by(EVTYPE) %>% summarize(sum(INJURIES,na.rm=TRUE))
    colnames(event_injuries) <- c("EVTYPE","INJURIES")
```

**Then rearrange the data in descending order to determine the most
harmful events.**

```r
    event_fatality <- event_fatality[order(event_fatality$FATALITIES,decreasing = TRUE),]
    event_injuries <- event_injuries[order(event_injuries$INJURIES,decreasing = TRUE),]
```
#### Results

**Getting and Plotting the top 7 causes of fatality and injury.**

```r
    event_fatality <- event_fatality[1:7,]
    event_injuries <- event_injuries[1:7,]
    injPlot <- ggplot(event_injuries) + geom_bar(aes(x = reorder(EVTYPE, -INJURIES), y = INJURIES,        
        fill=interaction(INJURIES,EVTYPE)), show.legend=FALSE, stat="identity") + 
        xlab("Event")+ylab("Injuries")+ggtitle("Top 7 Events Causing Injury") + scale_fill_brewer(palette="Spectral")

    fatPlot <- ggplot(event_fatality)+ geom_bar(aes(x = reorder(EVTYPE, -FATALITIES), y = FATALITIES, 
        fill=interaction(FATALITIES,EVTYPE)),show.legend=FALSE,stat="identity") + 
        xlab("Event")+ylab("Fatalities")+ggtitle("Top 7 Events Causing Fatalities") +     
        scale_fill_brewer(palette="Spectral")
    grid.arrange(fatPlot,injPlot)
```
![](https://github.com/DoaaElbanna/Data-Science-Projects/blob/master/07_ExploreWeatherEvents/graphs/Plot1.png)

**NOTE:From the plot we noticed the “Tornados” are the leading cause of
injuries and death.**

------------------------------------------------------------------------

### Second, We want to determine which types of events have the greatest economic consequences?

#### Data Processing

#### Converting economic data exponents to values then multiplying it to actual values.

```r
    storm_data$PROPDMGEXP <- gsub("m",1000000,storm_data$PROPDMGEXP,ignore.case = TRUE)
    storm_data$PROPDMGEXP <- gsub("k",1000,storm_data$PROPDMGEXP,ignore.case = TRUE)
    storm_data$PROPDMGEXP <- gsub("b",1000000000,storm_data$PROPDMGEXP,ignore.case = TRUE)
    storm_data$CROPDMGEXP <- gsub("m",1000000,storm_data$CROPDMGEXP,ignore.case=TRUE)
    storm_data$CROPDMGEXP <- gsub("k",1000,storm_data$CROPDMGEXP,ignore.case=TRUE)
    storm_data$CROPDMGEXP <- gsub("b",1000000000,storm_data$CROPDMGEXP,ignore.case=TRUE)
    storm_data_complete_case <- storm_data[complete.cases(storm_data),]
    storm_data_complete_case$CROPDMGEXP <- as.numeric(storm_data_complete_case$CROPDMGEXP)
```
    ## Warning: NAs introduced by coercion
```r
    storm_data_complete_case$PROPDMGEXP <- as.numeric(storm_data_complete_case$PROPDMGEXP)
```
    ## Warning: NAs introduced by coercion
```r
    storm_data_complete_case$TOTALPROPDMG <- storm_data_complete_case$PROPDMG * storm_data_complete_case$PROPDMGEXP
    storm_data_complete_case$TOTALCROPDMG <- storm_data_complete_case$CROPDMG * storm_data_complete_case$CROPDMGEXP
```
**Summarizing and re-organizing data to prepare it for Plotting.**

```r

    storm_data_complete_case_prop <- storm_data_complete_case %>% 
    group_by(EVTYPE) %>%         summarize(grandtotalprop=sum(TOTALPROPDMG,na.rm=TRUE))
    storm_data_complete_case_crop <- storm_data_complete_case %>% 
    group_by(EVTYPE) %>% summarize(grandtotalcrop=sum(TOTALCROPDMG,na.rm=TRUE))
    total_eco_dmg <- full_join(storm_data_complete_case_crop,storm_data_complete_case_prop,by="EVTYPE")
    total_eco_dmg$Total <- total_eco_dmg$grandtotalcrop+total_eco_dmg$grandtotalprop
    total_crop_dmg <- total_eco_dmg[order(total_eco_dmg$grandtotalcrop,decreasing=TRUE),]
    total_crop_dmg <- total_crop_dmg[1:7,]
    total_prop_dmg <- total_eco_dmg[order(total_eco_dmg$grandtotalprop,decreasing = TRUE),]
    total_prop_dmg <- total_prop_dmg[1:7,]
    total_eco_dmg <- total_eco_dmg[order(total_eco_dmg$Total,decreasing = TRUE),]
    total_eco_dmg <- total_eco_dmg[1:7,]
```

### Results

**Plotting and seeing top events causing the most damage economically**

```r
    cropPlot <- ggplot()+geom_bar(data=total_crop_dmg,aes(x = reorder(EVTYPE, -grandtotalcrop),
                                  y=grandtotalcrop,fill=interaction(EVTYPE,grandtotalcrop)),
                                  show.legend = FALSE,stat="identity")+
                                  xlab("Event")+ylab("Economic Damage")+ggtitle("Top 7 Events causing Damage (Crop)")+
                                  scale_fill_brewer(palette="BrBG")


    propPlot <- ggplot()+geom_bar(data=total_prop_dmg,aes(x = reorder(EVTYPE, -grandtotalprop),
                                  y=grandtotalprop,fill=interaction(EVTYPE,grandtotalprop)),
                                  show.legend = FALSE,stat="identity")+
                                  xlab("Event")+ylab("Economic Damage")+ggtitle("Top 7 Events causing Damage (Property)") +
                                  scale_fill_brewer(palette="BrBG")

    totalPlot <- ggplot()+geom_bar(data=total_eco_dmg,aes(x = reorder(EVTYPE,-Total),y = Total,fill=interaction(EVTYPE,Total)),
                                   show.legend = FALSE,stat="identity")+
                                   xlab("Event")+ylab("Economic Damage")+ggtitle("Top 7 Events causing Damage (Total)")+
                                   scale_fill_brewer(palette="BrBG")

     grid.arrange(cropPlot,propPlot,totalPlot)
```
![](https://github.com/DoaaElbanna/Data-Science-Projects/blob/master/07_ExploreWeatherEvents/graphs/Plot2.png)

### 3.Conclusion

Tornados caused the maximum number of fatalities and injuries. It was
followed by Excessive Heat for fatalities and Thunderstorm wind for
injuries.

Floods caused the maximum property damage where as Drought caused the
maximum crop damage. Second major events that caused the maximum damage
was Hurricanes/Typhoos for property damage and Floods for crop damage.
