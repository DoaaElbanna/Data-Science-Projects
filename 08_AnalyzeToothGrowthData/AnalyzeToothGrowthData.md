Introduction
------------

In this Project we’re going to analyze the ToothGrowth data in the R
datasets package. We’re also going to perform some basic exploratory
data analysis and provide a basic summary of the data.Then we’re going
to use hypothesis tests to compare tooth growth by supp and dose.

————————————————————————————————–
---------------------------------

```r
    # Load the Dataset
    library(ggplot2)
    library(datasets)
    data(ToothGrowth)
```
### Some Basic Exploratory Data Analysis
```r
    str(ToothGrowth)
```
    ## 'data.frame':    60 obs. of  3 variables:
    ##  $ len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
    ##  $ supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ dose: num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
```r
    summary(ToothGrowth)
```
    ##       len        supp         dose      
    ##  Min.   : 4.20   OJ:30   Min.   :0.500  
    ##  1st Qu.:13.07   VC:30   1st Qu.:0.500  
    ##  Median :19.25           Median :1.000  
    ##  Mean   :18.81           Mean   :1.167  
    ##  3rd Qu.:25.27           3rd Qu.:2.000  
    ##  Max.   :33.90           Max.   :2.000

### Plot of tooth growth by supp and dose

```r
    ggplot(data=ToothGrowth, aes(x=as.factor(dose), y=len, fill=supp)) +
        geom_bar(stat="identity") +
        facet_grid(. ~ supp) +
        xlab("Dose(mg/day)") +ylab("Tooth length") + ggtitle("Inferential Data Analysis on Toothgrowth")
```
![](https://github.com/DoaaElbanna/Data-Science-Projects/blob/master/08_AnalyzeToothGrowthData/graphs/Plot.png)

### Use hypothesis tests to compare tooth growth by supp and dose

First, We’re going to compare between Tooth Growth and Supplement and
invstigate if there any correlation between these variables.

```r
    t1 <- t.test(len ~ supp, data = ToothGrowth)
    print(t1)
```
    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  len by supp
    ## t = 1.9153, df = 55.309, p-value = 0.06063
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1710156  7.5710156
    ## sample estimates:
    ## mean in group OJ mean in group VC 
    ##         20.66333         16.96333

    ## [1] "P-Value = 0.0606345078809341"

    ## [1] "Confidence Interval = -0.171015618367164"
    ## [2] "Confidence Interval = 7.57101561836716"

So, from that P-value &gt; 0.05 and the confidence interval of the test
includes zero.Then we can say that the Supplement types doesn’t have an
impact on Tooth Growth based on this Test.

Let’s perform another test, now comparing Tooth Grow with Dose Amount
looking at the different pairs of dose values(0.5, 1.0, 2.0)

```r
    # Does amount 0.5 and 1.0
    t2 <- t.test(len~dose , data = subset(ToothGrowth, ToothGrowth$dose %in% c(1.0, 0.5)))
    print(t2)
```
    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  len by dose
    ## t = -6.4766, df = 37.986, p-value = 1.268e-07
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -11.983781  -6.276219
    ## sample estimates:
    ## mean in group 0.5   mean in group 1 
    ##            10.605            19.735

    ## [1] "P-value =  1.26830072017385e-07"

```r
    # Does amount 0.5 and 2.0
    t3 <- t.test(len~dose , data = subset(ToothGrowth, ToothGrowth$dose %in% c(2.0, 0.5)))
    print(t3)
```
    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  len by dose
    ## t = -11.799, df = 36.883, p-value = 4.398e-14
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -18.15617 -12.83383
    ## sample estimates:
    ## mean in group 0.5   mean in group 2 
    ##            10.605            26.100

    ## [1] "P-value =  4.39752495936323e-14"

```r
    # Does amount 1.0 and 2.0
    t4<- t.test(len~dose , data = subset(ToothGrowth, ToothGrowth$dose %in% c(2.0, 1.0)))
    print(t4)
```
    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  len by dose
    ## t = -4.9005, df = 37.101, p-value = 1.906e-05
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -8.996481 -3.733519
    ## sample estimates:
    ## mean in group 1 mean in group 2 
    ##          19.735          26.100

    ## [1] "P-value =  1.9064295136718e-05"

P-values are all close to 0 (less than 0.05),and the confidence interval
of each test does not cross over zero, From these results We can say
that supplement types seems doen’t have impact on Tooth growth based on
this test and we can assume that the average tooth length increases with
an increasing dose

### Conclusions

Given the following assumptions:

    1- The sample is a random sample and the data follows a normal probability distribution.

    2- The distribution of the sample means follows the Central Limit Theorem.

    3- The average tooth length increases with an inceasing of dose.

We can conclude that:

    1- Supplement Delivery Method has no effect on tooth growth/length

    2- Increased Dose lead to increased tooth length
