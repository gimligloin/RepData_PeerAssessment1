Import data
-----------

This is a project to explore activity monitoring data. We will import the data to a datafram

``` r
df=read.csv('D:/temp/repdata_data_activity/activity.csv')
df$date=as.Date(df$date,"%Y-%M-%d")
head(df)
```

    ##   steps       date interval
    ## 1    NA 2012-01-01        0
    ## 2    NA 2012-01-01        5
    ## 3    NA 2012-01-01       10
    ## 4    NA 2012-01-01       15
    ## 5    NA 2012-01-01       20
    ## 6    NA 2012-01-01       25

Calculate mean
--------------

We will now calculate the mean number of steps taken per daytot

``` r
totalsteps <- tapply(df$steps, df$date, sum)
mean(totalsteps, na.rm = TRUE )
```

    ## [1] 21151.88

``` r
median(totalsteps, na.rm = TRUE )
```

    ## [1] 21782

``` r
hist (totalsteps, breaks = 10)
```

![](PA1_template_files/figure-markdown_github/mean-1.png)

Generate average steps time series
----------------------------------

We will generate a plot showing average steps per 5 min time period.

``` r
averagesteps <- aggregate(df$steps, by=list(df$interval), FUN = mean, na.rm = TRUE)

plot(x=averagesteps$Group.1, y=averagesteps$x, type = 'l', xlab = 'Time', ylab = 'Average Steps', main = 'Average Steps Time Series')
```

![](PA1_template_files/figure-markdown_github/time%20series%20plot-1.png)

``` r
averagesteps[which.max(averagesteps$x),1]
```

    ## [1] 835

Impute missing numbers
----------------------

We will calculate and impute missing numbers

``` r
##number of incompletecases
sum(!complete.cases(df))
```

    ## [1] 2304

``` r
dfcopy <- df
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

``` r
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
dfcopy<-dfcopy %>%
  group_by(interval) %>%
  mutate(
    steps=impute.mean(steps)
  )

totalsteps <- tapply(dfcopy$steps, dfcopy$date, sum)
mean(totalsteps)
```

    ## [1] 21185.08

``` r
median(totalsteps)
```

    ## [1] 21641

``` r
hist (totalsteps, breaks = 10)
```

![](PA1_template_files/figure-markdown_github/missing%20numbers-1.png)

Weekends vs weekdays
====================

We will see if there is difference between activity pattern on weekdays and weekends

``` r
dfcopy$weekend <- 'Weekday'

for (i in 1:nrow(dfcopy)){
  if (weekdays(dfcopy$date[i])=="Saturday" | weekdays(dfcopy$date[i])=="Sunday" ) {
    dfcopy$weekend[i]<-'Weekend'
  }
}
averagesteps <- aggregate(x=list(steps=dfcopy$steps), by=list(interval=dfcopy$interval, weekend=dfcopy$weekend), FUN = mean, na.rm = TRUE)
library(ggplot2)
qplot(x=interval, y=steps, data=averagesteps, geom="line", facets = weekend~.)
```

![](PA1_template_files/figure-markdown_github/weekends-1.png)
