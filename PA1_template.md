---
title:"Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
=====================================================


## Loading and preprocessing the data
##Assuming that the file is already unzipped in the local folder

```r
data <- read.csv("activity.csv",header=T)
```


## What is mean total number of steps taken per day?

```r
data_trim <- na.omit(data) # Considering only complete cases
steppday <- aggregate(steps ~ date, data_trim, sum) #calculating total steps per day
hist(steppday$steps,col=1,main="Histogram - Total No. of Steps per Day",xlab="Total No. of steps each day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-13-1.png) 
## Mean & Median of Total No. of Steps taken per Day

```r
sprintf("Mean Value for Total No. of Steps per Day is '%a'",mean(steppday$steps))
```

```
## [1] "Mean Value for Total No. of Steps per Day is '0x1.5071826a439f6p+13'"
```

```r
sprintf("Median Value for Total No. of Steps per Day is '%a'",median(steppday$steps))
```

```
## [1] "Median Value for Total No. of Steps per Day is '0x1.5068p+13'"
```


## What is the average daily activity pattern?

```r
int_steps <- aggregate(steps ~ interval, data_trim, mean) #Calculating Steps with respect to interval and summing them up
plot(int_steps$interval,int_steps$steps,type='l',col=1,main="Avg. No. of Steps across all Days",xlab="Interval",ylab="Avg. No. of Steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-15-1.png) 
## Interval with Max. No. of Steps across all the Days

```r
max_steps_int <- int_steps[which.max(int_steps$steps),] ##Finding row with max interval
sprintf("Max interval is '%a'-'%a'with max. avg. no. of steps'%a'",as.numeric(max_steps_int$interval),as.numeric(max_steps_int$interval)+5,as.numeric(max_steps_int$steps))
```

```
## [1] "Max interval is '0x1.a18p+9'-'0x1.a4p+9'with max. avg. no. of steps'0x1.9c56f1826a43ap+7'"
```


## Imputing missing values

```r
data_na <- data[!complete.cases(data),] ##Rows with NAs
sprintf("No.of rows with missing values are '%a'",nrow(data_na))
```

```
## [1] "No.of rows with missing values are '0x1.2p+11'"
```
##Impute Mean for NA Values of the 5-min interval

```r
for(i in 1:nrow(data)){
        if(is.na(data$steps[i])){
                int_val <- data$interval[i]
                rid <- which(int_steps$interval==int_val)
                steps_val <- int_steps$steps[rid]
                data$steps[i] <- steps_val
        }
} 
```
##Aggregate Steps with respect to date

```r
imputed_steps <- aggregate(steps ~ date,data,sum)
```
##Creating Histogram with Imputed Values

```r
hist(imputed_steps$steps,col=1,main="Histogram with Imputed Values",xlab="Total No. of Steps each day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-20-1.png) 
##Calcuating Mean & Median for Original Data Set and Data Set with Imputed Values

```r
sprintf("Original Data Set Mean Value - '%a'",mean(steppday$steps))
```

```
## [1] "Original Data Set Mean Value - '0x1.5071826a439f6p+13'"
```

```r
sprintf("Original Data Set Median Value - '%a'",median(steppday$steps))
```

```
## [1] "Original Data Set Median Value - '0x1.5068p+13'"
```

```r
sprintf("Imputed Data Set Mean Value - '%a'",mean(imputed_steps$steps))
```

```
## [1] "Imputed Data Set Mean Value - '0x1.5071826a439f6p+13'"
```

```r
sprintf("Imputed Data Set Median Value - '%a'",median(imputed_steps$steps))
```

```
## [1] "Imputed Data Set Median Value - '0x1.5071826a439f6p+13'"
```
* Though the Means remain the same due to imputation, there is minor difference (by 1) in Median


## Are there differences in activity patterns between weekdays and weekends?

