ST558 - Project 2
================
Group 5: Tyler Pollard & Lucy Yin

-   [Required Packages](#required-packages)
-   [Introduction](#introduction)
-   [Data](#data)
-   [Summarization](#summarization)
    -   [Contingency Tables](#contingency-tables)
        -   [Weather Situation](#weather-situation)
        -   [Year, Season and Count of
            Riders](#year-season-and-count-of-riders)
        -   [Working Day and Count of Casual
            Riders](#working-day-and-count-of-casual-riders)
    -   [Summary Tables](#summary-tables)
        -   [Feeling Temperature](#feeling-temperature)
        -   [Humidity](#humidity)
        -   [Wind Speed](#wind-speed)
    -   [Histograms](#histograms)
        -   [Humidity and Windspeed
            Distributions](#humidity-and-windspeed-distributions)
    -   [Density Plot](#density-plot)
        -   [Casual Riders and Weather
            Situation](#casual-riders-and-weather-situation)
        -   [Registered Riders and Weather
            Situation](#registered-riders-and-weather-situation)
        -   [Total Riders and Weather
            Situation](#total-riders-and-weather-situation)
        -   [Casual Riders and Holiday](#casual-riders-and-holiday)
        -   [Registered Riders and
            Holiday](#registered-riders-and-holiday)
        -   [Total Riders and Holiday](#total-riders-and-holiday)
    -   [Boxplots](#boxplots)
        -   [Feeling Temperature Over the
            Year](#feeling-temperature-over-the-year)
        -   [Riders of Every Hour and Weather
            Situation](#riders-of-every-hour-and-weather-situation)
    -   [Scatter Plots](#scatter-plots)
        -   [Count vs Casual by Season](#count-vs-casual-by-season)
        -   [Riders vs Temperature](#riders-vs-temperature)
        -   [Riders vs. Hour vs. Month vs. Working
            Day](#riders-vs-hour-vs-month-vs-working-day)
    -   [Correlation Plot](#correlation-plot)
        -   [Correlation between temp, atemp, hum,
            windspeed](#correlation-between-temp-atemp-hum-windspeed)
    -   [Plots with GGally](#plots-with-ggally)
        -   [Using Day Data](#using-day-data)
        -   [Using Hour Data](#using-hour-data)
-   [Modeling](#modeling)
    -   [Linear Regression Model](#linear-regression-model)
        -   [What is Linear Regression
            Model](#what-is-linear-regression-model)
        -   [Picking predictors using
            AIC](#picking-predictors-using-aic)
        -   [Modeling using AIC picked
            predictor](#modeling-using-aic-picked-predictor)
    -   [Ensemble Tree Model](#ensemble-tree-model)
        -   [Random Forest Model](#random-forest-model)
        -   [Boosted Tree Model](#boosted-tree-model)
-   [Comparison](#comparison)

# Required Packages

# Introduction

For this report we will be using 6 models (4 linear regression, 1 random
forest model, 1 boosted tree model) to make predictions on the total
count of bike riders using data from the Bike Sharing Dataset (dataset
can be found
[here](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset)).
This dataset contains hourly and daily count of registered, casual, and
total sum of riders in the Capital bikeshare system, contributing
variables include:

-   season (winter, spring, summer, fall)  
-   year (2011, 2012)  
-   month of the year
-   hour of the day  
-   holiday (yes, no)  
-   day of the week  
-   working day (yes or no)  
-   weather situation (mostly clear, mist, light precipitation, heavy
    precipitation)  
-   temperature  
-   feeling temperature  
-   humidity  
-   wind speed

There are 3 different types of response variables in the dataset:

-   registered: registered riders who uses this bikeshare service
    regularly  
-   casual: un-registered riders who use this service casually or on
    occasions  
-   total: combined count of registered and casual riders

For our analysis, we will be working with almost all of the variables as
predictors, and our response variable will be the total count of bike
riders.

We will be selecting predictors using the `step()` function which
chooses a model by AIC in a stepwise algorithm. As a result, which
predictors we incorporate in our linear regression models and ensemble
tree (specifically random forest and boosted tree) models may differ
depending on which day of the week we look at. We’ll randomly split the
data into training and test sets and fit the 6 models on the training
set. Ultimately we will fit the 6 models on the test set and decide on
which model produced the best prediction, which we judge by the smallest
root mean squared error value.

# Data

First we will read in both the `hours.csv` and `day.csv` data.

``` r
# read in data
hour.data <- read_csv("data/hour.csv") %>% as_tibble()
day.data <- read_csv("data/day.csv") %>% as_tibble()
```

We will make corrections on variable types, specifically we’re making
sure categorical variables will be appropriately classified as factors
with clear levels.

``` r
# correct the variable types
hour.data$season <- factor(hour.data$season)
levels(hour.data$season) <- list(winter = 1, spring = 2, summer = 3, fall = 4)

hour.data$yr <- factor(hour.data$yr)
levels(hour.data$yr) <- list("2011" = 0, "2012" = 1)

hour.data$weekday <- factor(hour.data$weekday)
levels(hour.data$weekday) <- list(monday = 1, tuesday = 2, wednesday = 3, thursday = 4, friday = 5, saturday = 6, sunday = 0)

hour.data$mnth <- factor(hour.data$mnth)
hour.data$hr <- factor(hour.data$hr)
hour.data$holiday <- factor(hour.data$holiday)
hour.data$workingday <- factor(hour.data$workingday)
hour.data$weathersit <- factor(hour.data$weathersit)

day.data$season <- factor(day.data$season)
levels(day.data$season) <- list(winter = 1, spring = 2, summer = 3, fall = 4)

day.data$yr <- factor(day.data$yr)
levels(day.data$yr) <- list("2011" = 0, "2012" = 1)

day.data$weekday <- factor(day.data$weekday)
levels(day.data$weekday) <- list(monday = 1, tuesday = 2, wednesday = 3, thursday = 4, friday = 5, saturday = 6, sunday = 0)

day.data$mnth <- factor(day.data$mnth)
day.data$holiday <- factor(day.data$holiday)
day.data$workingday <- factor(day.data$workingday)
day.data$weathersit <- factor(day.data$weathersit)
```

Because the variables temperature, feeling temperature, humidity and
windspeed are normalized according to different measures, we will
un-normalize them and save the raw values as separate columns in the
dataset.

``` r
# Temp Unnormal
temp.tmin = -8
temp.tmax = 39
hour.data$temp.unnormal <- hour.data$temp*(temp.tmax - temp.tmin) + temp.tmin # Unnormalize temp
hour.data$temp.F <- hour.data$temp.unnormal*(9/5) + 32 # Convert to Fahrenheit
day.data$temp.unnormal <- hour.data %>% group_by(dteday) %>% summarise(mean = mean(temp.unnormal)) %>% select(mean)
day.data$temp.unnormal <- day.data$temp.unnormal[[1]]
day.data$temp.F <- hour.data %>% group_by(dteday) %>% summarise(mean = mean(temp.F)) %>% select(mean)
day.data$temp.F <- day.data$temp.F[[1]]

# Atemp Unnormal
atemp.tmin = -16
atemp.tmax = 50
hour.data$atemp.unnormal <- hour.data$atemp*(atemp.tmax - atemp.tmin) + atemp.tmin # Unnormalize atemps
hour.data$atemp.F <- hour.data$atemp.unnormal*(9/5) + 32 # Convert to Fahrenheit
day.data$atemp.unnormal <- hour.data %>% group_by(dteday) %>% summarise(mean = mean(atemp.unnormal)) %>% select(mean)
day.data$atemp.unnormal <- day.data$atemp.unnormal[[1]]
day.data$atemp.F <- hour.data %>% group_by(dteday) %>% summarise(mean = mean(atemp.F)) %>% select(mean)
day.data$atemp.F <- day.data$atemp.F[[1]]

# Humidity Unnormal
day.data$hum.unnormal <- day.data$hum * 100
hour.data$hum.unnormal <- hour.data$hum * 100

# Windspeed Unnormal
day.data$windspeed.unnormal <- day.data$windspeed * 67
hour.data$windspeed.unnormal <- hour.data$windspeed * 67
```

Because hour and day data are stored separately, we create a
`total.data` table with all the information combined just in case we
need to access this in later steps.

``` r
# add in a new variable before merging
hour.data <- mutate(hour.data, type = "hour")
day.data <- mutate(day.data, type = "day", hr = NA) %>% select(instant, dteday, season, yr, mnth, hr, everything())

# merge to create complete list of hour/day data
total.data <- rbind(hour.data, day.data)
```

We will filter to only include data from one specific day of the week at
a time.

``` r
# filter out to one specific day of the week
hour.data <- hour.data %>% filter(weekday == params$weekday)
day.data <- day.data %>% filter(weekday == params$weekday)
total.data <- total.data %>% filter(weekday == params$weekday)
```

We randomly sample from the filtered data to form a training set (with
70% of data) and test set (with the remaining 30% of data). Here we
randomly sampled from the day dataset and split it into training and
test sets, then we split the corresponding data from the hours dataset
according to which days are in the training set and which days are in
the test set. We will be using the hours dataset for our modeling, but
we wanted to make sure our day and hour datasets had matching training
and test splits.

``` r
# splitting data into training & test sets
set.seed(7)
train <- sample(1:nrow(day.data), size = nrow(day.data)*0.7)
test <- dplyr::setdiff(1:nrow(day.data), train)
day.training.data <- day.data[train, ]
day.test.data <- day.data[test, ]

hour.training.data <- hour.data[hour.data$dteday %in% day.training.data$dteday,]
hour.test.data <- hour.data[hour.data$dteday %in% day.test.data$dteday,]
```

# Summarization

We have some basic summary statistics and plots about our training data.

## Contingency Tables

### Weather Situation

Below is a contingency table that shows the count of days that fall into
the different categories of weather situation. This table will help
justify the total count of riders because it can be expected that the
number of casual riders, which influences the total count of riders,
will be higher on nicer days that fall into the first two categories of
Mostly clear and Mist.

``` r
levels(day.training.data$weathersit) <- list(
  "Mostly clear" = "1",
  "Mist" = "2",
  "Light precipitation" = "3",
  "Heavy precipitation" = "4")
kable(t(table(day.training.data$weathersit)))
```

| Mostly clear | Mist | Light precipitation | Heavy precipitation |
|-------------:|-----:|--------------------:|--------------------:|
|           44 |   28 |                   1 |                   0 |

### Year, Season and Count of Riders

These contingency tables show what count range of riders utilized the
bikeshare service for a given season or for a given year. This table can
help us see if the number of riders increased/decreased from 2011 to
2012, or if season has an effect on how many riders used the bikeshare
service.

``` r
kable(table(day.training.data$season, cut(day.training.data$cnt, breaks = 2, dig.lab = 10)), caption = "Occurrences of # Range of Riders of a given Season")
```

|        | (913.395,4222.5\] | (4222.5,7531.605\] |
|:-------|------------------:|-------------------:|
| winter |                14 |                  0 |
| spring |                 7 |                 11 |
| summer |                 3 |                 18 |
| fall   |                 7 |                 13 |

Occurrences of # Range of Riders of a given Season

``` r
kable(table(day.training.data$yr, cut(day.training.data$cnt, breaks = 2, dig.lab = 10)), caption = "Occurrences of # Range of Riders of a given Year")
```

|      | (913.395,4222.5\] | (4222.5,7531.605\] |
|:-----|------------------:|-------------------:|
| 2011 |                23 |                 13 |
| 2012 |                 8 |                 29 |

Occurrences of # Range of Riders of a given Year

### Working Day and Count of Casual Riders

This contingency table show what count range of casual riders utilized
the bikeshare service on working day versus non-working day. Intuitively
we’d suspect that there would more casual riders on non-working day than
working day, this table can show us whether it’s true or not.

``` r
levels(day.training.data$workingday) <- list("workday" = 1, "non-workday" = 0)
kable(table(day.training.data$workingday, cut(day.training.data$casual, breaks = 2, dig.lab = 10)), caption = "Occurrences of # Range of Casual Riders of Workday vs. non-Workday")
```

|             | (38.484,1299\] | (1299,2559.516\] |
|:------------|---------------:|-----------------:|
| workday     |             62 |                0 |
| non-workday |              7 |                4 |

Occurrences of # Range of Casual Riders of Workday vs. non-Workday

## Summary Tables

### Feeling Temperature

The summary tables of feeling temperature show the 5 number summary
along with the mean and standard deviation of what the temperature
actually felt like over the different years. The summary table for both
the normalized and raw feeling temperatures are provided. These tables
give insight to the range of feeling temperatures felt by the riders for
the different years.

``` r
# Normalized feeling temperature
atemp.summary <- hour.training.data %>% group_by(yr) %>% summarise(Min. = min(atemp), `1st Qu.` = quantile(atemp,0.25), Median = median(atemp), Mean = mean(atemp), `3rd Qu.` = quantile(atemp, 0.75), Max. = max(atemp), `St. Dev.` = sd(atemp))
kable(atemp.summary, digits = 2, caption = "Summary of feeling temperatures by year")
```

| yr   | Min. | 1st Qu. | Median | Mean | 3rd Qu. | Max. | St. Dev. |
|:-----|-----:|--------:|-------:|-----:|--------:|-----:|---------:|
| 2011 | 0.03 |    0.33 |    0.5 | 0.48 |    0.62 | 0.85 |     0.19 |
| 2012 | 0.11 |    0.39 |    0.5 | 0.50 |    0.62 | 0.82 |     0.16 |

Summary of feeling temperatures by year

``` r
# Raw feeling temperature in Fahrenheit
atemp.summary.unnormal <- hour.training.data %>% group_by(yr) %>% summarise(Min. = min(atemp.F), `1st Qu.` = quantile(atemp.F,0.25), Median = median(atemp.F), Mean = mean(atemp.F), `3rd Qu.` = quantile(atemp.F, 0.75), Max. = max(atemp.F), `St. Dev.` = sd(atemp.F))
kable(atemp.summary.unnormal, digits = 2, caption = "Summary of raw feeling temperatures by year")
```

| yr   | Min. | 1st Qu. | Median |  Mean | 3rd Qu. |  Max. | St. Dev. |
|:-----|-----:|--------:|-------:|------:|--------:|------:|---------:|
| 2011 |  6.8 |    42.8 |   62.6 | 59.87 |      77 | 104.0 |    22.11 |
| 2012 | 15.8 |    50.0 |   62.6 | 62.38 |      77 | 100.4 |    19.05 |

Summary of raw feeling temperatures by year

### Humidity

These summary tables show the spread of normalized and raw humidity
values. The table includes the 5 number summary along with mean and
standard deviation, which gives insight to the range of humidity levels
riders experienced.

``` r
kable(t(c(summary(day.training.data$hum), St.Dev. = sd(day.training.data$hum))), digits = 2, caption = "Summary of Normalized Humidity")
```

| Min. | 1st Qu. | Median | Mean | 3rd Qu. | Max. | St.Dev. |
|-----:|--------:|-------:|-----:|--------:|-----:|--------:|
| 0.32 |    0.55 |   0.66 | 0.65 |    0.74 | 0.92 |    0.13 |

Summary of Normalized Humidity

``` r
kable(t(c(summary(day.training.data$hum.unnormal), St.Dev. = sd(day.training.data$hum.unnormal))), digits = 2, caption = "Summary of Raw Humidity")  
```

|  Min. | 1st Qu. | Median |  Mean | 3rd Qu. | Max. | St.Dev. |
|------:|--------:|-------:|------:|--------:|-----:|--------:|
| 31.75 |   55.08 |  65.83 | 64.98 |   74.17 | 92.5 |    13.1 |

Summary of Raw Humidity

### Wind Speed

These summary tables show the spread of normalized and raw wind speeds.
The table includes the 5 number summary along with mean and standard
deviation, which gives insight to the range of wind speeds riders
experienced.

``` r
kable(t(c(summary(day.training.data$windspeed), St.Dev. = sd(day.training.data$windspeed))), digits = 2, caption = "Summary of Normalized Wind Speed")
```

| Min. | 1st Qu. | Median | Mean | 3rd Qu. | Max. | St.Dev. |
|-----:|--------:|-------:|-----:|--------:|-----:|--------:|
| 0.04 |    0.13 |   0.17 | 0.19 |    0.23 | 0.39 |    0.08 |

Summary of Normalized Wind Speed

``` r
kable(t(c(summary(day.training.data$windspeed.unnormal), St.Dev. = sd(day.training.data$windspeed.unnormal))), digits = 2, caption = "Summary of Raw Wind Speed ")
```

| Min. | 1st Qu. | Median |  Mean | 3rd Qu. |  Max. | St.Dev. |
|-----:|--------:|-------:|------:|--------:|------:|--------:|
| 2.83 |    8.71 |  11.58 | 12.49 |   15.67 | 25.92 |    5.54 |

Summary of Raw Wind Speed

## Histograms

### Humidity and Windspeed Distributions

The following density plots show the distribution of the weather effects
for raw humidity and raw wind speed over the span of the biker data.
These distributions provide insight on what values for each weather
effect can be expected and how the combination of each effect may drive
the different weather situations and in turn the expected count of
riders.

``` r
hum.histogram <- ggplot(data = day.training.data, aes(x = hum.unnormal)) + 
  geom_histogram(aes(y = ..density..), bins = 30) + 
  geom_density(color = "red", size = 2) + 
  labs(title = "Humidity Distribution", x = "Raw Humidity", y = "Density")
windspeed.histogram <- ggplot(data = day.training.data, aes(x = windspeed.unnormal)) + 
  geom_histogram(aes(y = ..density..), bins = 30) + 
  geom_density(color = "red", size = 2) + 
  labs(title = "Windspeed Distribution", x = "Raw Windspeed", y = "Density")
grid.arrange(hum.histogram, windspeed.histogram, ncol = 2, top = "Density Distribution of Weather Effects")
```

![](mondayAnalysis_files/figure-gfm/histogram%20-%20count-1.png)<!-- -->

## Density Plot

### Casual Riders and Weather Situation

This density plot shows the amount of casual riders in a given weather
situation. Intuitively we suspect that there would more casual riders in
better weather conditions. This density plot can show us whether or not
this is true.

``` r
ggplot(hour.training.data, aes(x = casual)) + 
  geom_density(alpha = 0.5, position = "stack", aes(fill = weathersit)) + 
  labs(title = "Density plot of casual riders",
       subtitle = "Specified by weather situation",
       x = "Count of Casual Riders",
       y = "Density") + 
  scale_fill_discrete(name = "Weather Situation", labels = c("Mostly Clear", "Mist", "Light Precip.", "Heavy Precip."))  
```

![](mondayAnalysis_files/figure-gfm/density%20plot%20-%20weather,casual-1.png)<!-- -->

### Registered Riders and Weather Situation

This density plot shows the amount of registered riders in a given
weather situation. We suspect that the amount of registered riders
wouldn’t be as affected by weather situation as the amount of casual
riders would. This density plot can show us whether or not this is true.

``` r
ggplot(hour.training.data, aes(x = registered)) + 
  geom_density(alpha = 0.5, position = "stack", aes(fill = weathersit)) + 
  labs(title = "Density plot of casual riders",
       subtitle = "Specified by weather situation",
       x = "Count of Registered Riders",
       y = "Density") + 
  scale_fill_discrete(name = "Weather Situation", labels = c("Mostly Clear", "Mist", "Light Precip.", "Heavy Precip."))  
```

![](mondayAnalysis_files/figure-gfm/density%20plot%20-%20weather,registered-1.png)<!-- -->

### Total Riders and Weather Situation

This density plot shows the total count of riders in a given weather
situation. We may see a relationship between how many riders there are
and what type of weather condition it is.

``` r
ggplot(hour.training.data, aes(x = cnt)) + 
  geom_density(alpha = 0.5, position = "stack", aes(fill = weathersit)) + 
  labs(title = "Density plot of casual riders",
       subtitle = "Specified by weather situation",
       x = "Total Count of Riders",
       y = "Density") + 
  scale_fill_discrete(name = "Weather Situation", labels = c("Mostly Clear", "Mist", "Light Precip.", "Heavy Precip."))  
```

![](mondayAnalysis_files/figure-gfm/density%20plot%20-%20weather,cnt-1.png)<!-- -->

### Casual Riders and Holiday

This density plot shows the amount of casual riders depending on whether
it is a holiday or non-holiday. We suspect there would be more casual
riders on holidays, especially at larger counts. This density plot can
show us whether that is true.

``` r
ggplot(hour.training.data, aes(x = casual)) + 
  geom_density(alpha = 0.5, position = "stack", aes(fill = holiday)) +
  labs(title = "Density plot of casual riders",
       subtitle = "Specified by holiday or not",
       x = "Count of Casual Riders",
       y = "Density") +
  scale_fill_discrete(name = "Holiday?", labels = c("Non-Holiday", "Holiday"))    
```

![](mondayAnalysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Registered Riders and Holiday

This density plot shows the amount of registered riders depending on
whether it is a holiday or non-holiday. We suspect there would be more
registered riders on non-holidays, especially at larger counts. This
density plot can show us whether that is true.

``` r
ggplot(hour.training.data, aes(x = registered)) + 
  geom_density(alpha = 0.5, position = "stack", aes(fill = holiday)) +
  labs(title = "Density plot of casual riders",
       subtitle = "Specified by holiday or not",
       x = "Count of Registered Riders",
       y = "Density") +
  scale_fill_discrete(name = "Holiday?", labels = c("Non-Holiday", "Holiday"))    
```

![](mondayAnalysis_files/figure-gfm/density%20plot%20-%20holiday,registered-1.png)<!-- -->

### Total Riders and Holiday

This density plot shows the total count of riders depending on whether
it is a holiday or non-holiday. This plot could show a relationship
between the amount of riders versus whether it’s a holiday or
non-holiday.

``` r
ggplot(hour.training.data, aes(x = cnt)) + 
  geom_density(alpha = 0.5, position = "stack", aes(fill = holiday)) +
  labs(title = "Density plot of casual riders",
       subtitle = "Specified by holiday or not",
       x = "Total Count of Riders",
       y = "Density") +
  scale_fill_discrete(name = "Holiday?", labels = c("Non-Holiday", "Holiday"))    
```

![](mondayAnalysis_files/figure-gfm/density%20plot%20-%20holiday,cnt-1.png)<!-- -->

## Boxplots

### Feeling Temperature Over the Year

To get a better understanding of the feeling temperature spreads over
the year, boxplots of the feeling temperature are plotted by month with
the data points for each day used to create them plotted overtop.
Intuitively, it can be expected that the feeling temperature rises from
the beginning of the year into the middle of summer and then drops back
down over the fall and winter months. These boxplots provide insight
into the possible number of rider fluctuation over the different months
of the year.

``` r
atemp.boxplot.df <- day.training.data
levels(atemp.boxplot.df$mnth) <- list(January = 1, February = 2, March = 3, April = 4, May = 5, June = 6, July = 7, August = 8, September = 9, October = 10, November = 11, December = 12)
ggplot(data = atemp.boxplot.df, aes(x = mnth, y = atemp.F)) + 
  geom_boxplot() + 
  geom_point(position = "jitter", color = "blue") + 
  labs(title = "Feeling temperature distribution per month", x = "Month", y = "Feeling Temperature (F)")
```

![](mondayAnalysis_files/figure-gfm/boxplot-%20adjusted%20temperature-1.png)<!-- -->

### Riders of Every Hour and Weather Situation

This boxplot shows the 5 number summary (in boxplot form with occasional
outliers) of the amount of riders for each hour of the day. The colored
lines should the mean number of riders for each given weather situation.
We expect that the highest amount of riders should appear around the
morning and afternoon commute time given it’s not on a holiday or on the
weekends. This boxplot can show if that’s true.

``` r
ggplot(hour.training.data, aes(x = hr, y = cnt)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "line", lwd = 0.8, aes(group = weathersit, col = weathersit)) + 
  labs(title = "Count of riders for every hr",
       subtitle = "Mean values based on weather situation",
       x = "Hour of the Day",
       y = "Count of Riders") + 
  scale_color_discrete(name = "Weather Situation", labels = c("Mostly Clear", "Mist", "Light Precip.", "Heavy Precip."))
```

![](mondayAnalysis_files/figure-gfm/boxplot%20-%20weathersit,count,hour-1.png)<!-- -->

## Scatter Plots

### Count vs Casual by Season

These four scatter plots show the relation between the total number of
riders and casual riders by day with linear models plotted overtop
parsed by season. These plots show how the number of casual riders
contribute to the total count of riders for each season. The greater the
slope of the linear model correlates to a greater number of causal
riders contributing to the total count of riders.

``` r
ggplot(data = day.training.data, aes(x = cnt, y = casual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(cols = vars(season)) + 
  labs(title = "Casual Riders Influence on Total Count", x = "Count", y = "Casual Riders")
```

![](mondayAnalysis_files/figure-gfm/scatter%20-%20registered%20vs%20count-1.png)<!-- -->

### Riders vs Temperature

Below is a scattered plot of the number of causal riders vs the raw
temperature for each day in the span of the data parsed by workingday
with a local polynomial regression line fit overtop. This plot provides
insight on how many people spontaneous chose to ride based on the raw
temperature of that day.

``` r
day.training.data$temp.indicator <- ifelse(day.training.data$temp < mean(day.training.data$temp), 0, 1)
day.training.data$temp.indicator <- as_factor(day.training.data$temp.indicator)
levels(day.training.data$temp.indicator) <- list("Low Temperature" = 0, "High Temperature" = 1)
ggplot(data = day.training.data, aes(x = temp.F, y = casual, color = workingday)) + 
  geom_point() + 
  geom_smooth() + 
  labs(title = "Casual Riders Based on Temperature", x = "Raw Temperature", y = "Number of Casual Riders")
```

![](mondayAnalysis_files/figure-gfm/scatter%20-%20riders-1.png)<!-- -->

### Riders vs. Hour vs. Month vs. Working Day

This boxplot below shows the count of riders for every month and every
hour of the day. The color of the points indicate whether it was on a
working day (1) or non-working day (0). We suspect for working days,
there would be an obvious uptick around the morning and afternoon
commute time. But for non-working days, the amount of riders shouldn’t
have an obvious pattern around those time frames. This plot shows us
whether that’s true or not.

``` r
count.df <- hour.training.data
levels(count.df$mnth) <- list(January = 1, February = 2, March = 3, April = 4, May = 5, June = 6, July = 7, August = 8, September = 9, October = 10, November = 11, December = 12)
ggplot(count.df, aes(x = hr, y = cnt)) +
  geom_point(aes(col = workingday)) +
  facet_wrap(vars(mnth)) + 
  labs(title = "Count of riders for every hour of every month",
       subtitle = "Specified by workday or non-workday",
       x = "Hour of the Day",
       y = "Count of Riders") +
  scale_color_discrete(name = "Working Day")
```

![](mondayAnalysis_files/figure-gfm/scatter%20-%20count,hour,month,workingday-1.png)<!-- -->

## Correlation Plot

### Correlation between temp, atemp, hum, windspeed

This correlation plots show the correlation (positive or negative)
between the 4 quantitative variables temperature, feeling temperature,
humidity and wind speed. We suspect that there likely would be a high
correlation between temperature and feeling temperature, and humidity
might be inverse correlated with wind speed. This correlation can show
us whether this is true.

``` r
cor.variables <- hour.training.data %>% select(temp, atemp, hum, windspeed)
correlation <- cor(cor.variables, method = "spearman")
corrplot(correlation)
```

![](mondayAnalysis_files/figure-gfm/correlation%20plot%20-%20temp,atemp,humidity,windspeed-1.png)<!-- -->

## Plots with GGally

The two GGally plots below will show whether there’s any relationship
between each of the variables. We run this plot on both the day and hour
data.

### Using Day Data

``` r
subset.data.day <- data_frame(weathersit=day.training.data$weathersit, temp=day.training.data$temp, atemp=day.training.data$atemp,humidity=day.training.data$hum, windspeed=day.training.data$windspeed, casual=day.training.data$casual, registered=day.training.data$registered, total=day.training.data$cnt)
GGally::ggpairs(subset.data.day)
```

![](mondayAnalysis_files/figure-gfm/GGally%20-%20day%20data-1.png)<!-- -->

### Using Hour Data

``` r
subset.data.hr <- data_frame(weathersit=hour.training.data$weathersit, temp=hour.training.data$temp, atemp=hour.training.data$atemp,humidity=hour.training.data$hum, windspeed=hour.training.data$windspeed, casual=hour.training.data$casual, registered=hour.training.data$registered, total=hour.training.data$cnt)
GGally::ggpairs(subset.data.hr)
```

![](mondayAnalysis_files/figure-gfm/GGally%20-%20hour%20data-1.png)<!-- -->

# Modeling

## Linear Regression Model

### What is Linear Regression Model

Linear regression is a type of modeling used to predict a response based
on explanatory variables by fitting a linear equation to observed data.
For simple linear regression using a single explanatory variable to
predict a response variable the equation is
*Y*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>*i*</sub> + *E*<sub>*i*</sub>
where *Y*<sub>*i*</sub> is the response for the *i*<sup>*t**h*</sup>
observation, *x*<sub>*i*</sub> is the value of the explanatory variable
for the *i*<sup>*t**h*</sup> observation, *β*<sub>0</sub> is the
y-intercept, *β*<sub>1</sub> is the slope, and *E*<sub>*i*</sub> is the
error for the *i*<sup>*t**h*</sup> observation. Fitting a linear model
to the observed dataset requires estimating the coefficients *β* such
that the error term
*E*<sub>*i*</sub> = *Y*<sub>*i*</sub> − *β*<sub>0</sub> − *β*<sub>1</sub>*x*<sub>*i*</sub>
is minimized. The most common way to minimize this term is through
least-squares where we minimize the sum of squared residuals through
$min\_{\\beta\_{0},\\beta\_{1}}\\sum\_{i=1}^n ({y}\_{i} - \\beta\_{0} - \\beta\_{1}{x}\_{i})$.
Simple linear regression can be extended in many ways to include:

-   higher order terms:
    *Y*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>*i*</sub> + *β*<sub>2</sub>*x*<sub>*i*</sub><sup>2</sup> + *E*<sub>*i*</sub>  
-   more explanatory variables:
    *Y*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>1*i*</sub> + *β*<sub>2</sub>*x*<sub>2*i*</sub> + *β*<sub>3</sub>*x*<sub>1*i*</sub>*x*<sub>2*i*</sub> + *E*<sub>*i*</sub>  
-   more explanatory variables and higher order terms:
    *Y*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>1*i*</sub> + *β*<sub>2</sub>*x*<sub>2*i*</sub> + *β*<sub>3</sub>*x*<sub>1*i*</sub>*x*<sub>2*i*</sub> + *β*<sub>4</sub>*x*<sub>1*i*</sub><sup>2</sup> + *β*<sub>5</sub>*x*<sub>2*i*</sub><sup>2</sup> + *E*<sub>*i*</sub>

In each of these linear regressions the model is still fit by minimizing
the sum of squared errors. As the number of explanatory variables
increase these regression models can become quite large, so it is best
to compare different candidate models to see which provides the best fit
of the data. Usually you would have some sort of subject matter
knowledge to help select these candidate models by understanding which
variables are related and which variables scientifically should be put
in the model. Without subject matter knowledge you might select multiple
candidate models and compare them using fit criteria such as AIC, BIC,
AICc, Adjusted R-squared or Variance Inflation Factor (VIF).
Alternatively, you may compare prediction error by splitting the data
into a training and test set with a 80/20 split and fit the candidate
models on the training set to predict the response of the test set. The
model with the lowest RMSE should be considered to be the best fit as it
minimized the error the best.

### Picking predictors using AIC

First we want to select only the variables that we will use in our
models, as variables such as record index, date are not useful to us. We
will be using the un-normalized versions of temperature, feeling
temperature, humidity and wind speed (instead of the normalized
versions) because we want to standardize all numerical variables when
running our models.  
Because on some days of the week holiday and working day both become 1
leveled factor variables and can cause issues in our modeling, so we
will omit these 2 variables for those days of the week.

``` r
# keep only variables that are relevant to modeling
if.weekday <- hour.training.data %>% filter(weekday == params$weekday) %>% select(workingday) %>% unique() %>% nrow()
if.holiday <- hour.training.data %>% filter(weekday == params$weekday) %>% select(holiday) %>% unique() %>% nrow()

# use function to decide if a weekday has 1 factored levels
# if so we will not use these factors in the model 
get.data <- function(weekday, ...){
  if (if.weekday == 1 & if.holiday == 1) {
    hour.training.data2 <- hour.training.data %>% select(season, yr, mnth, hr, weathersit, temp.F, atemp.F, hum.unnormal, windspeed.unnormal, cnt)
  }
  else {
    hour.training.data2 <- hour.training.data %>% select(season, yr, mnth, hr, holiday, workingday, weathersit, temp.F, atemp.F, hum.unnormal, windspeed.unnormal, cnt)
  }
  hour.training.data2
}
hour.training.data2 <- get.data(params$weekday)
```

We will let the `step()` function to pick our models using the stepwise
algorithm. We provide the `step()` function with 3 different linear
models, first with just first order variables, second with squared terms
and interactions, and third with first ordered variables and
interactions.

``` r
# aic using only 1st ordered terms
fit.aic <- step((lm(cnt ~ ., data = hour.training.data2, verbose = FALSE)), direction = "both")

# aic including squared terms and interactions
fit.aic2 <- step((lm(cnt ~ .^2 + I(temp.F^2) + I(atemp.F^2) + I(hum.unnormal^2) + I(windspeed.unnormal^2), data = hour.training.data2, verbose = FALSE)), direction = "both")

# aic using 1st order and interactions
fit.aic3 <- step((lm(cnt ~.^2, data = hour.training.data2, verbose = FALSE)), direction = "both")
```

### Modeling using AIC picked predictor

For the first linear regression model we run, we will pick predictors
based on our intuition. We expect the feeling temperature would be
highly correlated with the actual temperature, and because wind speed
and humidity could also be correlated with feeling temperature, so we
only kept temperature as a predictor in the model and will not include
feeling temperature. We also did not include holiday in the model
because it has some redundant information to the working day variable.

``` r
# use all predictors except atemp and holiday
set.seed(7)
fit.mlr0 <- train(cnt ~ season + yr + mnth + hr + workingday + weathersit + temp.F + hum.unnormal + windspeed.unnormal,
                  data = hour.training.data,
                  method = "lm",
                  preProcess = c("center", "scale"),
                  trControl = trainControl(method = "cv", number = 10))
fit.mlr0
```

    ## Linear Regression 
    ## 
    ## 1743 samples
    ##    9 predictor
    ## 
    ## Pre-processing: centered (45), scaled (45) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1568, 1569, 1569, 1569, 1569, 1569, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared   MAE     
    ##   85.05434  0.7829297  60.20054
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

``` r
# Examine performance of this multiple linear regression model on the test data after prediction
predict.mlr0 <- postResample(predict(fit.mlr0, newdata = hour.test.data), obs = hour.test.data$cnt)
```

The next three linear regression models are fit using the predictors
picked by the three step functions. These models include different
number of predictors in different complexity, so we will see which
models will produce the best prediction in the end.

``` r
# use aic predictors (1st ordered terms)
set.seed(7)
fit.mlr1 <- train(fit.aic$terms,
                  data = hour.training.data2,
                  method = "lm",
                  preProcess = c("center", "scale"),
                  trControl = trainControl(method = "cv", number = 10))
fit.mlr1
```

    ## Linear Regression 
    ## 
    ## 1743 samples
    ##    8 predictor
    ## 
    ## Pre-processing: centered (44), scaled (44) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1568, 1569, 1569, 1569, 1569, 1569, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared   MAE     
    ##   84.88118  0.7839797  60.17589
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

``` r
# variables used in fit
fit.aic
```

    ## 
    ## Call:
    ## lm(formula = cnt ~ season + yr + mnth + hr + holiday + weathersit + 
    ##     atemp.F + hum.unnormal, data = hour.training.data2, verbose = FALSE)
    ## 
    ## Coefficients:
    ##  (Intercept)  seasonspring  seasonsummer    seasonfall        yr2012         mnth2         mnth3         mnth4  
    ##     -72.1111       50.3331       77.6666      126.7000       78.1798        8.0893       -2.4180        2.2051  
    ##        mnth5         mnth6         mnth7         mnth8         mnth9        mnth10        mnth11        mnth12  
    ##       6.0765       12.6355      -11.8173       -9.3586       14.2532      -22.3457      -37.6974      -45.1886  
    ##          hr1           hr2           hr3           hr4           hr5           hr6           hr7           hr8  
    ##     -18.2315      -27.9507      -29.1547      -29.8863       -9.0825       59.6515      225.1329      386.3041  
    ##          hr9          hr10          hr11          hr12          hr13          hr14          hr15          hr16  
    ##     180.7728       94.2709      113.3211      153.1271      147.2792      132.6885      135.4910      225.1849  
    ##         hr17          hr18          hr19          hr20          hr21          hr22          hr23      holiday1  
    ##     448.8323      432.4544      292.7274      196.8069      130.9523       74.3374       27.9109      -11.6867  
    ##  weathersit2   weathersit3   weathersit4       atemp.F  hum.unnormal  
    ##     -16.2120      -57.3392     -244.9487        1.3667       -0.8239

``` r
# Examine performance of this multiple linear regression model on the test data after prediction
predict.mlr1 <- postResample(predict(fit.mlr1, newdata = hour.test.data), obs = hour.test.data$cnt)
```

``` r
# use aic predictors (2nd ordered terms and interactions)
set.seed(7)
fit.mlr2 <- train(fit.aic2$terms,
                  data = hour.training.data2,
                  method = "lm",
                  preProcess = c("center", "scale"),
                  trControl = trainControl(method = "cv", number = 10))
fit.mlr2
```

    ## Linear Regression 
    ## 
    ## 1743 samples
    ##   10 predictor
    ## 
    ## Pre-processing: centered (398), scaled (398) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1568, 1569, 1569, 1569, 1569, 1569, ... 
    ## Resampling results:
    ## 
    ##   RMSE     Rsquared   MAE     
    ##   48.2138  0.9297704  32.64087
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

``` r
# variables used in fit
fit.aic2
```

    ## 
    ## Call:
    ## lm(formula = cnt ~ season + yr + mnth + hr + holiday + weathersit + 
    ##     temp.F + atemp.F + hum.unnormal + windspeed.unnormal + I(temp.F^2) + 
    ##     I(atemp.F^2) + I(hum.unnormal^2) + season:yr + season:hr + 
    ##     season:atemp.F + season:windspeed.unnormal + yr:mnth + yr:hr + 
    ##     yr:holiday + yr:weathersit + yr:atemp.F + mnth:holiday + 
    ##     mnth:weathersit + mnth:temp.F + mnth:atemp.F + mnth:windspeed.unnormal + 
    ##     hr:holiday + hr:weathersit + hr:temp.F + hr:hum.unnormal + 
    ##     holiday:weathersit + holiday:hum.unnormal + holiday:windspeed.unnormal + 
    ##     weathersit:atemp.F + weathersit:hum.unnormal + weathersit:windspeed.unnormal + 
    ##     temp.F:atemp.F + atemp.F:hum.unnormal + hum.unnormal:windspeed.unnormal, 
    ##     data = hour.training.data2, verbose = FALSE)
    ## 
    ## Coefficients:
    ##                     (Intercept)                     seasonspring                     seasonsummer  
    ##                      -1.818e+02                        6.989e+01                        3.911e+02  
    ##                      seasonfall                           yr2012                            mnth2  
    ##                       2.834e+02                       -5.508e+01                        1.165e+02  
    ##                           mnth3                            mnth4                            mnth5  
    ##                      -6.458e+01                       -1.191e+01                       -1.153e+02  
    ##                           mnth6                            mnth7                            mnth8  
    ##                      -6.258e+01                        8.305e+01                       -2.137e+02  
    ##                           mnth9                           mnth10                           mnth11  
    ##                      -6.975e+02                       -3.911e+02                       -2.460e+02  
    ##                          mnth12                              hr1                              hr2  
    ##                      -3.561e+02                        2.750e+01                        1.253e+01  
    ##                             hr3                              hr4                              hr5  
    ##                      -7.869e-01                        2.603e+01                        2.577e+01  
    ##                             hr6                              hr7                              hr8  
    ##                       4.280e+01                        1.217e+02                        2.168e+02  
    ##                             hr9                             hr10                             hr11  
    ##                       1.178e+02                        1.221e+01                       -9.293e+00  
    ##                            hr12                             hr13                             hr14  
    ##                       3.721e+01                        6.218e+01                        4.221e+01  
    ##                            hr15                             hr16                             hr17  
    ##                       4.720e+01                        5.550e+01                        2.065e+02  
    ##                            hr18                             hr19                             hr20  
    ##                       2.335e+02                        7.551e+01                       -1.518e+01  
    ##                            hr21                             hr22                             hr23  
    ##                      -1.617e+01                       -5.488e+01                       -2.131e+01  
    ##                        holiday1                      weathersit2                      weathersit3  
    ##                      -7.006e+01                        2.967e+01                        8.402e+01  
    ##                     weathersit4                           temp.F                          atemp.F  
    ##                      -9.431e+01                        8.499e+00                       -5.793e+00  
    ##                    hum.unnormal               windspeed.unnormal                      I(temp.F^2)  
    ##                       3.492e+00                        1.089e+00                       -3.846e-01  
    ##                    I(atemp.F^2)                I(hum.unnormal^2)              seasonspring:yr2012  
    ##                      -3.197e-01                       -1.352e-02                        2.128e+01  
    ##             seasonsummer:yr2012                seasonfall:yr2012                 seasonspring:hr1  
    ##                      -2.165e+02                       -8.816e+00                        1.468e+01  
    ##                seasonsummer:hr1                   seasonfall:hr1                 seasonspring:hr2  
    ##                       1.615e+01                        1.465e+01                        2.287e+00  
    ##                seasonsummer:hr2                   seasonfall:hr2                 seasonspring:hr3  
    ##                       7.652e+00                        3.156e+00                       -5.600e+00  
    ##                seasonsummer:hr3                   seasonfall:hr3                 seasonspring:hr4  
    ##                      -1.062e+00                        1.192e+00                       -1.176e+01  
    ##                seasonsummer:hr4                   seasonfall:hr4                 seasonspring:hr5  
    ##                      -7.968e+00                        8.882e-01                       -4.380e-01  
    ##                seasonsummer:hr5                   seasonfall:hr5                 seasonspring:hr6  
    ##                       1.417e+01                        1.784e+01                        1.830e+01  
    ##                seasonsummer:hr6                   seasonfall:hr6                 seasonspring:hr7  
    ##                       4.408e+01                        6.078e+01                        9.621e+01  
    ##                seasonsummer:hr7                   seasonfall:hr7                 seasonspring:hr8  
    ##                       1.512e+02                        1.902e+02                        1.256e+02  
    ##                seasonsummer:hr8                   seasonfall:hr8                 seasonspring:hr9  
    ##                       1.492e+02                        2.613e+02                        8.117e+01  
    ##                seasonsummer:hr9                   seasonfall:hr9                seasonspring:hr10  
    ##                       1.234e+02                        1.345e+02                        3.360e+01  
    ##               seasonsummer:hr10                  seasonfall:hr10                seasonspring:hr11  
    ##                       5.498e+01                        6.021e+01                        4.090e+01  
    ##               seasonsummer:hr11                  seasonfall:hr11                seasonspring:hr12  
    ##                       6.088e+01                        8.517e+01                        8.583e+01  
    ##               seasonsummer:hr12                  seasonfall:hr12                seasonspring:hr13  
    ##                       1.028e+02                        1.181e+02                        1.148e+02  
    ##               seasonsummer:hr13                  seasonfall:hr13                seasonspring:hr14  
    ##                       1.268e+02                        1.376e+02                        8.628e+01  
    ##               seasonsummer:hr14                  seasonfall:hr14                seasonspring:hr15  
    ##                       1.215e+02                        1.179e+02                        7.432e+01  
    ##               seasonsummer:hr15                  seasonfall:hr15                seasonspring:hr16  
    ##                       1.018e+02                        1.198e+02                        8.591e+01  
    ##               seasonsummer:hr16                  seasonfall:hr16                seasonspring:hr17  
    ##                       1.350e+02                        1.757e+02                        9.824e+01  
    ##               seasonsummer:hr17                  seasonfall:hr17                seasonspring:hr18  
    ##                       2.051e+02                        2.611e+02                        1.641e+02  
    ##               seasonsummer:hr18                  seasonfall:hr18                seasonspring:hr19  
    ##                       2.968e+02                        2.618e+02                        8.622e+01  
    ##               seasonsummer:hr19                  seasonfall:hr19                seasonspring:hr20  
    ##                       1.436e+02                        1.381e+02                       -1.757e+00  
    ##               seasonsummer:hr20                  seasonfall:hr20                seasonspring:hr21  
    ##                       2.668e+01                        5.723e+01                        1.808e+01  
    ##               seasonsummer:hr21                  seasonfall:hr21                seasonspring:hr22  
    ##                       2.905e+01                        4.387e+01                       -1.884e+01  
    ##               seasonsummer:hr22                  seasonfall:hr22                seasonspring:hr23  
    ##                      -2.138e+01                        1.094e+01                       -1.151e+01  
    ##               seasonsummer:hr23                  seasonfall:hr23             seasonspring:atemp.F  
    ##                      -7.760e+00                        1.375e+01                       -2.001e+00  
    ##            seasonsummer:atemp.F               seasonfall:atemp.F  seasonspring:windspeed.unnormal  
    ##                      -5.852e+00                       -6.664e+00                       -2.401e+00  
    ## seasonsummer:windspeed.unnormal    seasonfall:windspeed.unnormal                     yr2012:mnth2  
    ##                      -4.615e+00                       -6.914e+00                        2.610e+00  
    ##                    yr2012:mnth3                     yr2012:mnth4                     yr2012:mnth5  
    ##                       2.251e+01                       -3.442e+01                       -1.242e+02  
    ##                    yr2012:mnth6                     yr2012:mnth7                     yr2012:mnth8  
    ##                       1.489e+02                        1.833e+02                        1.750e+02  
    ##                    yr2012:mnth9                    yr2012:mnth10                    yr2012:mnth11  
    ##                       1.800e+02                       -2.213e+01                       -2.307e+01  
    ##                   yr2012:mnth12                       yr2012:hr1                       yr2012:hr2  
    ##                              NA                       -6.670e+00                        2.763e+00  
    ##                      yr2012:hr3                       yr2012:hr4                       yr2012:hr5  
    ##                      -9.840e-02                       -3.927e+00                        1.325e+01  
    ##                      yr2012:hr6                       yr2012:hr7                       yr2012:hr8  
    ##                       4.171e+01                        1.238e+02                        2.201e+02  
    ##                      yr2012:hr9                      yr2012:hr10                      yr2012:hr11  
    ##                       1.010e+02                        5.028e+01                        5.715e+01  
    ##                     yr2012:hr12                      yr2012:hr13                      yr2012:hr14  
    ##                       7.968e+01                        6.820e+01                        6.082e+01  
    ##                     yr2012:hr15                      yr2012:hr16                      yr2012:hr17  
    ##                       7.688e+01                        1.133e+02                        2.071e+02  
    ##                     yr2012:hr18                      yr2012:hr19                      yr2012:hr20  
    ##                       1.959e+02                        1.275e+02                        9.737e+01  
    ##                     yr2012:hr21                      yr2012:hr22                      yr2012:hr23  
    ##                       5.981e+01                        4.202e+01                        3.270e+01  
    ##                 yr2012:holiday1               yr2012:weathersit2               yr2012:weathersit3  
    ##                       1.448e+01                       -1.472e+01                       -4.936e+01  
    ##              yr2012:weathersit4                   yr2012:atemp.F                   mnth2:holiday1  
    ##                              NA                        1.595e+00                       -9.202e+00  
    ##                  mnth3:holiday1                   mnth4:holiday1                   mnth5:holiday1  
    ##                              NA                        5.385e+01                        4.106e+01  
    ##                  mnth6:holiday1                   mnth7:holiday1                   mnth8:holiday1  
    ##                              NA                               NA                               NA  
    ##                  mnth9:holiday1                  mnth10:holiday1                  mnth11:holiday1  
    ##                      -5.949e+01                        5.600e+01                        5.757e+01  
    ##                 mnth12:holiday1                mnth2:weathersit2                mnth3:weathersit2  
    ##                              NA                        1.653e+01                       -2.153e+01  
    ##               mnth4:weathersit2                mnth5:weathersit2                mnth6:weathersit2  
    ##                      -2.451e+01                       -5.768e+01                       -8.725e+01  
    ##               mnth7:weathersit2                mnth8:weathersit2                mnth9:weathersit2  
    ##                      -6.218e+01                       -5.921e+01                       -4.233e+01  
    ##              mnth10:weathersit2               mnth11:weathersit2               mnth12:weathersit2  
    ##                      -4.615e+01                       -3.287e+01                       -2.941e+01  
    ##               mnth2:weathersit3                mnth3:weathersit3                mnth4:weathersit3  
    ##                      -1.012e+02                       -1.014e+02                       -1.282e+02  
    ##               mnth5:weathersit3                mnth6:weathersit3                mnth7:weathersit3  
    ##                      -1.475e+02                       -2.361e+02                       -1.892e+02  
    ##               mnth8:weathersit3                mnth9:weathersit3               mnth10:weathersit3  
    ##                      -1.511e+02                       -1.757e+02                       -1.082e+02  
    ##              mnth11:weathersit3               mnth12:weathersit3                mnth2:weathersit4  
    ##                      -1.443e+02                       -4.006e+01                               NA  
    ##               mnth3:weathersit4                mnth4:weathersit4                mnth5:weathersit4  
    ##                              NA                               NA                               NA  
    ##               mnth6:weathersit4                mnth7:weathersit4                mnth8:weathersit4  
    ##                              NA                               NA                               NA  
    ##               mnth9:weathersit4               mnth10:weathersit4               mnth11:weathersit4  
    ##                              NA                               NA                               NA  
    ##              mnth12:weathersit4                     mnth2:temp.F                     mnth3:temp.F  
    ##                              NA                       -8.780e+00                       -3.101e+00  
    ##                    mnth4:temp.F                     mnth5:temp.F                     mnth6:temp.F  
    ##                      -3.272e+00                       -5.716e+00                       -6.304e+00  
    ##                    mnth7:temp.F                     mnth8:temp.F                     mnth9:temp.F  
    ##                      -1.794e+01                       -9.800e+00                        3.268e+00  
    ##                   mnth10:temp.F                    mnth11:temp.F                    mnth12:temp.F  
    ##                      -3.723e+00                       -5.014e+00                        3.131e-01  
    ##                   mnth2:atemp.F                    mnth3:atemp.F                    mnth4:atemp.F  
    ##                       5.736e+00                        4.944e+00                        4.955e+00  
    ##                   mnth5:atemp.F                    mnth6:atemp.F                    mnth7:atemp.F  
    ##                       9.461e+00                        9.176e+00                        1.810e+01  
    ##                   mnth8:atemp.F                    mnth9:atemp.F                   mnth10:atemp.F  
    ##                       1.398e+01                        8.104e+00                        1.302e+01  
    ##                  mnth11:atemp.F                   mnth12:atemp.F         mnth2:windspeed.unnormal  
    ##                       1.121e+01                        8.038e+00                        3.046e+00  
    ##        mnth3:windspeed.unnormal         mnth4:windspeed.unnormal         mnth5:windspeed.unnormal  
    ##                       1.468e+00                        1.981e+00                        3.239e+00  
    ##        mnth6:windspeed.unnormal         mnth7:windspeed.unnormal         mnth8:windspeed.unnormal  
    ##                       3.530e+00                        4.647e+00                        4.770e+00  
    ##        mnth9:windspeed.unnormal        mnth10:windspeed.unnormal        mnth11:windspeed.unnormal  
    ##                       6.594e+00                        5.977e+00                        7.840e+00  
    ##       mnth12:windspeed.unnormal                     hr1:holiday1                     hr2:holiday1  
    ##                       5.150e+00                       -5.183e+00                       -5.319e+00  
    ##                    hr3:holiday1                     hr4:holiday1                     hr5:holiday1  
    ##                      -2.289e+01                       -1.273e+01                       -2.555e+01  
    ##                    hr6:holiday1                     hr7:holiday1                     hr8:holiday1  
    ##                      -8.735e+01                       -2.041e+02                       -2.781e+02  
    ##                    hr9:holiday1                    hr10:holiday1                    hr11:holiday1  
    ##                      -4.816e+01                        6.653e+01                        1.006e+02  
    ##                   hr12:holiday1                    hr13:holiday1                    hr14:holiday1  
    ##                       8.877e+01                        1.166e+02                        1.148e+02  
    ##                   hr15:holiday1                    hr16:holiday1                    hr17:holiday1  
    ##                       6.413e+01                       -4.112e+00                       -2.196e+02  
    ##                   hr18:holiday1                    hr19:holiday1                    hr20:holiday1  
    ##                      -2.029e+02                       -1.081e+02                       -9.032e+01  
    ##                   hr21:holiday1                    hr22:holiday1                    hr23:holiday1  
    ##                      -8.637e+01                       -7.620e+01                       -6.458e+01  
    ##                 hr1:weathersit2                  hr2:weathersit2                  hr3:weathersit2  
    ##                       1.461e+01                        1.653e+01                        2.397e+01  
    ##                 hr4:weathersit2                  hr5:weathersit2                  hr6:weathersit2  
    ##                       1.866e+01                        1.697e+01                        1.213e+01  
    ##                 hr7:weathersit2                  hr8:weathersit2                  hr9:weathersit2  
    ##                       4.740e+00                       -5.081e+00                       -1.395e+01  
    ##                hr10:weathersit2                 hr11:weathersit2                 hr12:weathersit2  
    ##                      -6.530e+00                       -2.230e+01                       -1.794e+01  
    ##                hr13:weathersit2                 hr14:weathersit2                 hr15:weathersit2  
    ##                      -2.525e+01                       -3.449e+01                       -1.512e+01  
    ##                hr16:weathersit2                 hr17:weathersit2                 hr18:weathersit2  
    ##                      -2.408e+01                       -1.117e+01                        3.665e+01  
    ##                hr19:weathersit2                 hr20:weathersit2                 hr21:weathersit2  
    ##                      -1.159e+01                       -2.720e+01                       -7.341e-01  
    ##                hr22:weathersit2                 hr23:weathersit2                  hr1:weathersit3  
    ##                      -2.931e+00                       -1.247e+01                        2.911e+01  
    ##                 hr2:weathersit3                  hr3:weathersit3                  hr4:weathersit3  
    ##                       1.896e+01                       -2.134e-02                        4.557e+01  
    ##                 hr5:weathersit3                  hr6:weathersit3                  hr7:weathersit3  
    ##                       4.139e+01                        7.574e+00                       -1.175e+02  
    ##                 hr8:weathersit3                  hr9:weathersit3                 hr10:weathersit3  
    ##                      -2.414e+02                       -7.124e+01                       -4.130e+01  
    ##                hr11:weathersit3                 hr12:weathersit3                 hr13:weathersit3  
    ##                      -1.205e+02                       -7.306e+01                       -7.295e+01  
    ##                hr14:weathersit3                 hr15:weathersit3                 hr16:weathersit3  
    ##                      -7.070e+01                       -5.803e+01                       -1.265e+02  
    ##                hr17:weathersit3                 hr18:weathersit3                 hr19:weathersit3  
    ##                      -1.317e+02                       -6.644e+01                       -6.265e+01  
    ##                hr20:weathersit3                 hr21:weathersit3                 hr22:weathersit3  
    ##                      -7.047e+01                       -5.621e+01                       -2.395e+01  
    ##                hr23:weathersit3                  hr1:weathersit4                  hr2:weathersit4  
    ##                      -3.239e+01                               NA                               NA  
    ##                 hr3:weathersit4                  hr4:weathersit4                  hr5:weathersit4  
    ##                              NA                               NA                               NA  
    ##                 hr6:weathersit4                  hr7:weathersit4                  hr8:weathersit4  
    ##                              NA                               NA                               NA  
    ##                 hr9:weathersit4                 hr10:weathersit4                 hr11:weathersit4  
    ##                              NA                               NA                               NA  
    ##                hr12:weathersit4                 hr13:weathersit4                 hr14:weathersit4  
    ##                              NA                               NA                               NA  
    ##                hr15:weathersit4                 hr16:weathersit4                 hr17:weathersit4  
    ##                              NA                               NA                               NA  
    ##                hr18:weathersit4                 hr19:weathersit4                 hr20:weathersit4  
    ##                              NA                               NA                               NA  
    ##                hr21:weathersit4                 hr22:weathersit4                 hr23:weathersit4  
    ##                              NA                               NA                               NA  
    ##                      hr1:temp.F                       hr2:temp.F                       hr3:temp.F  
    ##                      -7.749e-01                       -8.165e-01                       -6.917e-01  
    ##                      hr4:temp.F                       hr5:temp.F                       hr6:temp.F  
    ##                      -3.964e-01                       -6.557e-01                        2.261e-01  
    ##                      hr7:temp.F                       hr8:temp.F                       hr9:temp.F  
    ##                       9.292e-01                        9.602e-01                       -8.209e-01  
    ##                     hr10:temp.F                      hr11:temp.F                      hr12:temp.F  
    ##                       4.467e-01                        1.141e+00                        8.999e-01  
    ##                     hr13:temp.F                      hr14:temp.F                      hr15:temp.F  
    ##                       2.555e-01                        3.447e-01                        7.400e-01  
    ##                     hr16:temp.F                      hr17:temp.F                      hr18:temp.F  
    ##                       1.704e+00                        3.825e+00                        2.377e+00  
    ##                     hr19:temp.F                      hr20:temp.F                      hr21:temp.F  
    ##                       3.627e+00                        4.126e+00                        2.635e+00  
    ##                     hr22:temp.F                      hr23:temp.F                 hr1:hum.unnormal  
    ##                       2.717e+00                        1.056e+00                       -2.024e-01  
    ##                hr2:hum.unnormal                 hr3:hum.unnormal                 hr4:hum.unnormal  
    ##                       1.587e-03                        9.327e-02                       -4.176e-01  
    ##                hr5:hum.unnormal                 hr6:hum.unnormal                 hr7:hum.unnormal  
    ##                      -2.470e-01                       -5.963e-01                       -1.216e+00  
    ##                hr8:hum.unnormal                 hr9:hum.unnormal                hr10:hum.unnormal  
    ##                      -1.223e+00                       -7.061e-02                        2.059e-02  
    ##               hr11:hum.unnormal                hr12:hum.unnormal                hr13:hum.unnormal  
    ##                      -1.085e-01                       -7.293e-01                       -7.248e-01  
    ##               hr14:hum.unnormal                hr15:hum.unnormal                hr16:hum.unnormal  
    ##                      -2.903e-01                       -8.986e-01                       -1.039e+00  
    ##               hr17:hum.unnormal                hr18:hum.unnormal                hr19:hum.unnormal  
    ##                      -3.350e+00                       -3.407e+00                       -2.129e+00  
    ##               hr20:hum.unnormal                hr21:hum.unnormal                hr22:hum.unnormal  
    ##                      -1.177e+00                       -6.515e-01                       -4.388e-01  
    ##               hr23:hum.unnormal             holiday1:weathersit2             holiday1:weathersit3  
    ##                      -1.901e-01                        3.470e+01                        4.857e+01  
    ##            holiday1:weathersit4            holiday1:hum.unnormal      holiday1:windspeed.unnormal  
    ##                              NA                        4.781e-01                        1.071e+00  
    ##             weathersit2:atemp.F              weathersit3:atemp.F              weathersit4:atemp.F  
    ##                       1.003e+00                        2.447e+00                               NA  
    ##        weathersit2:hum.unnormal         weathersit3:hum.unnormal         weathersit4:hum.unnormal  
    ##                      -6.495e-01                       -1.265e+00                               NA  
    ##  weathersit2:windspeed.unnormal   weathersit3:windspeed.unnormal   weathersit4:windspeed.unnormal  
    ##                      -4.721e-01                        2.108e+00                               NA  
    ##                  temp.F:atemp.F             atemp.F:hum.unnormal  hum.unnormal:windspeed.unnormal  
    ##                       6.804e-01                       -1.823e-02                       -1.715e-02

``` r
# Examine performance of this multiple linear regression model on the test data after prediction
predict.mlr2 <- postResample(predict(fit.mlr2, newdata = hour.test.data), obs = hour.test.data$cnt)
```

``` r
# use aic predictors (1st order and interactions)
set.seed(7)
fit.mlr3 <- train(fit.aic3$terms,
                  data = hour.training.data2,
                  method = "lm",
                  preProcess = c("center", "scale"),
                  trControl = trainControl(method = "cv", number = 10))
fit.mlr3
```

    ## Linear Regression 
    ## 
    ## 1743 samples
    ##   10 predictor
    ## 
    ## Pre-processing: centered (393), scaled (393) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1568, 1569, 1569, 1569, 1569, 1569, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared   MAE     
    ##   48.09445  0.9300816  32.68957
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

``` r
# variables used in fit
fit.aic3
```

    ## 
    ## Call:
    ## lm(formula = cnt ~ season + yr + mnth + hr + holiday + weathersit + 
    ##     temp.F + atemp.F + hum.unnormal + windspeed.unnormal + season:yr + 
    ##     season:hr + season:atemp.F + season:windspeed.unnormal + 
    ##     yr:mnth + yr:hr + yr:holiday + yr:weathersit + yr:atemp.F + 
    ##     mnth:holiday + mnth:weathersit + mnth:temp.F + mnth:atemp.F + 
    ##     mnth:windspeed.unnormal + hr:holiday + hr:weathersit + hr:temp.F + 
    ##     hr:hum.unnormal + holiday:weathersit + weathersit:atemp.F + 
    ##     weathersit:hum.unnormal + weathersit:windspeed.unnormal + 
    ##     temp.F:atemp.F + atemp.F:hum.unnormal + atemp.F:windspeed.unnormal, 
    ##     data = hour.training.data2, verbose = FALSE)
    ## 
    ## Coefficients:
    ##                     (Intercept)                     seasonspring                     seasonsummer  
    ##                      -1.054e+02                        5.565e+01                        3.297e+02  
    ##                      seasonfall                           yr2012                            mnth2  
    ##                       2.780e+02                       -5.764e+01                        1.130e+02  
    ##                           mnth3                            mnth4                            mnth5  
    ##                      -8.750e+01                       -6.294e+01                       -1.910e+02  
    ##                           mnth6                            mnth7                            mnth8  
    ##                      -1.276e+02                        2.904e+00                       -2.911e+02  
    ##                           mnth9                           mnth10                           mnth11  
    ##                      -7.214e+02                       -4.500e+02                       -3.207e+02  
    ##                          mnth12                              hr1                              hr2  
    ##                      -4.024e+02                        2.862e+01                        1.360e+01  
    ##                             hr3                              hr4                              hr5  
    ##                      -4.019e-01                        3.233e+01                        2.487e+01  
    ##                             hr6                              hr7                              hr8  
    ##                       4.282e+01                        1.164e+02                        2.140e+02  
    ##                             hr9                             hr10                             hr11  
    ##                       1.058e+02                        2.297e+00                       -2.084e+01  
    ##                            hr12                             hr13                             hr14  
    ##                       2.247e+01                        4.641e+01                        2.679e+01  
    ##                            hr15                             hr16                             hr17  
    ##                       3.245e+01                        3.816e+01                        1.878e+02  
    ##                            hr18                             hr19                             hr20  
    ##                       2.147e+02                        5.282e+01                       -2.882e+01  
    ##                            hr21                             hr22                             hr23  
    ##                      -3.045e+01                       -5.949e+01                       -2.350e+01  
    ##                        holiday1                      weathersit2                      weathersit3  
    ##                      -3.563e+01                        4.301e+01                        1.120e+02  
    ##                     weathersit4                           temp.F                          atemp.F  
    ##                      -9.894e+01                        2.352e+00                        1.082e+00  
    ##                    hum.unnormal               windspeed.unnormal              seasonspring:yr2012  
    ##                       1.535e+00                       -5.566e-01                        1.478e+01  
    ##             seasonsummer:yr2012                seasonfall:yr2012                 seasonspring:hr1  
    ##                      -2.197e+02                       -1.596e+01                        1.223e+01  
    ##                seasonsummer:hr1                   seasonfall:hr1                 seasonspring:hr2  
    ##                       1.377e+01                        1.217e+01                       -4.698e-01  
    ##                seasonsummer:hr2                   seasonfall:hr2                 seasonspring:hr3  
    ##                       6.362e+00                        5.531e-01                       -6.879e+00  
    ##                seasonsummer:hr3                   seasonfall:hr3                 seasonspring:hr4  
    ##                      -2.565e+00                       -7.373e-01                       -1.367e+01  
    ##                seasonsummer:hr4                   seasonfall:hr4                 seasonspring:hr5  
    ##                      -8.784e+00                       -1.317e+00                       -2.211e+00  
    ##                seasonsummer:hr5                   seasonfall:hr5                 seasonspring:hr6  
    ##                       1.206e+01                        1.397e+01                        1.468e+01  
    ##                seasonsummer:hr6                   seasonfall:hr6                 seasonspring:hr7  
    ##                       4.189e+01                        5.742e+01                        9.593e+01  
    ##                seasonsummer:hr7                   seasonfall:hr7                 seasonspring:hr8  
    ##                       1.517e+02                        1.874e+02                        1.239e+02  
    ##                seasonsummer:hr8                   seasonfall:hr8                 seasonspring:hr9  
    ##                       1.473e+02                        2.584e+02                        7.746e+01  
    ##                seasonsummer:hr9                   seasonfall:hr9                seasonspring:hr10  
    ##                       1.169e+02                        1.282e+02                        2.955e+01  
    ##               seasonsummer:hr10                  seasonfall:hr10                seasonspring:hr11  
    ##                       4.767e+01                        5.463e+01                        3.576e+01  
    ##               seasonsummer:hr11                  seasonfall:hr11                seasonspring:hr12  
    ##                       5.229e+01                        7.838e+01                        8.206e+01  
    ##               seasonsummer:hr12                  seasonfall:hr12                seasonspring:hr13  
    ##                       9.582e+01                        1.124e+02                        1.117e+02  
    ##               seasonsummer:hr13                  seasonfall:hr13                seasonspring:hr14  
    ##                       1.210e+02                        1.298e+02                        8.329e+01  
    ##               seasonsummer:hr14                  seasonfall:hr14                seasonspring:hr15  
    ##                       1.143e+02                        1.092e+02                        7.032e+01  
    ##               seasonsummer:hr15                  seasonfall:hr15                seasonspring:hr16  
    ##                       9.120e+01                        1.120e+02                        8.278e+01  
    ##               seasonsummer:hr16                  seasonfall:hr16                seasonspring:hr17  
    ##                       1.266e+02                        1.690e+02                        9.332e+01  
    ##               seasonsummer:hr17                  seasonfall:hr17                seasonspring:hr18  
    ##                       1.950e+02                        2.553e+02                        1.587e+02  
    ##               seasonsummer:hr18                  seasonfall:hr18                seasonspring:hr19  
    ##                       2.869e+02                        2.560e+02                        7.696e+01  
    ##               seasonsummer:hr19                  seasonfall:hr19                seasonspring:hr20  
    ##                       1.297e+02                        1.306e+02                       -7.174e+00  
    ##               seasonsummer:hr20                  seasonfall:hr20                seasonspring:hr21  
    ##                       1.829e+01                        5.111e+01                        8.167e+00  
    ##               seasonsummer:hr21                  seasonfall:hr21                seasonspring:hr22  
    ##                       1.622e+01                        3.484e+01                       -2.419e+01  
    ##               seasonsummer:hr22                  seasonfall:hr22                seasonspring:hr23  
    ##                      -2.765e+01                        6.446e+00                       -1.631e+01  
    ##               seasonsummer:hr23                  seasonfall:hr23             seasonspring:atemp.F  
    ##                      -1.493e+01                        8.750e+00                       -1.497e+00  
    ##            seasonsummer:atemp.F               seasonfall:atemp.F  seasonspring:windspeed.unnormal  
    ##                      -4.607e+00                       -6.316e+00                       -2.583e+00  
    ## seasonsummer:windspeed.unnormal    seasonfall:windspeed.unnormal                     yr2012:mnth2  
    ##                      -5.155e+00                       -7.130e+00                       -8.520e+00  
    ##                    yr2012:mnth3                     yr2012:mnth4                     yr2012:mnth5  
    ##                       2.485e+01                       -3.100e+01                       -1.200e+02  
    ##                    yr2012:mnth6                     yr2012:mnth7                     yr2012:mnth8  
    ##                       1.466e+02                        1.842e+02                        1.741e+02  
    ##                    yr2012:mnth9                    yr2012:mnth10                    yr2012:mnth11  
    ##                       1.817e+02                       -1.591e+01                       -1.373e+01  
    ##                   yr2012:mnth12                       yr2012:hr1                       yr2012:hr2  
    ##                              NA                       -5.476e+00                        2.846e+00  
    ##                      yr2012:hr3                       yr2012:hr4                       yr2012:hr5  
    ##                       7.226e-01                       -2.557e+00                        1.496e+01  
    ##                      yr2012:hr6                       yr2012:hr7                       yr2012:hr8  
    ##                       4.219e+01                        1.242e+02                        2.190e+02  
    ##                      yr2012:hr9                      yr2012:hr10                      yr2012:hr11  
    ##                       1.006e+02                        4.881e+01                        5.541e+01  
    ##                     yr2012:hr12                      yr2012:hr13                      yr2012:hr14  
    ##                       7.796e+01                        6.689e+01                        6.025e+01  
    ##                     yr2012:hr15                      yr2012:hr16                      yr2012:hr17  
    ##                       7.637e+01                        1.135e+02                        2.069e+02  
    ##                     yr2012:hr18                      yr2012:hr19                      yr2012:hr20  
    ##                       1.949e+02                        1.278e+02                        9.761e+01  
    ##                     yr2012:hr21                      yr2012:hr22                      yr2012:hr23  
    ##                       5.963e+01                        4.174e+01                        3.227e+01  
    ##                 yr2012:holiday1               yr2012:weathersit2               yr2012:weathersit3  
    ##                       1.416e+01                       -1.468e+01                       -5.320e+01  
    ##              yr2012:weathersit4                   yr2012:atemp.F                   mnth2:holiday1  
    ##                              NA                        1.678e+00                       -1.468e+01  
    ##                  mnth3:holiday1                   mnth4:holiday1                   mnth5:holiday1  
    ##                              NA                        7.341e+01                        5.655e+01  
    ##                  mnth6:holiday1                   mnth7:holiday1                   mnth8:holiday1  
    ##                              NA                               NA                               NA  
    ##                  mnth9:holiday1                  mnth10:holiday1                  mnth11:holiday1  
    ##                      -4.046e+01                        6.616e+01                        6.688e+01  
    ##                 mnth12:holiday1                mnth2:weathersit2                mnth3:weathersit2  
    ##                              NA                        2.530e+01                       -1.661e+01  
    ##               mnth4:weathersit2                mnth5:weathersit2                mnth6:weathersit2  
    ##                      -1.823e+01                       -4.991e+01                       -8.163e+01  
    ##               mnth7:weathersit2                mnth8:weathersit2                mnth9:weathersit2  
    ##                      -5.586e+01                       -5.253e+01                       -3.226e+01  
    ##              mnth10:weathersit2               mnth11:weathersit2               mnth12:weathersit2  
    ##                      -4.069e+01                       -2.673e+01                       -2.620e+01  
    ##               mnth2:weathersit3                mnth3:weathersit3                mnth4:weathersit3  
    ##                      -1.002e+02                       -1.052e+02                       -1.290e+02  
    ##               mnth5:weathersit3                mnth6:weathersit3                mnth7:weathersit3  
    ##                      -1.519e+02                       -2.456e+02                       -1.957e+02  
    ##               mnth8:weathersit3                mnth9:weathersit3               mnth10:weathersit3  
    ##                      -1.605e+02                       -1.874e+02                       -1.132e+02  
    ##              mnth11:weathersit3               mnth12:weathersit3                mnth2:weathersit4  
    ##                      -1.524e+02                       -4.478e+01                               NA  
    ##               mnth3:weathersit4                mnth4:weathersit4                mnth5:weathersit4  
    ##                              NA                               NA                               NA  
    ##               mnth6:weathersit4                mnth7:weathersit4                mnth8:weathersit4  
    ##                              NA                               NA                               NA  
    ##               mnth9:weathersit4               mnth10:weathersit4               mnth11:weathersit4  
    ##                              NA                               NA                               NA  
    ##              mnth12:weathersit4                     mnth2:temp.F                     mnth3:temp.F  
    ##                              NA                       -6.452e+00                       -1.032e+00  
    ##                    mnth4:temp.F                     mnth5:temp.F                     mnth6:temp.F  
    ##                       1.609e+00                       -1.161e+00                       -1.141e+00  
    ##                    mnth7:temp.F                     mnth8:temp.F                     mnth9:temp.F  
    ##                      -1.174e+01                       -4.789e+00                        8.279e+00  
    ##                   mnth10:temp.F                    mnth11:temp.F                    mnth12:temp.F  
    ##                       1.924e+00                        3.742e-01                        4.753e+00  
    ##                   mnth2:atemp.F                    mnth3:atemp.F                    mnth4:atemp.F  
    ##                       3.450e+00                        3.486e+00                        1.232e+00  
    ##                   mnth5:atemp.F                    mnth6:atemp.F                    mnth7:atemp.F  
    ##                       6.565e+00                        5.601e+00                        1.372e+01  
    ##                   mnth8:atemp.F                    mnth9:atemp.F                   mnth10:atemp.F  
    ##                       1.068e+01                        3.920e+00                        8.754e+00  
    ##                  mnth11:atemp.F                   mnth12:atemp.F         mnth2:windspeed.unnormal  
    ##                       7.472e+00                        4.835e+00                        2.714e+00  
    ##        mnth3:windspeed.unnormal         mnth4:windspeed.unnormal         mnth5:windspeed.unnormal  
    ##                       5.163e-01                        8.065e-01                        1.430e+00  
    ##        mnth6:windspeed.unnormal         mnth7:windspeed.unnormal         mnth8:windspeed.unnormal  
    ##                       1.785e+00                        2.630e+00                        3.056e+00  
    ##        mnth9:windspeed.unnormal        mnth10:windspeed.unnormal        mnth11:windspeed.unnormal  
    ##                       5.529e+00                        4.778e+00                        6.485e+00  
    ##       mnth12:windspeed.unnormal                     hr1:holiday1                     hr2:holiday1  
    ##                       3.550e+00                       -4.442e+00                       -5.403e+00  
    ##                    hr3:holiday1                     hr4:holiday1                     hr5:holiday1  
    ##                      -2.441e+01                       -1.285e+01                       -2.925e+01  
    ##                    hr6:holiday1                     hr7:holiday1                     hr8:holiday1  
    ##                      -8.955e+01                       -2.074e+02                       -2.776e+02  
    ##                    hr9:holiday1                    hr10:holiday1                    hr11:holiday1  
    ##                      -5.038e+01                        6.733e+01                        1.016e+02  
    ##                   hr12:holiday1                    hr13:holiday1                    hr14:holiday1  
    ##                       9.042e+01                        1.150e+02                        1.123e+02  
    ##                   hr15:holiday1                    hr16:holiday1                    hr17:holiday1  
    ##                       6.608e+01                       -3.380e+00                       -2.170e+02  
    ##                   hr18:holiday1                    hr19:holiday1                    hr20:holiday1  
    ##                      -2.035e+02                       -1.098e+02                       -8.863e+01  
    ##                   hr21:holiday1                    hr22:holiday1                    hr23:holiday1  
    ##                      -8.331e+01                       -7.318e+01                       -6.167e+01  
    ##                 hr1:weathersit2                  hr2:weathersit2                  hr3:weathersit2  
    ##                       1.430e+01                        1.533e+01                        2.337e+01  
    ##                 hr4:weathersit2                  hr5:weathersit2                  hr6:weathersit2  
    ##                       1.842e+01                        1.599e+01                        1.039e+01  
    ##                 hr7:weathersit2                  hr8:weathersit2                  hr9:weathersit2  
    ##                       3.171e+00                       -5.905e+00                       -1.455e+01  
    ##                hr10:weathersit2                 hr11:weathersit2                 hr12:weathersit2  
    ##                      -7.948e+00                       -2.540e+01                       -2.096e+01  
    ##                hr13:weathersit2                 hr14:weathersit2                 hr15:weathersit2  
    ##                      -2.787e+01                       -3.983e+01                       -1.772e+01  
    ##                hr16:weathersit2                 hr17:weathersit2                 hr18:weathersit2  
    ##                      -2.631e+01                       -1.338e+01                        3.679e+01  
    ##                hr19:weathersit2                 hr20:weathersit2                 hr21:weathersit2  
    ##                      -1.372e+01                       -2.756e+01                       -1.742e+00  
    ##                hr22:weathersit2                 hr23:weathersit2                  hr1:weathersit3  
    ##                      -2.020e+00                       -1.148e+01                        2.856e+01  
    ##                 hr2:weathersit3                  hr3:weathersit3                  hr4:weathersit3  
    ##                       1.942e+01                       -4.651e-01                        4.845e+01  
    ##                 hr5:weathersit3                  hr6:weathersit3                  hr7:weathersit3  
    ##                       4.310e+01                        1.011e+01                       -1.177e+02  
    ##                 hr8:weathersit3                  hr9:weathersit3                 hr10:weathersit3  
    ##                      -2.376e+02                       -7.212e+01                       -4.216e+01  
    ##                hr11:weathersit3                 hr12:weathersit3                 hr13:weathersit3  
    ##                      -1.244e+02                       -7.990e+01                       -7.825e+01  
    ##                hr14:weathersit3                 hr15:weathersit3                 hr16:weathersit3  
    ##                      -8.083e+01                       -6.392e+01                       -1.323e+02  
    ##                hr17:weathersit3                 hr18:weathersit3                 hr19:weathersit3  
    ##                      -1.372e+02                       -6.678e+01                       -6.702e+01  
    ##                hr20:weathersit3                 hr21:weathersit3                 hr22:weathersit3  
    ##                      -7.136e+01                       -5.522e+01                       -1.989e+01  
    ##                hr23:weathersit3                  hr1:weathersit4                  hr2:weathersit4  
    ##                      -3.079e+01                               NA                               NA  
    ##                 hr3:weathersit4                  hr4:weathersit4                  hr5:weathersit4  
    ##                              NA                               NA                               NA  
    ##                 hr6:weathersit4                  hr7:weathersit4                  hr8:weathersit4  
    ##                              NA                               NA                               NA  
    ##                 hr9:weathersit4                 hr10:weathersit4                 hr11:weathersit4  
    ##                              NA                               NA                               NA  
    ##                hr12:weathersit4                 hr13:weathersit4                 hr14:weathersit4  
    ##                              NA                               NA                               NA  
    ##                hr15:weathersit4                 hr16:weathersit4                 hr17:weathersit4  
    ##                              NA                               NA                               NA  
    ##                hr18:weathersit4                 hr19:weathersit4                 hr20:weathersit4  
    ##                              NA                               NA                               NA  
    ##                hr21:weathersit4                 hr22:weathersit4                 hr23:weathersit4  
    ##                              NA                               NA                               NA  
    ##                      hr1:temp.F                       hr2:temp.F                       hr3:temp.F  
    ##                      -7.240e-01                       -8.103e-01                       -6.690e-01  
    ##                      hr4:temp.F                       hr5:temp.F                       hr6:temp.F  
    ##                      -4.319e-01                       -7.137e-01                        2.327e-01  
    ##                      hr7:temp.F                       hr8:temp.F                       hr9:temp.F  
    ##                       9.383e-01                        1.035e+00                       -6.844e-01  
    ##                     hr10:temp.F                      hr11:temp.F                      hr12:temp.F  
    ##                       5.961e-01                        1.259e+00                        1.030e+00  
    ##                     hr13:temp.F                      hr14:temp.F                      hr15:temp.F  
    ##                       3.755e-01                        4.361e-01                        9.017e-01  
    ##                     hr16:temp.F                      hr17:temp.F                      hr18:temp.F  
    ##                       1.854e+00                        4.006e+00                        2.579e+00  
    ##                     hr19:temp.F                      hr20:temp.F                      hr21:temp.F  
    ##                       3.910e+00                        4.267e+00                        2.910e+00  
    ##                     hr22:temp.F                      hr23:temp.F                 hr1:hum.unnormal  
    ##                       2.858e+00                        1.142e+00                       -2.425e-01  
    ##                hr2:hum.unnormal                 hr3:hum.unnormal                 hr4:hum.unnormal  
    ##                      -2.205e-03                        7.740e-02                       -4.798e-01  
    ##                hr5:hum.unnormal                 hr6:hum.unnormal                 hr7:hum.unnormal  
    ##                      -1.810e-01                       -5.771e-01                       -1.136e+00  
    ##                hr8:hum.unnormal                 hr9:hum.unnormal                hr10:hum.unnormal  
    ##                      -1.209e+00                        5.967e-02                        1.306e-01  
    ##               hr11:hum.unnormal                hr12:hum.unnormal                hr13:hum.unnormal  
    ##                       9.744e-02                       -5.010e-01                       -4.599e-01  
    ##               hr14:hum.unnormal                hr15:hum.unnormal                hr16:hum.unnormal  
    ##                       3.732e-02                       -6.928e-01                       -8.029e-01  
    ##               hr17:hum.unnormal                hr18:hum.unnormal                hr19:hum.unnormal  
    ##                      -3.114e+00                       -3.189e+00                       -1.893e+00  
    ##               hr20:hum.unnormal                hr21:hum.unnormal                hr22:hum.unnormal  
    ##                      -1.011e+00                       -5.564e-01                       -4.266e-01  
    ##               hr23:hum.unnormal             holiday1:weathersit2             holiday1:weathersit3  
    ##                      -1.610e-01                        3.526e+01                        5.555e+01  
    ##            holiday1:weathersit4              weathersit2:atemp.F              weathersit3:atemp.F  
    ##                              NA                        9.518e-01                        2.577e+00  
    ##             weathersit4:atemp.F         weathersit2:hum.unnormal         weathersit3:hum.unnormal  
    ##                              NA                       -8.622e-01                       -1.634e+00  
    ##        weathersit4:hum.unnormal   weathersit2:windspeed.unnormal   weathersit3:windspeed.unnormal  
    ##                              NA                       -5.481e-01                        1.967e+00  
    ##  weathersit4:windspeed.unnormal                   temp.F:atemp.F             atemp.F:hum.unnormal  
    ##                              NA                       -4.961e-02                       -1.785e-02  
    ##      atemp.F:windspeed.unnormal  
    ##                       3.547e-02

``` r
# Examine performance of this multiple linear regression model on the test data after prediction
predict.mlr3 <- postResample(predict(fit.mlr3, newdata = hour.test.data), obs = hour.test.data$cnt)
```

## Ensemble Tree Model

### Random Forest Model

#### What is Random Forest Model?

The random forest model is a type of tree based method where we create
multiple trees from bootstrap samples of the data and then average the
results. This process is done by first creating a bootstrap sample of
the data and then training a tree on this sample where we call the
prediction for a given set of *x* values *ŷ*<sup>\*1</sup>(*x*). This
process is then repeated a *B* number of times to obtain
*ŷ*<sup>\**j*</sup>(*x*), *j* = 1, ..., *B*. The final prediction is the
average of these predictions
$\\hat{y}(x) = \\frac{1}{B}\\sum\_{j=1}^{B}\\hat{y}^{\*j}(x)$. For each
of these bootstrap sample/tree fits a random subset of predictors is
chosen becasue if a really strong predictor exists, every bootstrap tree
will probably use it as the first split. By selecting a subset of
predictors, a good predictor or two won’t dominate the tree fits. The
number of predictors for a random forest regression tree is usually
*m* = *p*/3 where *m* is the random predictors chosen and *p* is the
full set of possible predictors. Cross-validation can also be used to
select these random predictors as we did in our random forest model.

We first fit a random forest model using default tuning parameters,
which produced a result with very large mtry values.

``` r
# Fit random forest model
set.seed(7)
fit.random.forest.trial <- train(fit.aic$terms,
                           data = hour.training.data2,
                           method = "rf",
                           preProcess = c("center", "scale"),
                           trControl = trainControl(method = "cv", number = 10),
                           verbose = FALSE)
fit.random.forest.trial
```

    ## Random Forest 
    ## 
    ## 1743 samples
    ##    8 predictor
    ## 
    ## Pre-processing: centered (44), scaled (44) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1568, 1569, 1569, 1569, 1569, 1569, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE       Rsquared   MAE     
    ##    2    120.58913  0.7998780  90.92849
    ##   23     61.86774  0.8847339  41.11062
    ##   44     61.61511  0.8841101  39.71443
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 44.

We can manually tune our parameters to only include 1 to the number of
predictors. We will use the result from this model to do predictions.

``` r
set.seed(7)
fit.random.forest <- train(fit.aic$terms,
                           data = hour.training.data2,
                           method = "rf",
                           preProcess = c("center", "scale"),
                           trControl = trainControl(method = "cv", number = 10),
                           tuneGrid = data.frame(mtry = 1:(ncol(hour.training.data2) -1)))
fit.random.forest
```

    ## Random Forest 
    ## 
    ## 1743 samples
    ##    8 predictor
    ## 
    ## Pre-processing: centered (44), scaled (44) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1568, 1569, 1569, 1569, 1569, 1569, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE       Rsquared   MAE      
    ##    1    156.47473  0.7321613  120.08896
    ##    2    120.26453  0.7973103   90.60258
    ##    3     97.98913  0.8300044   72.56582
    ##    4     84.65290  0.8501054   61.87810
    ##    5     77.14741  0.8614257   55.80681
    ##    6     72.18630  0.8707242   51.63537
    ##    7     69.29402  0.8751672   49.01036
    ##    8     67.33540  0.8785049   47.33283
    ##    9     66.32093  0.8790240   46.23798
    ##   10     65.18360  0.8812858   45.28542
    ##   11     64.52238  0.8817790   44.55295
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 11.

``` r
# Examine performance of random forest model on the test data after prediction
predict.rf <- postResample(predict(fit.random.forest, newdata = hour.test.data), obs = hour.test.data$cnt)
```

### Boosted Tree Model

#### What is Boosted Tree Model?

The boosted tree model is a type of tree based method where we grow our
trees in a sequential manner, each tree we create will be based off the
previous tree so we can update our prediction as we go. For example,
we’d fit our model and get a prediction, then create a new model based
off the previous, update the prediction on this new model, and we’d
repeat this process until we decide to stop. Boosted tree model slowly
trains the trees to ensure we don’t overfit to our training data. How
this is actually done is we create new residuals based off
`observed - new predictions`, fit a tree to those residuals to get new
predictions *ŷ*, then update our predictions again by a scaled down
version of the new predictions *λ**ŷ*<sup>*b*</sup> (here *λ* is the
growth rate tuning parameter, which keeps us from growing our
predictions too quickly). We repeat this process a total of `B` times.
We can use cross validation to select what *λ*, `d` and `B` should be.
Formula used to update predictions is *ŷ* = *ŷ* + *λ**ŷ*<sup>*b*</sup>.

For the boosted tree model, we first let the model pick default tuning
parameters, from the result we will further fine tune the parameters to
see if we can get even better results.

``` r
set.seed(7)
fit.boosted.trial <- train(fit.aic$terms,
                     data = hour.training.data2,
                     method = "gbm",
                     preProcess = c("center", "scale"),
                     trControl = trainControl(method = "cv", number = 10),
                     verbose = FALSE)
fit.boosted.trial
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 1743 samples
    ##    8 predictor
    ## 
    ## Pre-processing: centered (44), scaled (44) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1568, 1569, 1569, 1569, 1569, 1569, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  RMSE       Rsquared   MAE      
    ##   1                   50      129.28911  0.5988282  101.05321
    ##   1                  100      109.76560  0.6902563   83.25041
    ##   1                  150       99.21061  0.7303486   72.82004
    ##   2                   50      107.65478  0.7130148   82.77594
    ##   2                  100       89.62497  0.7768414   65.15802
    ##   2                  150       82.71760  0.7979855   59.68500
    ##   3                   50       95.62660  0.7611400   71.69103
    ##   3                  100       81.15381  0.8075174   58.15588
    ##   3                  150       76.34549  0.8248764   54.41544
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## Tuning parameter 'n.minobsinnode' was held constant at
    ##  a value of 10
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10.

``` r
set.seed(7)
fit.boosted <- train(fit.aic$terms,
                     data = hour.training.data2,
                     method = "gbm",
                     preProcess = c("center", "scale"),
                     trControl = trainControl(method = "cv", number = 10),
                     verbose = FALSE,
                     tuneGrid = expand.grid(interaction.depth = c(3:10),
                                            n.trees = (3:10)*50,
                                            shrinkage = 0.1,
                                            n.minobsinnode = 10))
fit.boosted
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 1743 samples
    ##    8 predictor
    ## 
    ## Pre-processing: centered (44), scaled (44) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1568, 1569, 1569, 1569, 1569, 1569, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  RMSE      Rsquared   MAE     
    ##    3                 150      76.11666  0.8252849  54.48880
    ##    3                 200      73.46728  0.8366174  52.37520
    ##    3                 250      71.87105  0.8433812  50.67997
    ##    3                 300      70.50822  0.8485410  49.41034
    ##    3                 350      69.35891  0.8526832  48.42859
    ##    3                 400      68.26765  0.8572838  47.55206
    ##    3                 450      67.52243  0.8604397  46.89469
    ##    3                 500      66.74404  0.8634880  46.20982
    ##    4                 150      71.91104  0.8431787  50.60661
    ##    4                 200      69.77970  0.8514632  48.83352
    ##    4                 250      68.12489  0.8581853  47.34888
    ##    4                 300      66.54363  0.8645023  45.98036
    ##    4                 350      65.50735  0.8683197  45.08788
    ##    4                 400      64.71343  0.8713943  44.29017
    ##    4                 450      63.58165  0.8758989  43.48950
    ##    4                 500      62.58709  0.8796009  42.83014
    ##    5                 150      68.91630  0.8546740  47.74661
    ##    5                 200      66.45538  0.8645881  45.89965
    ##    5                 250      64.83382  0.8706220  44.52650
    ##    5                 300      63.36544  0.8766410  43.34095
    ##    5                 350      62.36092  0.8805142  42.42763
    ##    5                 400      61.37287  0.8841972  41.71500
    ##    5                 450      60.46249  0.8874778  41.02130
    ##    5                 500      59.53664  0.8909788  40.50747
    ##    6                 150      67.08053  0.8624481  45.85719
    ##    6                 200      64.42442  0.8725704  44.00019
    ##    6                 250      62.81706  0.8784927  42.75519
    ##    6                 300      61.71557  0.8828556  41.93302
    ##    6                 350      60.93540  0.8858156  41.25543
    ##    6                 400      59.94678  0.8893109  40.67952
    ##    6                 450      58.70863  0.8937803  39.86062
    ##    6                 500      58.09539  0.8959463  39.50990
    ##    7                 150      65.03141  0.8705060  44.80809
    ##    7                 200      62.84227  0.8789482  43.00646
    ##    7                 250      61.26336  0.8847774  41.80124
    ##    7                 300      59.85483  0.8902806  40.71469
    ##    7                 350      58.68589  0.8942366  39.85762
    ##    7                 400      57.39598  0.8986780  39.12990
    ##    7                 450      56.67345  0.9014362  38.50013
    ##    7                 500      56.19229  0.9032488  38.19994
    ##    8                 150      62.55764  0.8801393  42.37489
    ##    8                 200      60.33422  0.8886633  40.75926
    ##    8                 250      59.17437  0.8928482  39.84261
    ##    8                 300      57.89014  0.8972169  38.90400
    ##    8                 350      57.04779  0.9002148  38.31895
    ##    8                 400      56.52551  0.9019077  37.95729
    ##    8                 450      55.84915  0.9043501  37.55416
    ##    8                 500      55.26062  0.9063874  37.13249
    ##    9                 150      61.24373  0.8845816  41.64919
    ##    9                 200      58.76909  0.8940015  39.74010
    ##    9                 250      57.16828  0.8995741  38.58089
    ##    9                 300      56.28450  0.9025810  37.83747
    ##    9                 350      55.76598  0.9042632  37.32458
    ##    9                 400      55.17777  0.9064330  36.91382
    ##    9                 450      54.67502  0.9081304  36.50794
    ##    9                 500      53.99788  0.9104898  36.00987
    ##   10                 150      60.05599  0.8897592  40.47658
    ##   10                 200      58.25878  0.8962090  38.98720
    ##   10                 250      56.44821  0.9025509  37.82623
    ##   10                 300      55.52478  0.9056874  37.24684
    ##   10                 350      54.66522  0.9085453  36.53935
    ##   10                 400      53.88818  0.9112380  36.24561
    ##   10                 450      53.46062  0.9127889  35.90049
    ##   10                 500      53.04503  0.9143916  35.69348
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## Tuning parameter 'n.minobsinnode' was held constant at
    ##  a value of 10
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 500, interaction.depth = 10, shrinkage = 0.1 and n.minobsinnode = 10.

``` r
# Examine performance of boosted tree model on the test data after prediction
predict.boosted <- postResample(predict(fit.boosted, newdata = hour.test.data), obs = hour.test.data$cnt)
```

# Comparison

We compare all 6 models on the test set and see which model produced the
lowest root mean squared error value, which indicate that model out of
the 6 had the best prediction.

``` r
compare.rmse <- data.frame(predict.mlr0, 
                           predict.mlr1,
                           predict.mlr2,
                           predict.mlr3,
                           predict.rf,
                           predict.boosted)
colnames(compare.rmse) <- c("mlr manual", "mlr aic1", "mlr aic2", "mlr aic3", "random forest", "boosted tree")
compare.rmse
```

    ##          mlr manual   mlr aic1   mlr aic2   mlr aic3 random forest boosted tree
    ## RMSE     83.3460430 83.0716807 93.6222169 92.4850843    66.4436780   53.6434205
    ## Rsquared  0.7682955  0.7701019  0.7669428  0.7699773     0.8626506    0.9032807
    ## MAE      60.0562939 59.6966930 62.9936904 62.3185114    45.4053615   36.8779881

``` r
min.compare.rmse <- min(compare.rmse["RMSE",])
min.test <- compare.rmse["RMSE",] == min.compare.rmse
paste0("After comparing all models on the test set, the model with the best prediction (lowest root MSE value) is the ", colnames(compare.rmse)[min.test], " model.")
```

    ## [1] "After comparing all models on the test set, the model with the best prediction (lowest root MSE value) is the boosted tree model."
