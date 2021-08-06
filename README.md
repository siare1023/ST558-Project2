# Purpose of the Repo
The purpose of this repo is we will be using 6 models (4 linear regression, 1 random forest model, 1 boosted tree model) to make predictions on the total count of bike riders using data from the Bike Sharing Dataset (dataset can be found [here](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset)). This dataset has many categorical and numeric variables, along with 3 response variables. For our analysis, we will be working with almost all of the variables as predictors, and our response variable will be the total count of bike riders. We will do summarization and modeling using only Monday's data, then automate the report generating process to create analysis reports the other 6 days of the week. We will be selecting predictors using the `step()` function which chooses a model by AIC in a stepwise algorithm. As a result, which predictors we incorporate in our linear regression models and ensemble tree (specifically random forest and boosted tree) models may differ depending on which day of the week we look at. We'll randomly split the data into training and test sets and fit the 6 models on the training set. Ultimately we will fit the 6 models on test set and decide on which model produced the best prediction, which we judge by the smallest root mean squared error value.  

# List of R Packages Used
```
library(tidyverse)
library(dplyr)
library(knitr)
library(ggplot2)
library(caret)
library(gridExtra)
library(corrplot)
library(GGally)
```

# Markdown Outputs of 7 Analyses
The analysis for [Monday is available here](mondayAnalysis.md)\
The analysis for [Tuesday is available here](tuesdayAnalysis.md)\
The analysis for [Wednesday is available here](wednesdayAnalysis.md)\
The analysis for [Thursday is available here](thursdayAnalysis.md)\
The analysis for [Friday is available here](fridayAnalysis.md)\
The analysis for [Saturday is available here](saturdayAnalysis.md)\
The analysis for [Sunday is available here](sundayAnalysis.md)

# Code Used to Create Analyses
```
weekday <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
output_file <- paste0(weekday, "Analysis.md")
params = lapply(weekday, FUN = function(x){list(weekday = x)})
reports <- tibble(output_file, params)
apply(reports, MARGIN = 1,FUN = function(x){rmarkdown::render(input = "Project2_Tyler_Lucy.Rmd", output_file = x[[1]], params = x[[2]])})
```
