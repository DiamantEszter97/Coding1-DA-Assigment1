rm(list=ls())

library(tidyverse)

# aim of code:
#     prepare the data frame for the regression analysis by dropping errors and unnecessary data
#     only missing values and zero values are removed. The extreme values that might be measurement errors
#     may be removed during the analysis. It depends on whether they are extreme values
#     that relate to the analysis


# load data:
cars_df <- read.csv("https://raw.githubusercontent.com/DiamantEszter97/Coding1-DA2-Assigments/master/Used_cars/data/raw/cars.csv")

# separate posting_date by T into to columns:
cars_df <- separate(cars_df, posting_date, "T", into = c("posting_date", "garbage"))

# drop column garbage:
cars_df <- cars_df %>% select(-garbage)

# make posting_date datetime data type:
cars_df <- cars_df %>% mutate(posting_date = as.Date(posting_date))


# due to big size of the original data, the cars_df is saved into df:
df <- cars_df

# drop zero in column price and odometer:
df <- df %>% filter(price != 0) %>% filter(odometer != 0)

# save the data:
path <- "C:/Users/diama/Documents/CEU-BA-Assignments/Coding1_DataAnalysis2/Used_cars/data/clean/"
write.csv(df, paste0(path, "cars.csv"))
