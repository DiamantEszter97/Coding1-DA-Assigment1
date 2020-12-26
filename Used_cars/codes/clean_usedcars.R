rm(list=ls())

library(tidyverse)

# load data:



# make posting_date datetime data type:
cars_df <- small_cars_df %>% mutate(posting_date = as.Date(posting_date))
