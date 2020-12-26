rm(list = ls())

library(tidyverse)

# aim of this code:
#      the file exceed the size of 1,3 GB but github only allows 100 MB file for uploading
#      therefore, this code aims for get rid of hose data that will be unnecessary for the analysis

# give file path for the whole code:
path_for_bigdf <- "C:/Users/diama/Documents/CEU-Business-Analytics-2020/Coding_1/data/raw/"
path_for_final <- "C:/Users/diama/Documents/CEU-BA-Assignments/Coding1_DataAnalysis2/Used_cars/data/raw/"

# load data
cars_df <- read.csv(paste0(path_for_bigdf, "vehicles.csv"))

# remove columns image_url, region_url, long, lat, VIN, drive, size, description, id
small_df <- cars_df %>% select(-c(image_url, region_url, long, lat, VIN, drive, size, description, id))

# save csv
write_csv(small_df, paste0(path_for_final, "cars.csv"))
