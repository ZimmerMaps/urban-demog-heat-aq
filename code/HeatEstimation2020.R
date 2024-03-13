rm(list = ls())

# Load packages ####
library(tidyverse)
library(dplyr)
library(purrr)

setwd("/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demography : AQ : Heat/")

heat30Exp <- read.csv("data/heat/sdei-high-res-daily-uhe-1983-2016-csv-json/wbgtmax30-tabular/wbgtmax30_EXP.csv")
heat30ExpUseful = dplyr::select(heat30Exp, ID_HDC_G0, year, tot_days)

library(dplyr)

# Define a function to perform linear regression and generate predictions
perform_linear_regression <- function(city_df) {
  # Filter the data for the years you have values for
  training_data <- city_df %>%
    filter(year >= 1983 & year <= 2016)
  
  # Perform linear regression
  lm_model <- lm(tot_days ~ year, data = training_data)
  
  # Generate predictions for the years 2017-2020
  new_years <- data.frame(year = 2017:2020)
  predictions <- predict(lm_model, newdata = new_years)
  
  # Return predictions as a dataframe
  return(data.frame(year = new_years$year, tot_days = predictions))
}

# Group by city and apply the perform_linear_regression function
combined_predictions <- heat30ExpUseful %>%
  group_by(ID_HDC_G0) %>%
  nest() %>%
  mutate(predictions = purrr::map(data, perform_linear_regression)) %>%
  unnest(predictions)

final_output = dplyr::select(combined_predictions, ID_HDC_G0, year, tot_days)

finalMerged = rbind(heat30ExpUseful, final_output)

finalMerged$tot_days[finalMerged$tot_days < 0] <- 0 # make the values zero if they are <0

write.csv(finalMerged, "data/heat/Heat30Estimated2020.csv")

