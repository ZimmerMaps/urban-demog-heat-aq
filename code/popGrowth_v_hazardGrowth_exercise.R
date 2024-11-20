# Urban Demography / Heat / AQ - Plotting

rm(list = ls())

# load packages and set working directory ####
library(tidyverse)
library(rnaturalearth)
library(scales)
library(lemon)

setwd("/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demography : AQ : Heat/")

# load data ####
#MeanData = read.csv("data/merged/Final Merged Data/UCDB-Dem-Heat-AQ-Mean.csv")
AllData = read.csv("data/merged/Final Merged Data/UCDB-Dem-Heat-AQ-All.csv")

# Add demographic variables ####
#MeanData$YoungPop <- rowSums(MeanData[c('f_0', 'm_0', 'f_1', 'm_1', 'f_5', 'm_5', 'f_10', 'm_10')])
#MeanData$WorkingPop <- rowSums(MeanData[c('f_15', 'f_20', 'f_25', 'f_30', 'f_35', 'f_40', 'f_45', 'f_50', 'f_55', 'f_60', 'm_15', 'm_20', 'm_25', 'm_30', 'm_35', 'm_40', 'm_45', 'm_50', 'm_55', 'm_60')])
#MeanData$OldPop <- rowSums(MeanData[c('f_65', 'f_70', 'f_75', 'f_80', 'm_65', 'm_70', 'm_75', 'm_80')])
#MeanData$TotalPop <- rowSums(MeanData[c('YoungPop', 'WorkingPop', 'OldPop')])
#MeanData$DependencyRatio = (MeanData$YoungPop + MeanData$OldPop) / MeanData$WorkingPop

#MeanDataClipped = dplyr::select(MeanData, UrbanID, year, Name, country_iso, country_name, continent_name, latitude, longitude, YoungPop, WorkingPop, OldPop, TotalPop, DependencyRatio, MeanNO2, MeanPM25, MeanOzone, MeanHeat30)
#colnames(MeanDataClipped) = c('urbanid', 'year', 'city_name', 'country_iso', 'country_name', 'continent_name', 'latitude', 'longitude', 'YoungPop', 'WorkingPop', 'OldPop', 'TotalPop', 'DependencyRatio', 'MeanNO2', 'MeanPM25', 'MeanOzone', 'MeanHeatDays30')

# keep only complete cases
#MeanDataClipped <- MeanDataClipped[complete.cases(MeanDataClipped), ]

AllDataClipped = AllData %>%
  filter(year %in% c(2005:2020))

AllDataClipped$YoungPop <- rowSums(AllDataClipped[c('f_0', 'm_0', 'f_1', 'm_1', 'f_5', 'm_5', 'f_10', 'm_10')])
AllDataClipped$WorkingPop <- rowSums(AllDataClipped[c('f_15', 'f_20', 'f_25', 'f_30', 'f_35', 'f_40', 'f_45', 'f_50', 'f_55', 'f_60', 'm_15', 'm_20', 'm_25', 'm_30', 'm_35', 'm_40', 'm_45', 'm_50', 'm_55', 'm_60')])
AllDataClipped$OldPop <- rowSums(AllDataClipped[c('f_65', 'f_70', 'f_75', 'f_80', 'm_65', 'm_70', 'm_75', 'm_80')])
AllDataClipped$TotalPop <- rowSums(AllDataClipped[c('YoungPop', 'WorkingPop', 'OldPop')])
AllDataClipped$DependencyRatio = (AllDataClipped$YoungPop + AllDataClipped$OldPop) / AllDataClipped$WorkingPop

AllDataClipped <- AllDataClipped %>%
  rename(urbanid = UrbanID,
         city_name = Name,
         AnnualNO2 = NO2,
         AnnualPM25 = PM25,
         AnnualOzone = Ozone,
         HeatDays30 = heat30_days)

# convert WHO thresholds to correct units for comparison ####

PM_WHO_thresholds_ug <- c(0, 5, 10, 15, 25, 35, Inf)
PM_WHO_labels <- c("AQG", "Interim4", "Interim3", "Interim2", "Interim1", "Exceeding")

NO2_WHO_thresholds_ug <- c(0, 10, 20, 30, 40, Inf)
NO2_WHO_thresholds_ppbv <- NO2_WHO_thresholds_ug * 1/1.88
NO2_WHO_labels <- c("AQG", "Interim3", "Interim2", "Interim1", "Exceeding")

O3_WHO_thresholds_ug <- c(0, 60, 70, 100, Inf)
O3_WHO_thresholds_ppbv <- O3_WHO_thresholds_ug * 1/2
O3_WHO_labels <- c("AQG", "Interim2", "Interim1", "Exceeding")

Heat_thresholds <- c(0, 7, 14, 30, 60, 90, 120, Inf)
Heat_labels <- c("<7", "7-14", "14-30", "30-60", "60-90", "90-120", ">120")

CitySize_thresholds <- c(0, 50000, 100000, 300000, 500000, 1000000, 5000000, 10000000, Inf)
CitySize_labels <- c("<50k", "50-100k", "100-300k", "300-500k", "500k-1m", "1m-5m", "5m-10m", ">10m")

DependencyRatio_thresholds <- c(0, 0.3, 0.6, 0.9, 1.2, 1.5, Inf)
DependencyRatio_labels <- c("0-0.3", "0.3-0.6", "0.6-0.9", "0.9-1.2", "1.2-1.5", "1.5+")


# apply thresholds to AQ variables ####
#MeanDataClipped$PM25WHOCategory <- cut(MeanDataClipped$MeanPM25, breaks = PM_WHO_thresholds_ug, labels = PM_WHO_labels, include.lowest = TRUE)
AllDataClipped$PM25WHOCategory <- cut(AllDataClipped$AnnualPM25, breaks = PM_WHO_thresholds_ug, labels = PM_WHO_labels, include.lowest = TRUE)
#MeanDataClipped$PM25WHOCategory <- factor(MeanDataClipped$PM25WHOCategory, levels = PM_WHO_labels)
AllDataClipped$PM25WHOCategory <- factor(AllDataClipped$PM25WHOCategory, levels = PM_WHO_labels)

#MeanDataClipped$NO2WHOCategory <- cut(MeanDataClipped$MeanNO2, breaks = NO2_WHO_thresholds_ppbv, labels = NO2_WHO_labels, include.lowest = TRUE)
AllDataClipped$NO2WHOCategory <- cut(AllDataClipped$AnnualNO2, breaks = NO2_WHO_thresholds_ppbv, labels = NO2_WHO_labels, include.lowest = TRUE)
#MeanDataClipped$NO2WHOCategory <- factor(MeanDataClipped$NO2WHOCategory, levels = NO2_WHO_labels)
AllDataClipped$NO2WHOCategory <- factor(AllDataClipped$NO2WHOCategory, levels = NO2_WHO_labels)

#MeanDataClipped$OzoneWHOCategory <- cut(MeanDataClipped$MeanOzone, breaks = O3_WHO_thresholds_ppbv, labels = O3_WHO_labels, include.lowest = TRUE)
AllDataClipped$OzoneWHOCategory <- cut(AllDataClipped$AnnualOzone, breaks = O3_WHO_thresholds_ppbv, labels = O3_WHO_labels, include.lowest = TRUE)
#MeanDataClipped$OzoneWHOCategory <- factor(MeanDataClipped$OzoneWHOCategory, levels = O3_WHO_labels)
AllDataClipped$OzoneWHOCategory <- factor(AllDataClipped$OzoneWHOCategory, levels = O3_WHO_labels)

#MeanDataClipped$HeatCategory <- cut(MeanDataClipped$MeanHeatDays30, breaks = Heat_thresholds, labels = Heat_labels, include.lowest = TRUE)
AllDataClipped$HeatCategory <- cut(AllDataClipped$HeatDays30, breaks = Heat_thresholds, labels = Heat_labels, include.lowest = TRUE)
#MeanDataClipped$HeatCategory <- factor(MeanDataClipped$HeatCategory, levels = Heat_labels)
AllDataClipped$HeatCategory <- factor(AllDataClipped$HeatCategory, levels = Heat_labels)

#MeanDataClipped$CitySize <- cut(MeanDataClipped$TotalPop, breaks = CitySize_thresholds, labels = CitySize_labels, include.lowest = TRUE)
AllDataClipped$CitySize <- cut(AllDataClipped$TotalPop, breaks = CitySize_thresholds, labels = CitySize_labels, include.lowest = TRUE)
#MeanDataClipped$CitySize <- factor(MeanDataClipped$CitySize, levels = CitySize_labels)
AllDataClipped$CitySize <- factor(AllDataClipped$CitySize, levels = CitySize_labels)

#MeanDataClipped$DependencyRatioCategory <- cut(MeanDataClipped$DependencyRatio, breaks = DependencyRatio_thresholds, labels = DependencyRatio_labels, include.lowest = TRUE)
AllDataClipped$DependencyRatioCategory <- cut(AllDataClipped$DependencyRatio, breaks = DependencyRatio_thresholds, labels = DependencyRatio_labels, include.lowest = TRUE)
#MeanDataClipped$DependencyRatioCategory <- factor(MeanDataClipped$DependencyRatioCategory, levels = DependencyRatio_labels)
AllDataClipped$DependencyRatioCategory <- factor(AllDataClipped$DependencyRatioCategory, levels = DependencyRatio_labels)



df_2005 <- AllDataClipped %>% 
  filter(year == 2005) %>%
  select(urbanid, TotalPop) %>%
  rename(TotalPop2005 = TotalPop)  # Rename before merging

# Step 2: Join the 2005 TotalPop values back into the original DataFrame
AllDataClipped <- AllDataClipped %>%
  left_join(df_2005, by = "urbanid")

AllDataClipped2005 = AllDataClipped %>%
  filter(year == 2005)

AllDataClipped2020 = AllDataClipped %>%
  filter(year == 2020) %>%
  filter(HeatCategory %in% c("30-60", "60-90", "90-120", ">120") & 
           PM25WHOCategory %in% c("Interim4", "Interim3", "Interim2", "Interim1", "Exceeding") & 
           NO2WHOCategory %in% c("Interim3", "Interim2", "Interim1", "Exceeding") & 
           OzoneWHOCategory %in% c("Interim2", "Interim1", "Exceeding"))

sum(AllDataClipped2020$TotalPop)
sum(AllDataClipped2020$TotalPop2005)

urbanid_2020 <- unique(AllDataClipped2020$urbanid)

AllDataClipped2005_filtered <- AllDataClipped2005 %>%
  filter(urbanid %in% urbanid_2020)

AllDataMerged <- bind_rows(AllDataClipped2005_filtered, AllDataClipped2020)

ggplot(AllDataMerged, aes(x = as.factor(year), y = AnnualNO2)) +
  geom_boxplot()

ggplot(AllDataMerged, aes(x = as.factor(year), y = AnnualPM25)) +
  geom_boxplot()

ggplot(AllDataMerged, aes(x = as.factor(year), y = AnnualOzone)) +
  geom_boxplot()

ggplot(AllDataMerged, aes(x = as.factor(year), y = HeatDays30)) +
  geom_boxplot()



data_wide <- AllDataMerged %>%
  select(urbanid, year, AnnualNO2) %>%
  spread(key = year, value = AnnualNO2, sep = "_")

# Step 3: Calculate the difference in AnnualNO2 between 2020 and 2005
data_wide <- data_wide %>%
  mutate(NO2_diff = `year_2020` - `year_2005`)

# Step 4: Find how many urbanids had an increase in AnnualNO2 (i.e., NO2_diff > 0)
increased_NO2 <- data_wide %>%
  filter(NO2_diff > 0)

# Step 5: Calculate the percentage of urbanids with an increase
percentage_increase <- nrow(increased_NO2) / nrow(data_wide) * 100

# Output the result
percentage_increase

data_wide <- AllDataMerged %>%
  select(urbanid, year, AnnualPM25) %>%
  spread(key = year, value = AnnualPM25, sep = "_")

# Step 3: Calculate the difference in AnnualNO2 between 2020 and 2005
data_wide <- data_wide %>%
  mutate(NO2_diff = `year_2020` - `year_2005`)

# Step 4: Find how many urbanids had an increase in AnnualNO2 (i.e., NO2_diff > 0)
increased_NO2 <- data_wide %>%
  filter(NO2_diff > 0)

# Step 5: Calculate the percentage of urbanids with an increase
percentage_increase <- nrow(increased_NO2) / nrow(data_wide) * 100

# Output the result
percentage_increase



data_wide <- AllDataMerged %>%
  select(urbanid, year, AnnualOzone) %>%
  spread(key = year, value = AnnualOzone, sep = "_")

# Step 3: Calculate the difference in AnnualNO2 between 2020 and 2005
data_wide <- data_wide %>%
  mutate(NO2_diff = `year_2020` - `year_2005`)

# Step 4: Find how many urbanids had an increase in AnnualNO2 (i.e., NO2_diff > 0)
increased_NO2 <- data_wide %>%
  filter(NO2_diff > 0)

# Step 5: Calculate the percentage of urbanids with an increase
percentage_increase <- nrow(increased_NO2) / nrow(data_wide) * 100

# Output the result
percentage_increase


data_wide <- AllDataMerged %>%
  select(urbanid, year, HeatDays30) %>%
  spread(key = year, value = HeatDays30, sep = "_")

# Step 3: Calculate the difference in AnnualNO2 between 2020 and 2005
data_wide <- data_wide %>%
  mutate(NO2_diff = `year_2020` - `year_2005`)

# Step 4: Find how many urbanids had an increase in AnnualNO2 (i.e., NO2_diff > 0)
increased_NO2 <- data_wide %>%
  filter(NO2_diff > 0)

# Step 5: Calculate the percentage of urbanids with an increase
percentage_increase <- nrow(increased_NO2) / nrow(data_wide) * 100

# Output the result
percentage_increase








