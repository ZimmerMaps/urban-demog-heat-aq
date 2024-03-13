rm(list = ls())

# Load packages ####
library(tidyverse)
library(dplyr)

setwd("/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demography : AQ : Heat/")

urbanHeat = read.csv("data/heat/Heat30Estimated2020.csv") %>%
  select(ID_HDC_G0, year, tot_days) %>%
  dplyr::rename(UrbanID = ID_HDC_G0,
                heat30_days = tot_days)

# Load AQ UCDB data ####

UrbanNo2 = read.csv("/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demography : AQ : Heat/data/aq/no2/ucdb-no2-extracted.csv") %>%
  dplyr::rename(UrbanID = fid)

UrbanPM25 = read.csv("/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demography : AQ : Heat/data/aq/pm2.5/ucdb-pm25-extracted.csv") %>%
  dplyr::rename(UrbanID = fid)

UrbanOzone = read.csv('/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demography : AQ : Heat/data/aq/ozone/ozone_ucdb.csv') %>%
  dplyr::rename(UrbanID = ID, Ozone = layer)


# merge AQ and Heat together ####
urbanAQ = merge(UrbanNo2, UrbanPM25, by= c("UrbanID", "year"), all=T)
urbanAQ = merge(urbanAQ, UrbanOzone, by= c("UrbanID", "year"), all=T)

urbanAQHeat = merge(urbanAQ, urbanHeat, by = c("UrbanID", "year"), all = T)

# Load Demographic UCDB data ####

urbanDem = read.csv("/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demography : AQ : Heat/data/demographic/worldpop_agesex_urban_all.csv") %>%
  select(-X) %>%
  unite(sex_age, c("sex", "age")) %>%
  spread(sex_age, sum) %>%
  dplyr::rename(UrbanID = zone,
                Name = city_name)

urbanDem = dplyr::select(urbanDem, -'_NA')


# Merge Heat / AQ / Demographics together

mergedHeatAQDemog = merge(urbanAQHeat, urbanDem, by= c("UrbanID", "year"), all=T)


## Mean heat and mean aq for each city
MeanUrbanAQHeat = mergedHeatAQDemog %>%
  group_by(UrbanID) %>%
  dplyr::summarise(MeanNO2 = mean(NO2,  na.rm = T),
                   MeanPM25 = mean(PM25,  na.rm = T),
                   MeanOzone = mean(Ozone, na.rm = T),
                   MeanHeat30 = mean(heat30_days, na.rm = T))


UrbanDemog2020 = urbanDem %>%
  filter(year == 2020)

MeanUrbanAQHeatDemog = merge(MeanUrbanAQHeat, UrbanDemog2020, by = c("UrbanID"))

# adding variables
MeanUrbanAQHeatDemog$YoungPop <- rowSums(MeanUrbanAQHeatDemog[c('f_0', 'm_0', 'f_1', 'm_1', 'f_5', 'm_5', 'f_10', 'm_10')])
MeanUrbanAQHeatDemog$WorkingPop <- rowSums(MeanUrbanAQHeatDemog[c('f_15', 'f_20', 'f_25', 'f_30', 'f_35', 'f_40', 'f_45', 'f_50', 'f_55', 'f_60', 'm_15', 'm_20', 'm_25', 'm_30', 'm_35', 'm_40', 'm_45', 'm_50', 'm_55', 'm_60')])
MeanUrbanAQHeatDemog$OldPop <- rowSums(MeanUrbanAQHeatDemog[c('f_65', 'f_70', 'f_75', 'f_80', 'm_65', 'm_70', 'm_75', 'm_80')])
MeanUrbanAQHeatDemog$TotalPop <- rowSums(MeanUrbanAQHeatDemog[c('YoungPop', 'WorkingPop', 'OldPop')])
MeanUrbanAQHeatDemog$DependencyRatio = (MeanUrbanAQHeatDemog$YoungPop + MeanUrbanAQHeatDemog$OldPop) / MeanUrbanAQHeatDemog$WorkingPop

MergedMonthlyDemHeatAQSummary = dplyr::select(MeanUrbanAQHeatDemog, UrbanID, Name, country_iso, country_name, continent_name, latitude, longitude, YoungPop, WorkingPop, OldPop, TotalPop, DependencyRatio, MeanNO2, MeanPM25, MeanHeat30)

# save file
write.csv(MeanUrbanAQHeatDemog, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demography : AQ : Heat/data/merged/Final Merged Data/UCDB-Dem-Heat-AQ-Mean.csv")
write.csv(mergedHeatAQDemog, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demography : AQ : Heat/data/merged/Final Merged Data/UCDB-Dem-Heat-AQ-All.csv")

