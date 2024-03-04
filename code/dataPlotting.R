# Urban Demography / Heat / AQ - Plotting

rm(list = ls())


# load packages and set working directory ####
library(tidyverse)

setwd("/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demography : AQ : Heat/")

# load data ####
MeanData = read.csv("data/merged/Final Merged Data/UCDB-Dem-Heat-AQ-Mean.csv")
AllData = read.csv("data/merged/Final Merged Data/UCDB-Dem-Heat-AQ-All.csv")


# Add demographic variables
MeanData$YoungPop <- rowSums(MeanData[c('f_0', 'm_0', 'f_1', 'm_1', 'f_5', 'm_5', 'f_10', 'm_10')])
MeanData$WorkingPop <- rowSums(MeanData[c('f_15', 'f_20', 'f_25', 'f_30', 'f_35', 'f_40', 'f_45', 'f_50', 'f_55', 'f_60', 'm_15', 'm_20', 'm_25', 'm_30', 'm_35', 'm_40', 'm_45', 'm_50', 'm_55', 'm_60')])
MeanData$OldPop <- rowSums(MeanData[c('f_65', 'f_70', 'f_75', 'f_80', 'm_65', 'm_70', 'm_75', 'm_80')])
MeanData$TotalPop <- rowSums(MeanData[c('YoungPop', 'WorkingPop', 'OldPop')])
MeanData$DependencyRatio = (MeanData$YoungPop + MeanData$OldPop) / MeanData$WorkingPop

MeanDataClipped = dplyr::select(MeanData, UrbanID, year, Name, country_iso, country_name, continent_name, latitude, longitude, YoungPop, WorkingPop, OldPop, TotalPop, DependencyRatio, MeanNO2, MeanPM25, MeanOzone, MeanHeat30)
colnames(MeanDataClipped) = c('urbanid', 'year', 'city_name', 'country_iso', 'country_name', 'continent_name', 'latitude', 'longitude', 'YoungPop', 'WorkingPop', 'OldPop', 'TotalPop', 'DependencyRatio', 'MeanNO2', 'MeanPM25', 'MeanOzone', 'MeanHeatDays30')

# keep only complete cases
MeanDataClipped <- MeanDataClipped[complete.cases(MeanDataClipped), ]


# Convert NO2 from ppbv to ug/m3
# Conversion constants for NO2
molecular_weight_no2 <- 46.0055  # g/mol
conversion_factor_no2 <- 1.882

# Perform the conversion for the entire column
MeanDataClipped$MeanNO2UG <- MeanDataClipped$MeanNO2 * molecular_weight_no2 * conversion_factor_no2 / 24.45


# bin data based on exceedance of WHO thresholds
breaks <- c(0, 5, 10, 15, 25, 35, Inf)
labels <- c("AQG", "Interim4", "Interim3", "Interim2", "Interim1", "Exceeding")

# Create a new column with the categories
MeanDataClipped$PM25WHOCategory <- cut(MeanDataClipped$MeanPM25, breaks = breaks, labels = labels, include.lowest = TRUE)
MeanDataClipped$PM25WHOCategory <- factor(MeanDataClipped$PM25WHOCategory, levels = labels)

breaks <- c(0, 10, 20, 30, 40, Inf)
labels <- c("AQG", "Interim3", "Interim2", "Interim1", "Exceeding")

# Create a new column with the categories
MeanDataClipped$NO2WHOCategory <- cut(MeanDataClipped$MeanNO2UG, breaks = breaks, labels = labels, include.lowest = TRUE)
MeanDataClipped$NO2WHOCategory <- factor(MeanDataClipped$NO2WHOCategory, levels = labels)

breaks <- c(0, 60, 70, 100, Inf)
labels <- c("AQG", "Interim2", "Interim1", "Exceeding")

# Create a new column with the categories
MeanDataClipped$OzoneWHOCategory <- cut(MeanDataClipped$MeanOzone, breaks = breaks, labels = labels, include.lowest = TRUE)
MeanDataClipped$OzoneWHOCategory <- factor(MeanDataClipped$OzoneWHOCategory, levels = labels)


breaks <- c(0, 50000, 100000, 300000, 500000, 1000000, 5000000, 10000000, Inf)
labels <- c("<50k", "50-100k", "100-300k", "300-500k", "500k-1m", "1m-5m", "5m-10m", ">10m")

# Create a new column with the categories
MeanDataClipped$city_size <- cut(MeanDataClipped$TotalPop, breaks = breaks, labels = labels, include.lowest = TRUE)
MeanDataClipped$city_size <- factor(MeanDataClipped$city_size, levels = labels)

breaks <- c(0, 7, 14, 30, 60, 90, 120, Inf)
labels <- c("<7", "7-14", "14-30", "30-60", "60-90", "90-120", ">120")

# Create a new column with the categories
MeanDataClipped$HeatCategory <- cut(MeanDataClipped$MeanHeatDays30, breaks = breaks, labels = labels, include.lowest = TRUE)
MeanDataClipped$HeatCategory <- factor(MeanDataClipped$HeatCategory, levels = labels)





# Figure 1 - Maps of all variables as subplot ####
world <- ne_countries(scale = "medium", returnclass = "sf")

#arrange value plotting highest
MeanDataClipped = MeanDataClipped %>%
  arrange(DependencyRatio)

DRMap = ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = MeanDataClipped, aes(x = longitude, y = latitude, color = DependencyRatio), size = 0.3) +
  scale_color_viridis_c(limits = c(0,1.4), option = 'viridis', breaks = seq(0, 1.4, 0.2)) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Population Dependency Ratio", color = "", subtitle = "2020") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

DRMap

#arrange value plotting highest
MeanDataClipped = MeanDataClipped %>%
  arrange(MeanNO2UG)

NO2Map = ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = MeanDataClipped, aes(x = longitude, y = latitude, color = MeanNO2UG), size = 0.3) +
  scale_color_viridis_c(limits = c(0,80), option = 'plasma', breaks = seq(0, 80, 10)) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = expression("Mean Annual NO"[2] ~ "(µg/m"^3*")"), color = "", subtitle = "2006-2020") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

NO2Map


#arrange value plotting highest
MeanDataClipped = MeanDataClipped %>%
  arrange(MeanPM25)

PMMap = ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = MeanDataClipped, aes(x = longitude, y = latitude, color = MeanPM25), size = 0.3) +
  scale_color_viridis_c(limits = c(0,120), option = 'plasma', breaks = seq(0, 120, 20)) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = expression("Mean Annual PM"[2.5] ~ "(µg/m"^3*")"), color = "", subtitle = "1998 - 2021") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

PMMap

#arrange value plotting highest
MeanDataClipped = MeanDataClipped %>%
  arrange(MeanOzone)

OzoneMap = ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = MeanDataClipped, aes(x = longitude, y = latitude, color = MeanOzone), size = 0.3) +
  scale_color_viridis_c(limits = c(0,70), option = 'plasma', breaks = seq(0, 70, 10)) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = expression("Mean Annual Ozone" ~ "(µg/m"^3*")"), color = "", subtitle = "1998 - 2021") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

OzoneMap

#arrange value plotting highest
MeanDataClipped = MeanDataClipped %>%
  arrange(MeanHeatDays30)

HeatMap = ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = MeanDataClipped, aes(x = longitude, y = latitude, color = MeanHeatDays30), size = 0.3) +
  scale_color_viridis_c(limits = c(0,240), option = 'rocket', begin = 0.2, breaks = seq(0, 240, 30)) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Mean # Days > 30°C WBGT", color = "", subtitle = "1983 - 2016") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

HeatMap




ggpubr::ggarrange(DRMap, NO2Map, PMMap, OzoneMap, HeatMap, nrow = 3, ncol = 2)




