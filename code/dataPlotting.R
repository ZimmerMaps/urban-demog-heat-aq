# Urban Demography / Heat / AQ - Plotting

rm(list = ls())

# load packages and set working directory ####
library(tidyverse)
library(rnaturalearth)
library(scales)
library(lemon)

setwd("/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demography : AQ : Heat/")

# load data ####
MeanData = read.csv("data/merged/Final Merged Data/UCDB-Dem-Heat-AQ-Mean.csv")
AllData = read.csv("data/merged/Final Merged Data/UCDB-Dem-Heat-AQ-All.csv")

# Add demographic variables ####
MeanData$YoungPop <- rowSums(MeanData[c('f_0', 'm_0', 'f_1', 'm_1', 'f_5', 'm_5', 'f_10', 'm_10')])
MeanData$WorkingPop <- rowSums(MeanData[c('f_15', 'f_20', 'f_25', 'f_30', 'f_35', 'f_40', 'f_45', 'f_50', 'f_55', 'f_60', 'm_15', 'm_20', 'm_25', 'm_30', 'm_35', 'm_40', 'm_45', 'm_50', 'm_55', 'm_60')])
MeanData$OldPop <- rowSums(MeanData[c('f_65', 'f_70', 'f_75', 'f_80', 'm_65', 'm_70', 'm_75', 'm_80')])
MeanData$TotalPop <- rowSums(MeanData[c('YoungPop', 'WorkingPop', 'OldPop')])
MeanData$DependencyRatio = (MeanData$YoungPop + MeanData$OldPop) / MeanData$WorkingPop

MeanDataClipped = dplyr::select(MeanData, UrbanID, year, Name, country_iso, country_name, continent_name, latitude, longitude, YoungPop, WorkingPop, OldPop, TotalPop, DependencyRatio, MeanNO2, MeanPM25, MeanOzone, MeanHeat30)
colnames(MeanDataClipped) = c('urbanid', 'year', 'city_name', 'country_iso', 'country_name', 'continent_name', 'latitude', 'longitude', 'YoungPop', 'WorkingPop', 'OldPop', 'TotalPop', 'DependencyRatio', 'MeanNO2', 'MeanPM25', 'MeanOzone', 'MeanHeatDays30')

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
MeanDataClipped$PM25WHOCategory <- cut(MeanDataClipped$MeanPM25, breaks = PM_WHO_thresholds_ug, labels = PM_WHO_labels, include.lowest = TRUE)
AllDataClipped$PM25WHOCategory <- cut(AllDataClipped$AnnualPM25, breaks = PM_WHO_thresholds_ug, labels = PM_WHO_labels, include.lowest = TRUE)
MeanDataClipped$PM25WHOCategory <- factor(MeanDataClipped$PM25WHOCategory, levels = PM_WHO_labels)
AllDataClipped$PM25WHOCategory <- factor(AllDataClipped$PM25WHOCategory, levels = PM_WHO_labels)

MeanDataClipped$NO2WHOCategory <- cut(MeanDataClipped$MeanNO2, breaks = NO2_WHO_thresholds_ppbv, labels = NO2_WHO_labels, include.lowest = TRUE)
AllDataClipped$NO2WHOCategory <- cut(AllDataClipped$AnnualNO2, breaks = NO2_WHO_thresholds_ppbv, labels = NO2_WHO_labels, include.lowest = TRUE)
MeanDataClipped$NO2WHOCategory <- factor(MeanDataClipped$NO2WHOCategory, levels = NO2_WHO_labels)
AllDataClipped$NO2WHOCategory <- factor(AllDataClipped$NO2WHOCategory, levels = NO2_WHO_labels)

MeanDataClipped$OzoneWHOCategory <- cut(MeanDataClipped$MeanOzone, breaks = O3_WHO_thresholds_ppbv, labels = O3_WHO_labels, include.lowest = TRUE)
AllDataClipped$OzoneWHOCategory <- cut(AllDataClipped$AnnualOzone, breaks = O3_WHO_thresholds_ppbv, labels = O3_WHO_labels, include.lowest = TRUE)
MeanDataClipped$OzoneWHOCategory <- factor(MeanDataClipped$OzoneWHOCategory, levels = O3_WHO_labels)
AllDataClipped$OzoneWHOCategory <- factor(AllDataClipped$OzoneWHOCategory, levels = O3_WHO_labels)

MeanDataClipped$HeatCategory <- cut(MeanDataClipped$MeanHeatDays30, breaks = Heat_thresholds, labels = Heat_labels, include.lowest = TRUE)
AllDataClipped$HeatCategory <- cut(AllDataClipped$HeatDays30, breaks = Heat_thresholds, labels = Heat_labels, include.lowest = TRUE)
MeanDataClipped$HeatCategory <- factor(MeanDataClipped$HeatCategory, levels = Heat_labels)
AllDataClipped$HeatCategory <- factor(AllDataClipped$HeatCategory, levels = Heat_labels)

MeanDataClipped$CitySize <- cut(MeanDataClipped$TotalPop, breaks = CitySize_thresholds, labels = CitySize_labels, include.lowest = TRUE)
AllDataClipped$CitySize <- cut(AllDataClipped$TotalPop, breaks = CitySize_thresholds, labels = CitySize_labels, include.lowest = TRUE)
MeanDataClipped$CitySize <- factor(MeanDataClipped$CitySize, levels = CitySize_labels)
AllDataClipped$CitySize <- factor(AllDataClipped$CitySize, levels = CitySize_labels)

MeanDataClipped$DependencyRatioCategory <- cut(MeanDataClipped$DependencyRatio, breaks = DependencyRatio_thresholds, labels = DependencyRatio_labels, include.lowest = TRUE)
AllDataClipped$DependencyRatioCategory <- cut(AllDataClipped$DependencyRatio, breaks = DependencyRatio_thresholds, labels = DependencyRatio_labels, include.lowest = TRUE)
MeanDataClipped$DependencyRatioCategory <- factor(MeanDataClipped$DependencyRatioCategory, levels = DependencyRatio_labels)
AllDataClipped$DependencyRatioCategory <- factor(AllDataClipped$DependencyRatioCategory, levels = DependencyRatio_labels)



# Figure 1 - Maps of all variables as subplot ####
AllDataClipped2020 = AllDataClipped %>%
  filter(year == 2020)

# KEEP ONLY CITIES WITH ALL DATA POINTS
#AllDataClipped2019 <- AllDataClipped2019[complete.cases(AllDataClipped2019), ]

world <- ne_countries(scale = "medium", returnclass = "sf")

#arrange value plotting highest
PopMapData = AllDataClipped2020 %>%
  filter(!is.na(TotalPop)) %>%
  arrange(TotalPop)
  

PopMap <- ggplot() +
  geom_sf(data = world, fill = "grey75", color = 'black', linewidth = 0.1) +
  geom_point(data = PopMapData, aes(x = longitude, y = latitude, color = TotalPop), size = 0.3) +
  scale_color_viridis_c(limits = c(1, 46000000), option = 'viridis', 
                        breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000), 
                        trans = "log10") +
  scale_y_continuous(limits = c(-55, 90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Total Urban Population", color = "", subtitle = "2020") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

PopMap


DRMapData = AllDataClipped2020 %>%
  filter(!is.na(DependencyRatio)) %>%
  arrange(DependencyRatio)

DRMap = ggplot() +
  geom_sf(data = world, fill = "grey75", color = 'black', linewidth = 0.1) +
  geom_point(data = DRMapData, aes(x = longitude, y = latitude, color = DependencyRatio), size = 0.3) +
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
NOMapData = AllDataClipped2020 %>%
  filter(!is.na(AnnualNO2)) %>%
  arrange(AnnualNO2)

NO2Map = ggplot() +
  geom_sf(data = world, fill = "grey75", color = 'black', linewidth = 0.1) +
  geom_point(data = NOMapData, aes(x = longitude, y = latitude, color = AnnualNO2), size = 0.3) +
  scale_color_viridis_c(limits = c(0,25), option = 'plasma', breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = expression("Mean Annual NO"[2] ~ "(ppbv)"), color = "", subtitle = "2020") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

NO2Map


#arrange value plotting highest
PMMapData = AllDataClipped2020 %>%
  filter(!is.na(AnnualPM25)) %>%
  arrange(AnnualPM25)

PMMap = ggplot() +
  geom_sf(data = world, fill = "grey75", color = 'black', linewidth = 0.1) +
  geom_point(data = PMMapData, aes(x = longitude, y = latitude, color = AnnualPM25), size = 0.3) +
  scale_color_viridis_c(limits = c(0,150), option = 'plasma', breaks = seq(0, 150, 25)) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = expression("Mean Annual PM"[2.5] ~ "(µg/m"^3*")"), color = "", subtitle = "2020") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

PMMap

#arrange value plotting AllDataClipped2016
O3MapData = AllDataClipped2020 %>%
  filter(!is.na(AnnualOzone)) %>%
  arrange(AnnualOzone)

OzoneMap = ggplot() +
  geom_sf(data = world, fill = "grey75", color = 'black', linewidth = 0.1) +
  geom_point(data = O3MapData, aes(x = longitude, y = latitude, color = AnnualOzone), size = 0.3) +
  scale_color_viridis_c(limits = c(0,82), option = 'plasma', breaks = seq(0, 82, 20)) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = expression("Mean Annual Ozone" ~ "(ppbv)"), color = "", subtitle = "2020") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

OzoneMap

#arrange value plotting highest
HeatMapData = AllDataClipped2020 %>%
  filter(!is.na(HeatDays30)) %>%
  arrange(HeatDays30)

HeatMap = ggplot() +
  geom_sf(data = world, fill = "grey75", color = 'black', linewidth = 0.1) +
  geom_point(data = HeatMapData, aes(x = longitude, y = latitude, color = HeatDays30), size = 0.3) +
  scale_color_viridis_c(limits = c(0,260), option = 'rocket', begin = 0.2, breaks = seq(0,260,40)) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "# Days > 30°C WBGT", color = "", subtitle = "2020") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

HeatMap




ggpubr::ggarrange(PopMap, DRMap, NO2Map, PMMap, OzoneMap, HeatMap, nrow = 3, ncol = 2)




# Figure 1 - Nigeria Inset ####

AllDataClippedNigeria = AllDataClipped2020 %>%
  filter(country_name == "Nigeria")

nigeria <- ne_countries(country = "Nigeria", scale = "large", returnclass = "sf")

#arrange value plotting highest
AllDataClippedNigeria = AllDataClippedNigeria %>%
  arrange(DependencyRatio)

DRMapNigeria = ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_sf(data = nigeria, fill = "grey30", color = 'black', linewidth = 0.1) +
  geom_point(data = AllDataClippedNigeria, aes(x = longitude, y = latitude, color = DependencyRatio), size = 1.5) +
  scale_color_viridis_c(limits = c(0.5,1.2), option = 'viridis', breaks = seq(0.5, 1.2, 0.1)) +
  scale_y_continuous(limits = c(4, 14)) +
  scale_x_continuous(limits = c(3, 15)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Population Dependency Ratio", color = "", subtitle = "Nigeria | 1983 - 2016") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 10, 
                                 barheight = 1,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

DRMapNigeria

#arrange value plotting highest
AllDataClippedNigeria = AllDataClippedNigeria %>%
  arrange(AnnualNO2)

NO2MapNigeria = ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_sf(data = nigeria, fill = "grey30", color = 'black', linewidth = 0.1) +
  geom_point(data = AllDataClippedNigeria, aes(x = longitude, y = latitude, color = AnnualNO2), size = 1.5) +
  scale_color_viridis_c(limits = c(0,25), option = 'plasma', breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(4, 14)) +
  scale_x_continuous(limits = c(3, 15)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = expression("Mean Annual NO"[2]), color = "", subtitle = "Nigeria | 1983 - 2016") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 10, 
                                 barheight = 1,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

NO2MapNigeria

#arrange value plotting highest
AllDataClippedNigeria = AllDataClippedNigeria %>%
  arrange(AnnualPM25)

PMMapNigeria = ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_sf(data = nigeria, fill = "grey30", color = 'black', linewidth = 0.1) +
  geom_point(data = AllDataClippedNigeria, aes(x = longitude, y = latitude, color = AnnualPM25), size = 1.5) +
  scale_color_viridis_c(limits = c(0,150), option = 'plasma', breaks = seq(0, 150, 25)) +
  scale_y_continuous(limits = c(4, 14)) +
  scale_x_continuous(limits = c(3, 15)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = expression("Mean Annual PM"[2.5]), color = "", subtitle = "Nigeria | 1983 - 2016") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 10, 
                                 barheight = 1,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

PMMapNigeria

AllDataClippedNigeria = AllDataClippedNigeria %>%
  arrange(AnnualOzone)

OzoneMapNigeria = ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_sf(data = nigeria, fill = "grey30", color = 'black', linewidth = 0.1) +
  geom_point(data = AllDataClippedNigeria, aes(x = longitude, y = latitude, color = AnnualOzone), size = 1.5) +
  scale_color_viridis_c(limits = c(0,260), option = 'rocket', begin = 0.2, breaks = seq(0,260,40)) +
  scale_y_continuous(limits = c(4, 14)) +
  scale_x_continuous(limits = c(3, 15)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = expression("Mean Annual Ozone"), color = "", subtitle = "Nigeria | 1983 - 2016") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 10, 
                                 barheight = 1,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

OzoneMapNigeria



#arrange value plotting highest
AllDataClippedNigeria = AllDataClippedNigeria %>%
  arrange(HeatDays30)

HeatMapNigeria = ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_sf(data = nigeria, fill = "grey30", color = 'black', linewidth = 0.1) +
  geom_point(data = AllDataClippedNigeria, aes(x = longitude, y = latitude, color = HeatDays30), size = 1.5) +
  scale_color_viridis_c(limits = c(0,100), option = 'rocket', begin = 0.2, breaks = seq(0, 100, 20)) +
  scale_y_continuous(limits = c(4, 14)) +
  scale_x_continuous(limits = c(3, 15)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Mean # Days > 30°C WBGT", color = "", subtitle = "Nigeria | 1983 - 2016") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 10, 
                                 barheight = 1,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 

HeatMapNigeria

ggpubr::ggarrange(DRMapNigeria, NO2MapNigeria, PMMapNigeria, OzoneMapNigeria, HeatMapNigeria, nrow = 3, ncol = 2)




#PM25 
MeanDataClipped = MeanDataClipped %>%
  arrange(DependencyRatio)

PM_Heat_Box = ggplot(MeanDataClipped, aes(y = PM25WHOCategory, x = MeanHeatDays30)) +
  geom_jitter(width = 0.25, alpha = 0.5, size = 0.1, aes(color = DependencyRatio)) +
  geom_boxplot(fill = NA) +
  scale_color_viridis_c(limits = c(0,1.4), option = 'viridis', breaks = seq(0, 1.4, 0.2)) +
  theme_bw() +
  labs(title = "PM2.5") +
  facet_wrap(~continent_name)
PM_Heat_Box

#NO2 
NO2_Heat_Box = ggplot(MeanDataClipped, aes(y = NO2WHOCategory, x = MeanHeatDays30)) +
  geom_jitter(width = 0.25, alpha = 0.5, size = 0.1, aes(color = DependencyRatio)) +
  geom_boxplot(fill = NA) +
  scale_color_viridis_c(limits = c(0,1.4), option = 'viridis', breaks = seq(0, 1.4, 0.2)) +
  theme_bw() +
  labs(title = "NO2") +
  facet_wrap(~continent_name)
NO2_Heat_Box

#Ozone 
Ozone_Heat_Box = ggplot(MeanDataClipped, aes(y = OzoneWHOCategory, x = MeanHeatDays30)) +
  geom_jitter(width = 0.25, alpha = 0.5, size = 0.1, aes(color = DependencyRatio)) +
  geom_boxplot(fill = NA) +
  scale_color_viridis_c(limits = c(0,1.4), option = 'viridis', breaks = seq(0, 1.4, 0.2)) +
  theme_bw() +
  labs(title = "O3") +
  facet_wrap(~continent_name)
Ozone_Heat_Box

ggpubr::ggarrange(PM_Heat_Box, NO2_Heat_Box, Ozone_Heat_Box, nrow = 1, ncol = 3, common.legend = T, legend = "bottom")



# Figure 3 - Cumulative population exposed to Heat and AQ ####
color_palette <- c("#0c2c4d", "#1f78b4", "#92b3d3",
                   "#14522a", "#33a02c", "#7fcd8c",
                   "#8c0c0e", "#e31a1c", "#f59c99",
                   "#9c5100", "#ff7f00", "#ffb366",
                   "#402750", "#6a3d9a", "#b09abf",
                   "#5696bb", "#a6cee3", "#d5ebf7")

test = AllDataClipped2020 %>%
  group_by(PM25WHOCategory, continent_name) %>%
  summarise(YoungPopExposed = sum(YoungPop),
            WorkingPopExposed = sum(WorkingPop),
            OldPopExposed = sum(OldPop))

test_long <- gather(test, AgeCat, PopSum, YoungPopExposed:OldPopExposed, factor_key=TRUE)

test_long$PlotGroup <- paste(test_long$continent_name, "-", test_long$AgeCat)
test_long <- test_long[complete.cases(test_long), ]

PMPopulationPlot = ggplot(test_long, aes(y = PM25WHOCategory, x =PopSum, fill = PlotGroup)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_bw() +
  scale_x_continuous(labels = comma, limits = c(0,1800000000), breaks = seq(0, 1800000000, by = 200000000)) +
  scale_y_discrete(drop = FALSE) +
  scale_fill_manual(values = color_palette) +
  labs(x = expression("WHO Annual PM2.5 Threshold", y = "Total Population", fill = "Country & Age-Group"))
PMPopulationPlot

test = AllDataClipped2020 %>%
  group_by(NO2WHOCategory, continent_name) %>%
  summarise(YoungPopExposed = sum(YoungPop),
            WorkingPopExposed = sum(WorkingPop),
            OldPopExposed = sum(OldPop))

test_long <- gather(test, AgeCat, PopSum, YoungPopExposed:OldPopExposed, factor_key=TRUE)

test_long$PlotGroup <- paste(test_long$continent_name, "-", test_long$AgeCat)
test_long <- test_long[complete.cases(test_long), ]

NOPopulationPlot = ggplot(test_long, aes(y = NO2WHOCategory, x =PopSum, fill = PlotGroup)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_bw() +
  scale_x_continuous(labels = comma, limits = c(0,1800000000), breaks = seq(0, 1800000000, by = 200000000)) +
  scale_y_discrete(drop = FALSE) +
  scale_fill_manual(values = color_palette) +
  labs(x = expression("WHO Annual NO2 Threshold", y = "Total Population", fill = "Country & Age-Group"))
NOPopulationPlot


test = AllDataClipped2020 %>%
  group_by(OzoneWHOCategory, continent_name) %>%
  summarise(YoungPopExposed = sum(YoungPop),
            WorkingPopExposed = sum(WorkingPop),
            OldPopExposed = sum(OldPop))

test_long <- gather(test, AgeCat, PopSum, YoungPopExposed:OldPopExposed, factor_key=TRUE)

test_long$PlotGroup <- paste(test_long$continent_name, "-", test_long$AgeCat)
test_long <- test_long[complete.cases(test_long), ]

OzonePopulationPlot = ggplot(test_long, aes(y = OzoneWHOCategory, x =PopSum, fill = PlotGroup)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_bw() +
  scale_x_continuous(labels = comma, limits = c(0,1800000000), breaks = seq(0, 1800000000, by = 200000000)) +
  scale_y_discrete(drop = FALSE) +
  scale_fill_manual(values = color_palette) +
  labs(x = expression("WHO Annual Ozone Threshold", y = "Total Population", fill = "Country & Age-Group"))
OzonePopulationPlot

test = AllDataClipped2020 %>%
  group_by(HeatCategory, continent_name) %>%
  summarise(YoungPopExposed = sum(YoungPop),
            WorkingPopExposed = sum(WorkingPop),
            OldPopExposed = sum(OldPop))

test_long <- gather(test, AgeCat, PopSum, YoungPopExposed:OldPopExposed, factor_key=TRUE)

test_long$PlotGroup <- paste(test_long$continent_name, "-", test_long$AgeCat)
test_long <- test_long[complete.cases(test_long), ]

HeatPopulationPlot = ggplot(test_long, aes(y = HeatCategory, x =PopSum, fill = PlotGroup)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_bw() +
  scale_x_continuous(labels = comma, limits = c(0,1800000000), breaks = seq(0, 1800000000, by = 200000000)) +
  scale_y_discrete(drop = FALSE) +
  scale_fill_manual(values = color_palette) +
  labs(x = expression("Heat Threshold", y = "Total Population", fill = "Country & Age-Group")) 
HeatPopulationPlot

ggpubr::ggarrange(PMPopulationPlot, NOPopulationPlot, OzonePopulationPlot, HeatPopulationPlot, 
                  nrow = 2, ncol = 2, common.legend = T,
                  legend = "bottom")



## Figure 5 ####

Fig5Data = AllDataClipped2020 %>%
  filter(HeatDays30 > 30 & AnnualPM25 > 15 & AnnualOzone > 35  & AnnualNO2 > 15.96)
         
         
  
           AnnualOzone > 35 &
           AnnualPM25 > 25 & 
           AnnualNO2 > 15.96)

MapPlot = ggplot() +
  geom_sf(data = world, fill = "grey75", color = 'black', linewidth = 0.1) +
  geom_point(data = Fig5Data, aes(x = longitude, y = latitude), color = 'red', size = 0.3) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Cities Exposed to Extreme Heat & Poor AQ", color = "", 
       subtitle = "Heat30 > 30 Days &  AQ > Interim2") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 
MapPlot

Fig5Data = Fig5Data %>%
  select(continent_name, f_0, f_1, f_5, f_10, f_15, f_20, f_25, f_30, f_35, f_40, f_45, f_50, f_55, f_60, f_65, f_70, f_75, f_80,
         m_0, m_1, m_5, m_10, m_15, m_20, m_25, m_30, m_35, m_40, m_45, m_50, m_55, m_60, m_65, m_70, m_75, m_80)

Fig5Data$f_1 = Fig5Data$f_1 + Fig5Data$f_0
Fig5Data$m_1 = Fig5Data$m_1 + Fig5Data$m_0

Fig5Data = Fig5Data %>%
  select(-c (f_0, m_0))

Fig5Data <- Fig5Data %>%
  pivot_longer(
    cols = starts_with(c("f_", "m_")),
    names_to = c("sex", "age"),
    names_pattern = "([fm])_(\\d+)",
    values_to = "population") %>%
  group_by(continent_name, sex, age) %>%
  summarise(population = sum(population)) 

abs_comma <- function (x, ...) {
  format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

Fig5Data$age = as.factor(as.numeric(Fig5Data$age))

PyramidPlot = ggplot(data = Fig5Data, 
                     mapping = aes(x = ifelse(test = sex == "m", yes = -population, no = population), 
                                   y = age, fill = continent_name)) +
  geom_col(color = 'grey50', position = "stack") +
  scale_x_symmetric(labels = abs_comma, mid = 0) +
  labs(x = "Population", y = "", fill = "Continent", title = "") +
  theme_bw() +
  theme(legend.position = "bottom")

PyramidPlot

ggpubr::ggarrange(MapPlot, PyramidPlot, nrow = 1)


AllDataClippedComplete = AllDataClipped 
AllDataClippedComplete <- AllDataClippedComplete[complete.cases(AllDataClippedComplete), ]

ggplot(AllDataClippedComplete, aes(x = year, y = TotalPop)) +
  geom_bar(stat = "identity")

Fig4Data = AllDataClippedComplete %>%
  filter(HeatDays30 > 30 & AnnualPM25 > 5 & AnnualOzone > 30  & AnnualNO2 > 5.32)

Fig4Data  = Fig4Data %>%
  group_by(year) %>%
  summarise(YoungExposed = sum(YoungPop),
            WorkingExposed = sum(WorkingPop),
            OldExposed = sum(OldPop))

Fig4DataLong <- gather(Fig4Data, AgeCat, PopSum, YoungExposed:OldExposed, factor_key=TRUE)
Fig4DataLong$year = as.factor(Fig4DataLong$year)

ggplot(Fig4DataLong, aes(x = year, PopSum, fill = AgeCat)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma) +
  theme_bw() +
  labs(x = "", y = "Population", 
       title = "Heat30 > 30 days & PM2.5, O3 and NO2 > Interim2",
       fill = "Category") +
  theme(legend.position = c(0.2, 0.8))


### Calculate contribution by each variable ####
ExposedPop2005 = AllDataClipped %>%
  filter(year == 2005) %>%
  summarize(SumYoungPop = sum(YoungPop, na.rm = T),
            SumWorkingPop = sum(WorkingPop, na.rm = T),
            SumOldPop = sum(OldPop, na.rm = T))

ExposedCities2005 <- AllDataClipped %>%
  filter(year == 2005) %>%
  mutate(
    HeatExceed = if_else(!is.na(HeatDays30) & HeatDays30 > 30, 1, 0),
    OzoneExceed = if_else(!is.na(AnnualOzone) & AnnualOzone > 50, 1, 0),
    PM25Exceed = if_else(!is.na(AnnualPM25) & AnnualPM25 > 25, 1, 0),
    NO2Exceed = if_else(!is.na(AnnualNO2) & AnnualNO2 > 21.28, 1, 0),
    AllExceed = if_else(HeatExceed & OzoneExceed & PM25Exceed & NO2Exceed, 1, 0)
  ) %>%
  summarise(
    CountHeat = sum(HeatExceed),
    CountOzone = sum(OzoneExceed),
    CountPM25 = sum(PM25Exceed),
    CountNO2 = sum(NO2Exceed),
    CountAll = sum(AllExceed)
  )
ExposedCities2005$year = 2005

ExposedPop2005 = cbind(ExposedPop2005, ExposedCities2005)


ExposedPop2020 = AllDataClipped %>%
  filter(year == 2020) %>%
  summarize(SumYoungPop = sum(YoungPop, na.rm = T),
            SumWorkingPop = sum(WorkingPop, na.rm = T),
            SumOldPop = sum(OldPop, na.rm = T))


ExposedCities2020 <- AllDataClipped %>%
  filter(year == 2020) %>%
  mutate(
    HeatExceed = if_else(!is.na(HeatDays30) & HeatDays30 > 30, 1, 0),
    OzoneExceed = if_else(!is.na(AnnualOzone) & AnnualOzone > 50, 1, 0),
    PM25Exceed = if_else(!is.na(AnnualPM25) & AnnualPM25 > 25, 1, 0),
    NO2Exceed = if_else(!is.na(AnnualNO2) & AnnualNO2 > 21.28, 1, 0),
    AllExceed = if_else(HeatExceed & OzoneExceed & PM25Exceed & NO2Exceed, 1, 0)
  ) %>%
  summarise(
    CountHeat = sum(HeatExceed),
    CountOzone = sum(OzoneExceed),
    CountPM25 = sum(PM25Exceed),
    CountNO2 = sum(NO2Exceed),
    CountAll = sum(AllExceed)
  )
ExposedCities2020$year = 2020

ExposedPop2020 = cbind(ExposedPop2020, ExposedCities2020)

ExposedPopChange = rbind(ExposedPop2005, ExposedPop2020)

# count number of cities that exceed all thresholds together

# population exposed if 2005 amounts were applied to 2020


# make a binary column for the cities that would be included in the thresholds in 2005
# make a binary column for the cities that exceeded the thresholds in 2020

# sum the population of those = 1 in 2005
# sum the population of those = 1 in 2020


## Calculate contribution to increased exposure ####
UsefulData = AllDataClipped %>%
  select(urbanid, year, AnnualNO2, AnnualPM25, AnnualOzone, HeatDays30, TotalPop)
UsefulData <- UsefulData[complete.cases(UsefulData), ]


data = UsefulData

# Ensure 'year' column is numeric
data$year <- as.numeric(data$year)

# Filter data for years 2005 and 2020
data <- data[data$year %in% c(2005, 2020), ]

# Initialize an empty dataframe to store results
predicted_values <- data.frame(matrix(NA, nrow = 0, ncol = 2 + 4 * 2))

# Perform linear regression for each variable\
# Perform linear regression for each variable
variables <- c("AnnualPM25", "AnnualNO2", "AnnualOzone", "HeatDays30", "TotalPop")
for (var in variables) {
  # Fit linear regression model
  lm_model <- lm(data[, var] ~ year, data = data)
  
  # Predict values for 2005 and 2020
  pred_2005 <- predict(lm_model, newdata = data.frame(year = 2005))
  pred_2020 <- predict(lm_model, newdata = data.frame(year = 2020))
  
  # Append results to the dataframe
  predicted_values <- rbind(predicted_values, cbind(data[, c("urbanid", "year")], var, pred_2005, pred_2020))
}

# Add column names
colnames(predicted_values) <- c("urbanid", "year", "variable", "pred_2005", "pred_2020")



































UsefulData$NO2Exposure = UsefulData$AnnualNO2 * UsefulData$TotalPop
UsefulData$PM25Exposure = UsefulData$AnnualPM25 * UsefulData$TotalPop
UsefulData$OzoneExposure = UsefulData$AnnualOzone * UsefulData$TotalPop
UsefulData$HeatExposure = UsefulData$HeatDays30 * UsefulData$TotalPop

UsefulData$TotalExposure = UsefulData$NO2Exposure + UsefulData$PM25Exposure + UsefulData$OzoneExposure + UsefulData$HeatExposure
UsefulData$TotalValues = UsefulData$AnnualNO2 + UsefulData$AnnualPM25 + UsefulData$AnnualOzone + UsefulData$HeatDays30 + UsefulData$TotalPop

# Now, let's create an empty dataframe to store the coefficients
coefficients_df_heat <- data.frame(urbanid = character(),
                              coefficient = numeric(),
                              stringsAsFactors = FALSE)

# Get unique urbanids
urbanids <- unique(UsefulData$urbanid)

# Loop through each urbanid
for (id in urbanids) {
  # Subset data for each urbanid
  subset_data <- UsefulData[UsefulData$urbanid == id, ]
  
  # Fit linear regression model
  lm_model <- lm(HeatDays30 ~ year, data = subset_data)
  
  # Extract coefficients
  coef_value <- coef(lm_model)[2]  # Extracting the coefficient for TotalExposure
  
  # Store urbanid and coefficient in the dataframe
  coefficients_df_heat <- rbind(coefficients_df_heat, data.frame(urbanid = id,
                                                       coefficient = coef_value))
}

colnames(coefficients_df) <- c("urbanid", "TotalExposure_Coef")
colnames(coefficients_df_no2) <- c("urbanid", "NO2_Coef")
colnames(coefficients_df_pm25) <- c("urbanid", "PM_Coef")
colnames(coefficients_df_o3) <- c("urbanid", "Ozone_Coef")
colnames(coefficients_df_heat) <- c("urbanid", "Heat_Coef")
colnames(coefficients_df_pop) <- c("urbanid", "Pop_Coef")
colnames(coefficients_df_values) <- c("urbanid", "Values_Coef")

# Merge data frames
merged_df <- merge(merge(merge(merge(merge(merge(coefficients_df, coefficients_df_no2, by="urbanid", all=TRUE), coefficients_df_pm25, by="urbanid", all=TRUE), coefficients_df_o3, by="urbanid", all=TRUE), coefficients_df_heat, by="urbanid", all=TRUE), coefficients_df_pop, by="urbanid", all=TRUE), coefficients_df_values, by="urbanid", all=TRUE)

merged_df$Check <- rowSums(merged_df[, 3:7])

merged_df$no2_percent = merged_df$NO2_Coef / merged_df$Values_Coef * 100
merged_df$pm25_percent = merged_df$PM_Coef / merged_df$Values_Coef * 100
merged_df$o3_percent = merged_df$Ozone_Coef / merged_df$Values_Coef * 100
merged_df$heat_percent = merged_df$Heat_Coef / merged_df$Values_Coef * 100
merged_df$pop_percent = merged_df$Pop_Coef / merged_df$Values_Coef * 100

merged_df$TotalPercent = merged_df$no2_percent + merged_df$pm25_percent + merged_df$o3_percent + merged_df$heat_percent + merged_df$pop_percent

merged_df_filtered = merged_df %>%
  filter(Values_Coef > 0)

DelhiData = merged_df_filtered %>%
  filter(urbanid == 9770) %>%
  select(no2_percent, pm25_percent, o3_percent, heat_percent, pop_percent) %>% 
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "value")

ggplot(DelhiData, aes(x = variable, y = value)) +
  geom_bar(stat = "identity", position = "dodge")


ggplot(data = merged_df_filtered) +
  geom_density(aes(x = no2_percent), color = 'red') + 
  geom_density(aes(x = pm25_percent), color = 'blue') + 
  geom_density(aes(x = o3_percent), color = 'green') + 
  geom_density(aes(x = heat_percent), color = 'orange') +
  geom_density(aes(x = pop_percent), color = 'purple') + 
  scale_x_continuous(limits = c(0,101)) +
  theme_bw()









