# This script contains basic calculations of urban growth and building construction from World Settlement Footprint.
# See end of script for variable codebook.


# ------------- PREPARE WORKSPACE -------------
setwd("C:/Users/grace/GIS/povertyequity/urban_growth")
wd <- getwd()
library(ursa)
library(raster)
library(sp)
library(rgdal)
library(sf)
library(dplyr)
library(rgeos)
library(naniar) # replace_with_na()
library(zoo) # rollsum()
library(tidyverse) # ggplot2()


# ------------- LOAD DATA ----------------

# --- 1. World Settlement Footprint Evolution ---
# Data for eight African cities. (Ouaga & Bangui to be added)
# Yearly built up area extents from 1985 - 2015
# Each year feature only contains the *additional* built up area from the year before, e.g. 1999 is *exclusive* of all 1998's existing area.
# Each city location has been clipped to a 20km radius buffer from the GeoNames central point.
# N'Djamena and Brazzaville buffer circles were reduced to exclude areas overlapping into neighboring country (Kousseri & Kinshasa).
# Added the years where there was no WSF-E layer manually in the csv version. 
casestudies <- read.csv('WSFE_casestudycities_yearsfilled.csv')

# Clean up
casestudies <- rename(casestudies, c("year"="gridcode"))
casestudies$year <- as.numeric(as.character(casestudies$year))

# Convert square meters to square kilometers (less unwieldy)
casestudies <- casestudies %>% mutate(Shape_Area = Shape_Area / 1000000)

# Create two different files, one spatial and one not, so that we can work with a smaller df and rejoin by ID later.
casestudies_sf <- readOGR(paste0(wd, '/WSFE_casestudycities.shp'))
casestudies_sf <- st_as_sf(casestudies_sf)
casestudies_sf <- casestudies_sf %>% mutate(Shape_Area = Shape_Area / 1000000)
casestudies_sf <- rename(casestudies_sf, c("year"="gridcode"))
casestudies_sf$year <- as.numeric(as.character(casestudies_sf$year))
casestudies_sf <- select(casestudies_sf, -Shape_Leng)

# Old code from before manually working with csv:
# casestudies$caseID <- 1:nrow(casestudies)
# casestudies_sf <- casestudies[ , c('caseID', 'geometry')]
# casestudies <- casestudies %>% st_drop_geometry()


# --- 2. Machine Learning-generated classification of WSF buildings ---
# For select African cities. 
# Classifications differ based on each team (each city). 
# Time is constant: 2015.
# Three cities currently ready for initial comparison to WSF-E growth analysis:
# Bamako: high income, middle income, informal
# N'Djamena: high income, middle income, informal, and commercial
# Niamey: middle income, informal, formal, commercial
bamako_ml <- readOGR(paste0(wd, '/Bamako_ML_prediction_V1/prediction_result.shp')) # Error. Need missing .shp from Eigo.
bamako_ml <- st_as_sf(bamako_ml)
ndjamena_ml <- readOGR(paste0(wd, '/NDjamena_ML_prediction_V2/prediction_result.shp'))
ndjamena_ml <- st_as_sf(ndjamena_ml)
niamey_ml <- readOGR(paste0(wd, '/Niamey_ML_prediction_V1/prediction_result.shp'))
niamey_ml <- st_as_sf(niamey_ml)


# ------------- COMBINE URBAN GROWTH WITH BUILDING CONSTRUCTION CLASSES ----------------

# --- Ensure same CRS and projected ---
st_crs(casestudies_sf)
st_crs(niamey_ml)
niamey_ml <- st_transform(niamey_ml, crs = st_crs(casestudies_sf))


# --- Spatial join ---
niamey_ml <- st_join(niamey_ml, casestudies_sf)

# Predict variable:
## 1 Middle income
## 2 Informal
## 3 Formal
## 4 Commercial



# ------------- SUMMARY STATISTICS, GROWTH ----------------

# --- Total square area of the city. ---
# Creating a cumulative area field for area as of that year, and one for 2015 for ease of calculation farther down.
# Note: Shape area variable is now in SQUARE KILOMETERS, not meters.
area_df <- aggregate(casestudies$Shape_Area, list(casestudies$city_name), FUN=sum, na.rm=TRUE, na.action=NULL)
area_df <- rename(area_df, c("city_name"="Group.1", "totArea"="x")) # Calling it "tot" for "total" instead of "2015" in case any WSF-E rasters aren't available for 2015.
casestudies <- merge(x=casestudies, y=area_df, by='city_name', all.x=TRUE)

casestudies$cuArea1y <- 1
casestudies = casestudies %>% group_by(city_name) %>% mutate(cuArea1y = cumsum(Shape_Area))

# --- Proportion of the city built in each year. (1985 is treated as 'Before 1986') ---
casestudies$prop1y <- 1
casestudies$prop1y <- casestudies$Shape_Area / casestudies$totArea * 100

# --- Proportion of the city built in the last 5 years. ---
# Also including a field for cumulative area from the last 5 years.
casestudies$cuArea5y <- rollsumr(casestudies$Shape_Area, k = 5, fill = NA)
casestudies$prop5y <- 1
casestudies$prop5y <- casestudies$cuArea5y / casestudies$totArea * 100


# --- Percent increase in built area compared to previous year. ---
casestudies$pcGRO_1y <- 1
casestudies <- casestudies %>% 
  arrange(city_name) %>% 
  mutate(diff_year = year - lag(year),
         diff_area = cuArea - lag(cuArea),
         pcGRO_1y = (diff_area / diff_year) / lag(cuArea) * 100)


# --- Percent increase in built area compared to 5 years prior. ---
casestudies$pcGRO_5y <- 1
casestudies <- casestudies %>% 
  arrange(city_name) %>% 
  mutate(diff_year = year - lag(year, 5),
         diff_area = cuArea - lag(cuArea, 5),
         pcGRO_5y = ifelse(diff_year==5, (cuArea - lag(cuArea, 5)) / lag(cuArea, 5) * 100, NA))


# --- Percent increase in built area compared to 10 years prior. ---
casestudies$pcGRO_10y <- 1
casestudies <- casestudies %>% 
  arrange(city_name) %>% 
  mutate(diff_year = year - lag(year, 10),
         diff_area = cuArea - lag(cuArea, 10),
         pcGRO_10y = ifelse(diff_year==10, (cuArea - lag(cuArea, 10)) / lag(cuArea, 10) * 100, NA))


# --- Subset of 5 year increments ---
casestudies_5y <- casestudies %>% 
  filter_at(vars(year), 
            any_vars(. %in% c('1985', '1990', '1995', '2000', '2005', '2010', '2015'))) 



# ------------- SUMMARY STATISTICS, CONSTRUCTION & GROWTH ----------------

# --- Proportion of total area of each building class, all years. ---
class_year <- aggregate(niamey_ml$predict, list(niamey_ml$year), FUN=sum, na.rm=TRUE, na.action=NULL)
class_year <- rename(base_iso, c("ADM1_PCODE"="Group.1", "postiso"="x"))


# --- Year that development (of any construction or income class) began for each cell. ---
# Note: Building class is likely to have evolved between years (upgrading, destruction, etc.).
  # This measure is only intended as an initial exploration as to the relationship between neighborhood age and construction.
  # It does not imply that this construction type started in this year.



# ------------- VISUALIZATION ----------------
# Color blind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# --- Chart: Visualizing spikes in construction ---
# Additional construction area from previous year, yearly
km2_plot <- casestudies %>%
  ggplot(aes(x = year, y = cuArea1y))

km2_plot +
  geom_point(aes(color = city_name), alpha = .5)

# Just Niamey and Yaoundé
casestudies %>%
  filter(city_name == "Niamey" | city_name == "YaoundÃ©") %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  geom_point(aes(color= city_name), alpha = .7) +
  labs(title = "Niamey vs. Yaoundé, cumulative area, 1985-2015, in square km")

# Additional construction area from previous 5 year timeframe


# --- How early or late the city's construction has been compared to other case studies. ---
proportion_5y_plot <- casestudies_5y %>%
  filter(year != "1985") %>% 
  ggplot(aes(x = city_name, y = prop5y, fill = year))

proportion_5y_plot +
  geom_bar(stat='identity') +
  scale_fill_gradient(low="blue", high="yellow")

# --- Subset to single city for easier plotting. ---
city <- casestudies[casestudies$city_name == "Niamey", ]

city_prop_5y_plot <- city %>%
  filter(year > 1994) %>% 
  ggplot(aes(x = year, y = prop5y, fill = year))
city_prop_5y_plot +
  geom_bar(stat='identity') +
  scale_fill_gradient(low="black", high="#EE6C4D")


# ------------- RE-GEOREFERENCE AND SAVE TO FILE ----------------
casestudies <- select(casestudies, -c(diff_year, diff_area))
casestudies <- merge(casestudies_sf, casestudies, by="caseID", all.x=TRUE, all.y=TRUE)
st_write(casestudies, "casestudies_scratch.shp", driver = "ESRI Shapefile", append=FALSE)

write.csv(casestudies, "casestudies_scratch.csv")
casestudies <- read.csv("casestudies_scratch.csv")


# ------------- CODEBOOK ----------------

# --- casestudies ---
## city_name
## Id
## year
## Shape_Area
## caseID
## totArea
## cuArea
## prop1y
## pcGRO_1y
## pcGRO_5y


