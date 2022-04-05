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
library(tidyr) # complete()
library(viridis)



# ------------- LOAD DATA ----------------

# --- 1. World Settlement Footprint Evolution ---
# Yearly built up area extents from 1985 - 2015
# Each year feature only contains the *additional* built up area from the year before, e.g. 1999 is *exclusive* of all 1998's existing area.
# The extents of each city boundary are based on whichever Africapolis settlement extent boundary they intersect with or are nearest to.
casestudies <-  st_read('WSFE_CMN_GRID3/WSFE_CMN_GRID3.shp')

# Clean up, if necessary
#casestudies <- rename(casestudies, c("year"="gridcode"))
#casestudies$year <- as.numeric(as.character(casestudies$year))

# Convert square meters to square kilometers (less unwieldy)
casestudies$area_km <- 0
casestudies <- casestudies %>% mutate(area_km = area_m / 1000000)



# ------------- PREPARE DATASET ----------------

# --- Add in rows for years where build-up was zero. ---
# Alternatively, you could account for the missing years in each calculation below. This global prep seemed easier, though more memory intensive.
casestudies <- complete(casestudies, year, NEAR_FID, fill=list(area_m=0, area_km=0))

# --- Total square area of the city. ---
# Creating a cumulative area field for 2015 for ease of calculation farther down.
# Note: Shape area variable is now in SQUARE KILOMETERS, not meters.
area_df <- aggregate(casestudies$area_km, list(casestudies$NEAR_FID), FUN=sum, na.rm=TRUE, na.action=NULL)
area_df <- rename(area_df, c("NEAR_FID"="Group.1", "totArea"="x")) # Calling it "tot" for "total" instead of "2015" in case any WSF-E rasters aren't available for 2015.
casestudies <- merge(x=casestudies, y=area_df, by='NEAR_FID', all.x=TRUE)

# Create unique ID for each location & year combination.
casestudies$built_ID <- 'string field placeholder'
casestudies$built_ID <- paste(casestudies$NEAR_FID, casestudies$year, sep="_")

# Arrange so that rows are grouped by location and sorted in ascending year order.
# This simplifies the stats approach below, as we can call on the previous row without specifying groups.
# There's a more eloquent way to do this, but I was struggling.
casestudies <- arrange(casestudies, year)
casestudies <- arrange(casestudies, NEAR_FID)



# ------------- SUMMARY STATISTICS, GROWTH ----------------

# --- Proportion of the city built in each year. (1985 should be treated as 'Before 1986') ---
casestudies$prop1y <- 1
casestudies$prop1y <- casestudies$area_km / casestudies$totArea * 100

# --- Total area of the city in the given year. ---
casestudies$cuArea1y <- 1
casestudies = casestudies %>% 
  group_by(NEAR_FID) %>% 
  mutate(cuArea1y = cumsum(area_km))

# --- Proportion of the city built in the last 5 years. (1989 should be treated as 'Before 1990') ---
# Also including a field for cumulative area from the last 5 years.
casestudies$cuArea5y <- 1
casestudies = casestudies %>% 
  group_by(NEAR_FID) %>% 
  mutate(cuArea5y = rollsumr(area_km, k = 5, fill = NA))

casestudies$prop5y <- 1
casestudies$prop5y <- casestudies$cuArea5y / casestudies$totArea * 100


# --- Percent increase in built area compared to previous year. ---
casestudies$pcGRO_1y <- 1
casestudies <- casestudies %>% 
  arrange(NEAR_FID) %>% 
  mutate(diff_year = year - lag(year),
         diff_area = cuArea1y - lag(cuArea1y),
         pcGRO_1y = (diff_area / diff_year) / lag(cuArea1y) * 100)


# --- Percent increase in built area compared to 5 years prior. ---
casestudies$pcGRO_5y <- 1
casestudies <- casestudies %>% 
  arrange(NEAR_FID) %>% 
  mutate(diff_year = year - lag(year, 5),
         diff_area = cuArea1y - lag(cuArea1y, 5),
         pcGRO_5y = ifelse(diff_year==5, (cuArea1y - lag(cuArea1y, 5)) / lag(cuArea1y, 5) * 100, NA))


# --- Percent increase in built area compared to 10 years prior. ---
casestudies$pcGRO_10y <- 1
casestudies <- casestudies %>% 
  arrange(NEAR_FID) %>% 
  mutate(diff_year = year - lag(year, 10),
         diff_area = cuArea1y - lag(cuArea1y, 10),
         pcGRO_10y = ifelse(diff_year==10, (cuArea1y - lag(cuArea1y, 10)) / lag(cuArea1y, 10) * 100, NA))


# --- Subset of 5 year increments ---
casestudies_5y <- casestudies %>% 
  filter_at(vars(year), 
            any_vars(. %in% c('1985', '1990', '1995', '2000', '2005', '2010', '2015'))) 



# ------------- SAVE TO FILE ----------------
casestudies <- select(casestudies, -c(diff_year, diff_area)) # Remove temporary columns.

st_write(casestudies, "WSFE_growthstats_grid3/WSFE_CMN_growthstats.shp", driver = "ESRI Shapefile", append=FALSE)







# ------------- VISUALIZATION ----------------
# Color blind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# --- Chart: Visualizing spikes in construction ---
# Additional construction area from previous year, yearly
km2_plot <- casestudies %>%
  ggplot(aes(x = year, y = cuArea1y))

km2_plot +
  geom_point(aes(color = NEAR_FID), alpha = .5)

# Just case study cities
# Bafoussam: 287 
# Douala: 17 
# Yaoundé: 280

casestudies %>%
  filter(NEAR_FID == 287 | NEAR_FID == 17 | NEAR_FID == 280) %>% 
  ggplot(aes(x = year, y = cuArea1y)) +
  geom_point(aes(color=factor(NEAR_FID))) +
  geom_line(aes(color=factor(NEAR_FID)), alpha = .4) +
  scale_color_discrete() +
  labs(title = "Cameroon: Bafoussam (287), Douala (17), and Yaounde (280), cumulative area in km^2")

casestudies %>%
  filter(NEAR_FID == 287 | NEAR_FID == 17 | NEAR_FID == 280) %>% 
  filter( year != 1985) %>%
  ggplot(aes(x = year, y = prop1y)) +
  geom_point(aes(color=factor(NEAR_FID))) +
  geom_line(aes(color=factor(NEAR_FID)), alpha = .4) +
  scale_color_discrete() +
  labs(title = "Cameroon: Bafoussam (287), Douala (17), and Yaounde (280), % of city area built that year")


# --- How early or late the city's construction has been compared to other case studies. ---
proportion_5y_plot <- casestudies_5y %>%
  filter(NEAR_FID == 287 | NEAR_FID == 17 | NEAR_FID == 280) %>% 
  filter(year != 1985) %>% 
  ggplot(aes(x = factor(NEAR_FID), y = prop5y, fill = year))

proportion_5y_plot +
  geom_bar(stat='identity') +
  scale_fill_gradient(low="blue", high="yellow") +
  labs(title = "Cameroon: Bafoussam (287), Douala (17), and Yaounde (280), % of city built in 5yr timeframe.")

# --- Subset to single city for easier plotting. ---
city <- casestudies[casestudies$NEAR_FID == 280, ]

city_prop_5y_plot <- city %>%
  filter(year > 1994) %>% 
  ggplot(aes(x = year, y = prop5y, fill = year))
city_prop_5y_plot +
  geom_bar(stat='identity') +
  scale_fill_gradient(low="black", high="#EE6C4D") + 
  labs(title = "Yaoundé, CMN: % of city built in 5yr timeframe.")


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


