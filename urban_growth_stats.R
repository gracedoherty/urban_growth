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
casestudies <-  st_read('WSFE_CAF_GRID3/WSFE_CAF_GRID3.shp')

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



# ------------- SAVE TO FILE ----------------
casestudies <- select(casestudies, -c(diff_year, diff_area)) # Remove temporary columns.

st_write(casestudies, "WSFE_growthstats_grid3/WSFE_CAF_growthstats.shp", driver = "ESRI Shapefile", append=FALSE)







# ------------- VISUALIZATION ----------------

# --- Prepare cities dataset ---

# Reload if new session.
casestudies <-  st_read('WSFE_growthstats_grid3/WSFE_CMN_growthstats.shp')

# Add name of case study city as new variable field for easy plotting.
casestudies$City <- NA

# Case study cities:
# BFA 
# casestudies <- within(casestudies, City[NEAR_FID == 354] <- 'Ouagadougou')
# casestudies <- within(casestudies, City[NEAR_FID == 332] <- 'Bobo-Dioulasso')
# casestudies <- within(casestudies, City[NEAR_FID == 347] <- 'Koudougou')
# casestudies <- within(casestudies, City[NEAR_FID == 180] <- 'Madiagdune')
# casestudies <- within(casestudies, City[NEAR_FID == 364] <- 'Kongoussi')
# CAF
# casestudies <- within(casestudies, City[NEAR_FID == 10028] <- 'Bangui')
# casestudies <- within(casestudies, City[NEAR_FID == 25878] <- 'Bambari')
# casestudies <- within(casestudies, City[NEAR_FID == 929] <- 'Berberati')
# CMN
casestudies <- within(casestudies, City[NEAR_FID == 18] <- 'Douala')
casestudies <- within(casestudies, City[NEAR_FID == 280] <- 'Yaounde')
casestudies <- within(casestudies, City[NEAR_FID == 266] <- 'Bamenda')
# COG
# casestudies <- within(casestudies, City[NEAR_FID == 8605 <- 'Brazzaville')
# casestudies <- within(casestudies, City[NEAR_FID == 439] <- 'Pointe-Noire')
# casestudies <- within(casestudies, City[NEAR_FID == 14780] <- 'Ouesso')
# GAB
# casestudies <- within(casestudies, City[NEAR_FID == 12] <- 'Libreville')
# casestudies <- within(casestudies, City[NEAR_FID == 18] <- 'Port-Gentil')
# casestudies <- within(casestudies, City[NEAR_FID == 6] <- 'Franceville')

# XXXXX
# MLI     Bamako: 97, 98
# casestudies <- within(casestudies, City[NEAR_FID == 97] <- 'Bamako')
# casestudies <- within(casestudies, City[NEAR_FID == 313] <- 'Gao')
# casestudies <- within(casestudies, City[NEAR_FID == 80] <- 'Sikasso')
# casestudies <- within(casestudies, City[NEAR_FID == 184] <- 'Kayes')
# XXXXX

# NER
# casestudies <- within(casestudies, City[NEAR_FID == 370] <- 'Niamey')
# casestudies <- within(casestudies, City[NEAR_FID == 161] <- 'Maradi')
# casestudies <- within(casestudies, City[NEAR_FID == 324] <- 'Zinder')
# casestudies <- within(casestudies, City[NEAR_FID == 341] <- 'Diffa')
# TCD
# casestudies <- within(casestudies, City[NEAR_FID == 131507] <- 'NDjamena')
# casestudies <- within(casestudies, City[NEAR_FID == 278172] <- 'Abeche')
# casestudies <- within(casestudies, City[NEAR_FID == 333078] <- 'Faya')
# casestudies <- within(casestudies, City[NEAR_FID == 234510] <- 'Bol')



# Narrow down to only case studies
cs_subset <- casestudies %>%
  filter(!is.na(City))

# Subset of 5 year increments
cs_5y <- cs_subset %>% 
  filter_at(vars(year), 
            any_vars(. %in% c('1985', '1990', '1995', '2000', '2005', '2010', '2015'))) 

# Subset to single city. 
cs_1 <- casestudies %>%
  filter(NEAR_FID == 266)

  


# --- Prepare design workspace ---

# Check available fonts
windowsFonts() # If limited, load desired font from computer's font library with showtext package.
# install.packages('showtext') 
library(showtext)
font_paths()
font_files()
font_add(family="Barlow", regular="C:/Users/grace/AppData/Local/Microsoft/Windows/Fonts/Barlow-Black.ttf")
font_families() # Desired font should now be available.

# Color palettes
cbPalette <- c("#E69F00", "#999999", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # Color blind friendly
boldcolorPalette <- c("#2a9d8f", "#e76f51", "#e9c46a", "#f4a261", "#264653") # Retro/bold

# Create quick-adds for plotting.
BOLD <- element_text(face="bold")
THEME_BARLOW <- theme_light() + theme(text=element_text(family="Barlow"), plot.title = BOLD, legend.title = BOLD) # https://ggplot2.tidyverse.org/reference/theme.html
CITY_LINE <- geom_line(aes(color=City), alpha = 1, lwd = 1.25) # Create line for each city

# Function for labeling the cumulative area by 2015.
max_cuArea1y <- function(city_max){
  points <- data.frame(xpt = rep(NA,length(city_max)),
                       ypt = rep(NA, length(city_max)))
  for(i in 1:length(city_max)){
    points$ypt[i] <- max(cs_subset$cuArea1y[cs_subset$City == city_max[i] & cs_subset$year!=1985],na.rm = T)
    check <- cs_subset$year[cs_subset$City == city_max[i] & cs_subset$cuArea1y == points$ypt[i]]
    points$xpt[i] <- max(check, na.rm = T)
  }
  list(annotate(geom = "text", x = points$xpt, y=points$ypt+2, label =round(points$ypt, digits=2), 
                fontface="italic", color="#6c757d", size=3))
}

# Function for marking the year in which the largest proportion was built
max_prop1y <- function(city_max){
  points <- data.frame(xpt = rep(NA,length(city_max)),
                       ypt = rep(NA, length(city_max)))
  for(i in 1:length(city_max)){
    points$ypt[i] <- max(cs_subset$prop1y[cs_subset$City == city_max[i] & cs_subset$year!=1985],na.rm = T)
    check <- cs_subset$year[cs_subset$City == city_max[i] & cs_subset$prop1y == points$ypt[i]]
    points$xpt[i] <- max(check, na.rm = T)
  }
  list(geom_segment(data = points, aes(x=xpt,y=ypt,xend=xpt+3,yend=ypt), inherit.aes = F, color="#6c757d"),
       annotate(geom = "text", x = points$xpt+4, y=points$ypt, label =points$xpt, fontface="italic", color="#6c757d"))
}




# --- Chart: Cumulative area ---
cs_subset %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  CITY_LINE +
  THEME_BARLOW +
  max_cuArea1y(c("Ouagadougou", "Bobo-Dioulasso", "Koudougou", "Madiagdune", "Kongoussi"))+
  scale_color_manual(values = cbPalette) + 
  labs(title = "Burkina Faso", subtitle ="Cumulative area of the city, 1985-2015", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))


# --- Chart: Proportion of area ---
cs_subset %>%
  filter(year != 1985) %>%
  ggplot(aes(x = year, y = prop1y)) +
  CITY_LINE +
  THEME_BARLOW +
  max_prop1y(c("Ouagadougou", "Bobo-Dioulasso", "Koudougou", "Madiagdune", "Kongoussi"))+
  scale_color_manual(values = cbPalette) + 
  labs(title = "Burkina Faso", subtitle ="Proportion of city built each year, 1986-2015", 
       x = "Year", y = expression(paste("Proportion of built-up area (%)")))


# --- Chart: Proportion of area, 5 year increments ---
cs_5y %>%
  filter(year != 1985) %>%
  ggplot(aes(x = City, y = prop5y, fill = year)) +
  THEME_BARLOW +
  geom_bar(stat='identity') +
  scale_fill_gradient(low="black", high="#EE6C4D") + 
  labs(title = "Cameroon", subtitle ="Proportion of city built every 5 years, 1986-2015", 
       x = "", y = expression(paste("Proportion of built-up area (%)")))


# --- One city at a time. ---
# Cumulative area
cs_1 %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  CITY_LINE +
  geom_area(fill = "#E69F00", alpha = .4) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  labs(title = "Cameroon", subtitle ="Cumulative area of Bamenda, 1985-2015", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))

# Proportion (1 year)
cs_1 %>%
  filter(year != 1985) %>%
  ggplot(aes(x = year, y = prop1y)) +
  CITY_LINE +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  labs(title = "Burkina Faso", subtitle ="Proportion of Bamenda built each year, 1986-2015", 
       x = "Year", y = expression(paste("Proportion of built-up area (%)")))



# ------------- CODEBOOK ----------------

# --- casestudies ---
## NEAR_FID       GRID3 settlement ID used to define the extents of the WSF agglomeration.
## City           Name of settlement, based on common knowledge.
## year           Year that the reported build-up was first detected.
## area_m         Total geographic area of build-up detected for that year for that settlement, in meters.
## area_km        Total geographic area of build-up detected for that year for that settlement, in kilometers.
## built_ID       Unique ID field. The name is derived from GRID3 settlement ID + year of first detection.
## totArea        Total geographic area for all years combined for the GRID3 settlement, in kilometers.
## cuArea1y       Cumulative area of the settlement by that year.
## prop1y         Areal proportion of the settlement attributable to that year.
## prop5y         Areal proportion of the settlement attributable to the last five years, inclusive of that year.
## pcGRO_1y       Percent increase in area from the previous year's cumulative total area.


