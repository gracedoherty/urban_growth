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
# The extents of each city boundary are based on whichever GRID3 settlement extent boundary they intersect with or are nearest to.
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

st_write(casestudies, "WSFE_growthstats_grid3_v2/WSFE_CAF_growthstats2.shp", driver = "ESRI Shapefile", append=FALSE)





# ------------- AGGREGATED TRENDS ----------------

# Reload if necessary.
casestudies <-  st_read('WSFE_growthstats_grid3_v2/WSFE_BFA_adm.shp')
casestudies <- casestudies %>% st_drop_geometry()

# Make a simpler settlement type variable for generating unique IDs later on.
casestudies$typ = "placeholder"
casestudies$typ = with(casestudies, ifelse(type == "Built-up Area", "bua",
                                                     ifelse(type == "Small Settlement Area", "ssa", "ha")))


# --- 1. Total country annual growth rate and proportion of build-up. ---

# Average growth rate per year, weighted by settlement. (every BUA, SSA, and HA has an equal weight).
# growth_weighted_adm0 <- 1
# growth_weighted_adm0 <- casestudies %>% 
#   group_by(year) %>%
#   dplyr::summarise(GRO_w = mean(pcGRO_1y, na.rm=T)) %>% 
#   as.data.frame()


# Average growth rate per year, unweighted. (Total increase in built area across the country.)
# Also includes a variable for total area (km) built in that year, and total cumulative built area by that year.
growth_unw_adm0 <- 1
growth_unw_adm0 <- casestudies %>% 
  group_by(year) %>%
  dplyr::summarise(area_km = sum(area_km, na.rm=T)) %>% 
  as.data.frame()

growth_unw_adm0 <- arrange(growth_unw_adm0, year) # Ensure that the years are ordered sequentially.
growth_unw_adm0$cuArea = 1
growth_unw_adm0$cuArea = cumsum(growth_unw_adm0$area_km)
totarea = sum(growth_unw_adm0$area_km) # Quality control: This value should be the same as cuArea for 2015.

growth_unw_adm0$GRO_unw = 1
growth_unw_adm0 = growth_unw_adm0 %>% mutate(GRO_unw= ((area_km / (cuArea - area_km)) * 100))


# Proportion of the country built in that year, unweighted.
growth_unw_adm0$prop = 1
growth_unw_adm0 = growth_unw_adm0 %>% mutate(prop= ((area_km / totarea) * 100))
sum(growth_unw_adm0$prop) # Quality control: This value should be 100. (100 percent)


# growth_unw_adm0 <- merge(growth_weighted_adm0, growth_unw_adm0, by="year")
write.csv(growth_unw_adm0, file = "BFA_growth_adm0.csv")




# --- 2. Disaggregated by settlement type: Total country annual growth rate and proportion of build-up. ---

# Average growth rate per year per settlement type, weighted by settlement. (every settlement within its class has an equal weight).
growth_type_adm0 <- 1
growth_type_adm0 <- casestudies %>% 
  group_by(year, typ) %>%
  dplyr::summarise(GRO_w = mean(pcGRO_1y, na.rm=T)) %>% 
  as.data.frame()

# Create unique ID.
growth_type_adm0$typ_yr <- 'placeholder'
growth_type_adm0$typ_yr <- paste(growth_type_adm0$typ, growth_type_adm0$year, sep="_")


# Average growth rate per year, per settlement type, unweighted. (Total increase in built area across the country for each type.)
# Also includes a variable for total area (km) built in that year, and total cumulative built area by that year.
growth_type_unw_adm0 <- 1
growth_type_unw_adm0 <- casestudies %>% 
  group_by(year, typ) %>%
  dplyr::summarise(area_km = sum(area_km, na.rm=T)) %>% 
  as.data.frame()

growth_type_unw_adm0 <- arrange(growth_type_unw_adm0, year) # Ensure that the years are ordered sequentially.
growth_type_unw_adm0$cuArea = 1
growth_type_unw_adm0 = growth_type_unw_adm0 %>%
  group_by(typ) %>%
  mutate(cuArea = cumsum(area_km))

growth_type_unw_adm0$GRO_unw = 1
growth_type_unw_adm0 = growth_type_unw_adm0 %>% mutate(GRO_unw= ((area_km / (cuArea - area_km)) * 100))
totarea_bua = sum(growth_type_unw_adm0$area_km[growth_type_unw_adm0$typ=="bua"]) # Quality control: This value should be the same as cuArea for 2015.
totarea_ssa = sum(growth_type_unw_adm0$area_km[growth_type_unw_adm0$typ=="ssa"])
totarea_ha = sum(growth_type_unw_adm0$area_km[growth_type_unw_adm0$typ=="ha"])

growth_type_unw_adm0$GRO_unw = 1
growth_type_unw_adm0 = growth_type_unw_adm0 %>% mutate(GRO_unw= ((area_km / (cuArea - area_km)) * 100))


# Proportion of each settlement type built in that year, unweighted.
growth_type_unw_adm0 <- growth_type_unw_adm0 %>%
                        group_by(typ) %>%
                        mutate(typ_area_tot = sum(area_km, na.rm = T)) %>%
                        ungroup() %>%
                        mutate(prop = area_km/typ_area_tot * 100)
sum(growth_type_unw_adm0$prop) # Quality control: This value should be 300 (100% x 3)

growth_type_unw_adm0 = select(growth_type_unw_adm0, -c(typ_area_tot))

# Create unique ID, then merge with other settlement-disaggregated file.
growth_type_unw_adm0$typ_yr <- 'placeholder'
growth_type_unw_adm0$typ_yr <- paste(growth_type_unw_adm0$typ, growth_type_unw_adm0$year, sep="_")

WSFE_type_adm0_stats <- merge(growth_type_adm0, growth_type_unw_adm0, by="typ_yr")

write.csv(WSFE_type_adm0_stats, file = "BFA_growth_SettlementType_adm0.csv")





# --- 3. By ADM1 region. ---

# Average growth rate per year per region, weighted by settlement. (every settlement within its region has an equal weight).
# growth_weighted_adm1 <- 1
# growth_weighted_adm1 <- casestudies %>% 
#   group_by(year, admin1Pcod) %>%
#   dplyr::summarise(GRO_w = mean(pcGRO_1y, na.rm=T)) %>% 
#   as.data.frame()


# Average growth rate per year, per region, unweighted. (Total increase in built area across the region.)
# Also includes a variable for total area (km) built in that year, and total cumulative built area by that year.
growth_unw_adm1 <- 1
growth_unw_adm1 <- casestudies %>% 
  group_by(year, admin1Pcod) %>%
  dplyr::summarise(area_km = sum(area_km, na.rm=T)) %>% 
  as.data.frame()

growth_unw_adm1 <- arrange(growth_unw_adm1, year) # Ensure that the years are ordered sequentially.
growth_unw_adm1$cuArea = 1
growth_unw_adm1$cuArea = cumsum(growth_unw_adm1$area_km)
totarea = sum(growth_unw_adm1$area_km) # Quality control: This value should be the same as cuArea for 2015.

growth_unw_adm1$GRO_unw = 1
growth_unw_adm1 = growth_unw_adm1 %>% mutate(GRO_unw= ((area_km / (cuArea - area_km)) * 100))


# Proportion of the country built in that year, unweighted.
growth_unw_adm1$prop = 1
growth_unw_adm1 = growth_unw_adm1 %>% mutate(prop= ((area_km / totarea) * 100))
sum(growth_unw_adm1$prop) # Quality control: This value should be 100. (100 percent)


# Create unique ID, then save to file.
growth_unw_adm1$adm_yr <- 'placeholder'
growth_unw_adm1$adm_yr <- paste(growth_unw_adm1$admin1Pcod, growth_unw_adm1$year, sep="_")

# growth_unw_adm1 <- merge(growth_weighted_adm1, growth_unw_adm1, by="year")
write.csv(growth_unw_adm1, file = "BFA_growth_adm1.csv")




# --- 4. Disaggregated by settlement type: Regional annual growth rate and proportion of build-up. ---

# Average growth rate per year per settlement type, weighted by settlement. (every settlement within its class has an equal weight).
growth_type_w_adm1 <- 1
growth_type_w_adm1 <- casestudies %>% 
  group_by(year, typ, admin1Pcod) %>%
  dplyr::summarise(GRO_w = mean(pcGRO_1y, na.rm=T)) %>% 
  as.data.frame()

# Create unique ID.
growth_type_w_adm1$adm_typ_yr <- 'placeholder'
growth_type_w_adm1$adm_typ_yr <- paste(growth_type_w_adm1$admin1Pcod, growth_type_w_adm1$typ, growth_type_w_adm1$year, sep="_")


# Average growth rate per year, per settlement type, unweighted. (Total increase in built area across the country for each type.)
# Also includes a variable for total area (km) built in that year, and total cumulative built area by that year.
growth_type_unw_adm1 <- 1
growth_type_unw_adm1 <- casestudies %>% 
  group_by(year, typ, admin1Pcod) %>%
  dplyr::summarise(area_km = sum(area_km, na.rm=T)) %>% 
  as.data.frame()

growth_type_unw_adm1 <- arrange(growth_type_unw_adm1, year) # Ensure that the years are ordered sequentially.
growth_type_unw_adm1$cuArea = 1
growth_type_unw_adm1 = growth_type_unw_adm1 %>%
  group_by(typ, admin1Pcod) %>%
  mutate(cuArea = cumsum(area_km))

growth_type_unw_adm1$GRO_unw = 1
growth_type_unw_adm1 = growth_type_unw_adm1 %>% mutate(GRO_unw= ((area_km / (cuArea - area_km)) * 100))


# Proportion of each settlement type built in that year, unweighted.
growth_type_unw_adm1 <- growth_type_unw_adm1 %>%
  group_by(typ, admin1Pcod) %>%
  mutate(adm_typ_area_tot = sum(area_km, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop = area_km/adm_typ_area_tot * 100)
sum(growth_type_unw_adm1$prop[growth_type_unw_adm1$typ=="bua"])

growth_type_unw_adm1 = select(growth_type_unw_adm1, -c(adm_typ_area_tot))

# Create unique ID, then merge with other settlement-disaggregated file.
growth_type_unw_adm1$adm_typ_yr <- 'placeholder'
growth_type_unw_adm1$adm_typ_yr <- paste(growth_type_unw_adm1$admin1Pcod, growth_type_unw_adm1$typ, growth_type_unw_adm1$year, sep="_")

WSFE_type_adm1_stats <- merge(growth_type_w_adm1, growth_type_unw_adm1, by="adm_typ_yr")

write.csv(WSFE_type_adm1_stats, file = "BFA_growth_SettlementType_adm1.csv")




# --- 5. By ADM2 province. ---

# Average growth rate per year per region, weighted by settlement. (every settlement within its region has an equal weight).
# growth_weighted_adm2 <- 1
# growth_weighted_adm2 <- casestudies %>% 
#   group_by(year, admin2Pcod) %>%
#   dplyr::summarise(GRO_w = mean(pcGRO_1y, na.rm=T)) %>% 
#   as.data.frame()


# Average growth rate per year, per region, unweighted. (Total increase in built area across the region.)
# Also includes a variable for total area (km) built in that year, and total cumulative built area by that year.
growth_unw_adm2 <- 1
growth_unw_adm2 <- casestudies %>% 
  group_by(year, admin2Pcod) %>%
  dplyr::summarise(area_km = sum(area_km, na.rm=T)) %>% 
  as.data.frame()

growth_unw_adm2 <- arrange(growth_unw_adm2, year) # Ensure that the years are ordered sequentially.
growth_unw_adm2$cuArea = 1
growth_unw_adm2$cuArea = cumsum(growth_unw_adm2$area_km)
totarea = sum(growth_unw_adm2$area_km) # Quality control: This value should be the same as cuArea for 2015.

growth_unw_adm2$GRO_unw = 1
growth_unw_adm2 = growth_unw_adm2 %>% mutate(GRO_unw= ((area_km / (cuArea - area_km)) * 100))


# Proportion of the country built in that year, unweighted.
growth_unw_adm2$prop = 1
growth_unw_adm2 = growth_unw_adm2 %>% mutate(prop= ((area_km / totarea) * 100))
sum(growth_unw_adm2$prop) # Quality control: This value should be 100. (100 percent)


# Create unique ID, then save to file.
growth_unw_adm2$adm_yr <- 'placeholder'
growth_unw_adm2$adm_yr <- paste(growth_unw_adm2$admin2Pcod, growth_unw_adm2$year, sep="_")

# growth_unw_adm2 <- merge(growth_weighted_adm2, growth_unw_adm2, by="year")
write.csv(growth_unw_adm2, file = "BFA_growth_adm2.csv")




# --- 6. Disaggregated by settlement type: Regional annual growth rate and proportion of build-up. ---

# Average growth rate per year per settlement type, weighted by settlement. (every settlement within its class has an equal weight).
growth_type_w_adm2 <- 1
growth_type_w_adm2 <- casestudies %>% 
  group_by(year, typ, admin2Pcod) %>%
  dplyr::summarise(GRO_w = mean(pcGRO_1y, na.rm=T)) %>% 
  as.data.frame()

# Create unique ID.
growth_type_w_adm2$adm_typ_yr <- 'placeholder'
growth_type_w_adm2$adm_typ_yr <- paste(growth_type_w_adm2$admin2Pcod, growth_type_w_adm2$typ, growth_type_w_adm2$year, sep="_")


# Average growth rate per year, per settlement type, unweighted. (Total increase in built area across the country for each type.)
# Also includes a variable for total area (km) built in that year, and total cumulative built area by that year.
growth_type_unw_adm2 <- 1
growth_type_unw_adm2 <- casestudies %>% 
  group_by(year, typ, admin2Pcod) %>%
  dplyr::summarise(area_km = sum(area_km, na.rm=T)) %>% 
  as.data.frame()

growth_type_unw_adm2 <- arrange(growth_type_unw_adm2, year) # Ensure that the years are ordered sequentially.
growth_type_unw_adm2$cuArea = 1
growth_type_unw_adm2 = growth_type_unw_adm2 %>%
  group_by(typ, admin2Pcod) %>%
  mutate(cuArea = cumsum(area_km))

growth_type_unw_adm2$GRO_unw = 1
growth_type_unw_adm2 = growth_type_unw_adm2 %>% mutate(GRO_unw= ((area_km / (cuArea - area_km)) * 100))


# Proportion of each settlement type built in that year, unweighted.
growth_type_unw_adm2 <- growth_type_unw_adm2 %>%
  group_by(typ, admin2Pcod) %>%
  mutate(adm_typ_area_tot = sum(area_km, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop = area_km/adm_typ_area_tot * 100)
sum(growth_type_unw_adm2$prop[growth_type_unw_adm2$typ=="bua"])

growth_type_unw_adm2 = select(growth_type_unw_adm2, -c(adm_typ_area_tot))

# Create unique ID, then merge with other settlement-disaggregated file.
growth_type_unw_adm2$adm_typ_yr <- 'placeholder'
growth_type_unw_adm2$adm_typ_yr <- paste(growth_type_unw_adm2$admin2Pcod, growth_type_unw_adm2$typ, growth_type_unw_adm2$year, sep="_")

WSFE_type_adm2_stats <- merge(growth_type_w_adm2, growth_type_unw_adm2, by="adm_typ_yr")

write.csv(WSFE_type_adm2_stats, file = "BFA_growth_SettlementType_adm2.csv")






# --- 7. By ADM3 department. ---

# Average growth rate per year per region, weighted by settlement. (every settlement within its region has an equal weight).
# growth_weighted_adm3 <- 1
# growth_weighted_adm3 <- casestudies %>% 
#   group_by(year, admin3Pcod) %>%
#   dplyr::summarise(GRO_w = mean(pcGRO_1y, na.rm=T)) %>% 
#   as.data.frame()


# Average growth rate per year, per region, unweighted. (Total increase in built area across the region.)
# Also includes a variable for total area (km) built in that year, and total cumulative built area by that year.
growth_unw_adm3 <- 1
growth_unw_adm3 <- casestudies %>% 
  group_by(year, admin3Pcod) %>%
  dplyr::summarise(area_km = sum(area_km, na.rm=T)) %>% 
  as.data.frame()

growth_unw_adm3 <- arrange(growth_unw_adm3, year) # Ensure that the years are ordered sequentially.
growth_unw_adm3$cuArea = 1
growth_unw_adm3$cuArea = cumsum(growth_unw_adm3$area_km)
totarea = sum(growth_unw_adm3$area_km) # Quality control: This value should be the same as cuArea for 2015.

growth_unw_adm3$GRO_unw = 1
growth_unw_adm3 = growth_unw_adm3 %>% mutate(GRO_unw= ((area_km / (cuArea - area_km)) * 100))


# Proportion of the country built in that year, unweighted.
growth_unw_adm3$prop = 1
growth_unw_adm3 = growth_unw_adm3 %>% mutate(prop= ((area_km / totarea) * 100))
sum(growth_unw_adm3$prop) # Quality control: This value should be 100. (100 percent)


# Create unique ID, then save to file.
growth_unw_adm3$adm_yr <- 'placeholder'
growth_unw_adm3$adm_yr <- paste(growth_unw_adm3$admin3Pcod, growth_unw_adm3$year, sep="_")

# growth_unw_adm3 <- merge(growth_weighted_adm3, growth_unw_adm3, by="year")
write.csv(growth_unw_adm3, file = "BFA_growth_adm3.csv")




# --- 8. Disaggregated by settlement type: Regional annual growth rate and proportion of build-up. ---

# Average growth rate per year per settlement type, weighted by settlement. (every settlement within its class has an equal weight).
growth_type_w_adm3 <- 1
growth_type_w_adm3 <- casestudies %>% 
  group_by(year, typ, admin3Pcod) %>%
  dplyr::summarise(GRO_w = mean(pcGRO_1y, na.rm=T)) %>% 
  as.data.frame()

# Create unique ID.
growth_type_w_adm3$adm_typ_yr <- 'placeholder'
growth_type_w_adm3$adm_typ_yr <- paste(growth_type_w_adm3$admin3Pcod, growth_type_w_adm3$typ, growth_type_w_adm3$year, sep="_")


# Average growth rate per year, per settlement type, unweighted. (Total increase in built area across the country for each type.)
# Also includes a variable for total area (km) built in that year, and total cumulative built area by that year.
growth_type_unw_adm3 <- 1
growth_type_unw_adm3 <- casestudies %>% 
  group_by(year, typ, admin3Pcod) %>%
  dplyr::summarise(area_km = sum(area_km, na.rm=T)) %>% 
  as.data.frame()

growth_type_unw_adm3 <- arrange(growth_type_unw_adm3, year) # Ensure that the years are ordered sequentially.
growth_type_unw_adm3$cuArea = 1
growth_type_unw_adm3 = growth_type_unw_adm3 %>%
  group_by(typ, admin3Pcod) %>%
  mutate(cuArea = cumsum(area_km))

growth_type_unw_adm3$GRO_unw = 1
growth_type_unw_adm3 = growth_type_unw_adm3 %>% mutate(GRO_unw= ((area_km / (cuArea - area_km)) * 100))


# Proportion of each settlement type built in that year, unweighted.
growth_type_unw_adm3 <- growth_type_unw_adm3 %>%
  group_by(typ, admin3Pcod) %>%
  mutate(adm_typ_area_tot = sum(area_km, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop = area_km/adm_typ_area_tot * 100)
sum(growth_type_unw_adm3$prop[growth_type_unw_adm3$typ=="bua"])

growth_type_unw_adm3 = select(growth_type_unw_adm3, -c(adm_typ_area_tot))

# Create unique ID, then merge with other settlement-disaggregated file.
growth_type_unw_adm3$adm_typ_yr <- 'placeholder'
growth_type_unw_adm3$adm_typ_yr <- paste(growth_type_unw_adm3$admin3Pcod, growth_type_unw_adm3$typ, growth_type_unw_adm3$year, sep="_")

WSFE_type_adm3_stats <- merge(growth_type_w_adm3, growth_type_unw_adm3, by="adm_typ_yr")

write.csv(WSFE_type_adm3_stats, file = "BFA_growth_SettlementType_adm3.csv")






# ------------- VISUALIZATION ----------------

# --- Prepare cities dataset ---

# Reload if necessary.
casestudies <-  st_read('WSFE_growthstats_grid3_v2/WSFE_BFA_growthstats2.shp')
casestudies <- casestudies %>% st_drop_geometry()
country_agg <- read.csv('BFA_growth_SettlementType_adm0.csv')

# Add name of case study city as new variable field for easy plotting.
casestudies$City <- NA

# Case study cities:
# BFA 
casestudies <- within(casestudies, City[NEAR_FID == 354] <- 'Ouagadougou')
casestudies <- within(casestudies, City[NEAR_FID == 332] <- 'Bobo-Dioulasso')
casestudies <- within(casestudies, City[NEAR_FID == 347] <- 'Koudougou')
casestudies <- within(casestudies, City[NEAR_FID == 180] <- 'Madiagdune')
casestudies <- within(casestudies, City[NEAR_FID == 364] <- 'Kongoussi')
# CAF
# casestudies <- within(casestudies, City[NEAR_FID == 10028] <- 'Bangui')
# casestudies <- within(casestudies, City[NEAR_FID == 25878] <- 'Bambari')
# casestudies <- within(casestudies, City[NEAR_FID == 929] <- 'Berberati')
# CMN
# casestudies <- within(casestudies, City[NEAR_FID == 18] <- 'Douala')
# casestudies <- within(casestudies, City[NEAR_FID == 280] <- 'Yaounde')
# casestudies <- within(casestudies, City[NEAR_FID == 266] <- 'Bamenda')
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
  filter(NEAR_FID == 354)

# Narrow down to only Built-Up Areas, and only Settlement Areas.
country_bua = country_agg %>%
  filter(type.x=='bua')




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
cuAreaplot = cs_subset %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  CITY_LINE +
  THEME_BARLOW +
  max_cuArea1y(c("Ouagadougou", "Bobo-Dioulasso", "Koudougou", "Madiagdune", "Kongoussi"))+
  scale_color_manual(values = cbPalette) + 
  labs(title = "Comparing cities of Burkina Faso", subtitle ="Cumulative area of the city, 1985-2015", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))

cuAreaplot + geom_line(data=wsfe_adm, aes(color=City), alpha = 1, lwd = 1.25)


# --- Chart: Proportion of area ---
cs_subset %>%
  filter(year != 1985) %>%
  ggplot(aes(x = year, y = prop1y)) +
  CITY_LINE +
  THEME_BARLOW +
  max_prop1y(c("Koudougou", "Madiagdune", "Kongoussi"))+
  scale_color_manual(values = cbPalette) + 
  labs(title = "Comparing cities of Burkina Faso", subtitle ="Proportion of the total city area built each year, 1986-2015", 
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
  labs(title = "Ouagadougou, Burkina Faso", subtitle ="Cumulative area, 1985-2015", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))

# Proportion (1 year)
cs_1 %>%
  filter(year != 1985) %>%
  ggplot(aes(x = year, y = prop1y)) +
  CITY_LINE +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  labs(title = "Ouagadougou, Burkina Faso", subtitle ="Proportion of the total city area built each year, 1986-2015", 
       x = "Year", y = expression(paste("Proportion of built-up area (%)")))



# ------------- CODEBOOK ----------------

# --- WSFE_[COUNTRY-ISO]_growthstats.shp ---
# 

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



# --- [COUNTRY-ISO]_growth_adm0.csv ---
## year           Year that the reported build-up was first detected.
## area_km        Total geographic area of build-up detected for that year for all settlements, in kilometers.
## cuArea         Cumulative area of build-up for all settlements by that year, in kilometers.
## GRO_unw        Average growth rate per year, unweighted. (Total increase in built area across the country.)
## GRO_w          Average growth rate per year, weighted by settlement. (every settlement of every type has an equal weight).
## prop           Average proportion of the country built in that year.


# --- [COUNTRY-ISO]_growth_SettlementType_adm0.csv ---
## All variables the same as [COUNTRY-ISO]_growth_adm0.csv, but disaggregated by settlement type. (Each year has three rows: BUA, SSA, and HA).
## typ_yr         Unique ID field combining settlement type and year of build-up.
## typ            Type of settlement as defined by GRID3: Built-up Area (bua), Small Settlement Area (ssa), or Hamlet Area (ha).
## year           Year that the reported build-up was first detected.
## area_km        Total geographic area of build-up detected for that year for that settlement type, in kilometers.
## cuArea         Cumulative area of build-up for that settlement type by that year, in kilometers.
## GRO_unw        Average growth rate from previous year for that settlement type, no weighting. (Total rate of increase in built area for that settlement type across the country.)
## GRO_w          Average growth rate from previous year for that settlement type, where every settlement in that type has an equal weight, without consideration for relative size. For example, an SSA with 2km of build-up has as much influence as an SSA with 1km.
## prop           Average proportion of that settlement type built in that year.


# --- [COUNTRY-ISO]_growth_adm1.csv ---
## All variables the same as [COUNTRY-ISO]_growth_adm0.csv, but for each ADM1 region.
## adm_typ_yr     Unique ID field combining administrative code, settlement type, and year of build-up.


# --- [COUNTRY-ISO]_growth_SettlementType_adm1.csv ---
## All variables the same as [COUNTRY-ISO]_growth_SettlementType_adm0.csv, but for each ADM1 region.
## adm_typ_yr     Unique ID field combining administrative code, settlement type, and year of build-up.


