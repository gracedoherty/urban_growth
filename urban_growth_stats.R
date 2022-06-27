# This script contains basic calculations of urban growth and building construction from World Settlement Footprint.
# See end of script for variable codebook.


# ------------- PREPARE WORKSPACE -------------
setwd("C:/Users/grace/GIS/povertyequity/urban_growth")
wd = getwd()
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
library(ggnewscale) # The ggnewscale::new_scale_colour() command acts as an instruction to ggplot2 to initialise a new colour scale: scale and guide commands that appear above the new_scale_colour() command will be applied to the first colour scale, and commands that appear below are applied to the second colour scale.
library(svglite) # ggsave()


# ------------- LOAD DATA ----------------

# --- 1. World Settlement Footprint Evolution ---
# Yearly built up area extents from 1985 - 2015
# Each year feature only contains the *additional* built up area from the year before, e.g. 1999 is *exclusive* of all 1998's existing area.
# The extents of each city boundary are based on whichever GRID3 settlement extent boundary they intersect with or are nearest to.
AllBuildup =  st_read('WSFE_growthstats_grid3_v2/WSFE_TCD_growthstats2.shp')

# Clean up, if necessary
#AllBuildup$year = as.numeric(as.character(AllBuildup$year))

AllBuildup = select(AllBuildup, c(WSFE_ID, GRID3_ID, year, area_m, type, ADM1, pop2020, geometry))
# AllBuildup = select(AllBuildup, c(TARGET_FID, NEAR_FID, year, area_m, type, ADM1_CODE, pop_un_adj, geometry))
# AllBuildup = rename(AllBuildup, c("WSFE_ID"="TARGET_FID", "GRID3_ID"="NEAR_FID", "ADM1"="ADM1_CODE", "pop2020"="pop_un_adj"))

# Accidentally dropped the pop value earlier
# GRID3 = st_read("GRID3_albers/GRID3_TCD_albers.shp")
# GRID3 = select(GRID3, c(OBJECTID, pop_un_adj))
# GRID3 = GRID3 %>% st_drop_geometry()
# GRID3 = rename(GRID3, c("pop2020"="pop_un_adj"))
# AllBuildup = merge(AllBuildup, GRID3, by.x="GRID3_ID", by.y="OBJECTID", all.x=T)

# Convert square meters to square kilometers (less unwieldy)
AllBuildup$AreaKM = 0
AllBuildup = AllBuildup %>% mutate(AreaKM = area_m / 1000000)


# ------------- PREPARE DATASET ----------------

# # --- Merge any settlement IDs that should be a single city (to align with common / political definition of the city's extent) ---
# Bamako = AllBuildup %>%
#               filter(GRID3_ID==97 | GRID3_ID == 98) %>%
#               group_by(year) %>%
#               mutate(area_m = sum(area_m))
# Bamako = Bamako[Bamako$GRID3_ID == 97,]
# 
# # Making sure it worked by spot-checking that area_m column would add up to new merged value
# AllBuildup_9798 = AllBuildup[AllBuildup$GRID3_ID == 97 | AllBuildup$GRID3_ID == 98,]
# 
# # Append
# AllBuildup = AllBuildup[!AllBuildup$GRID3_ID == 97 & !AllBuildup$GRID3_ID == 98,]
# AllBuildup = rbind(AllBuildup, Bamako)
# 
# rm(Bamako)


# --- Add in rows for years where build-up was zero. ---
# Alternatively, you could account for the missing years in each calculation below. This global prep seemed easier, though more memory intensive.
AllBuildup = complete(AllBuildup, year, GRID3_ID, fill=list(area_m=0, AreaKM=0))

# --- Total square area of the city. ---
# Creating a cumulative area field for 2015 for ease of calculation farther down.
# Note: Shape area variable is now in SQUARE KILOMETERS, not meters.
area_df = aggregate(AllBuildup$AreaKM, list(AllBuildup$GRID3_ID), FUN=sum, na.rm=TRUE, na.action=NULL)
area_df = rename(area_df, c("GRID3_ID"="Group.1", "TotArea"="x")) # Calling it "tot" for "total" instead of "2015" in case any WSF-E rasters aren't available for 2015.
AllBuildup = merge(x=AllBuildup, y=area_df, by='GRID3_ID', all.x=TRUE)
rm(area_df)

# Create unique ID for each location & year combination.
AllBuildup$built_ID = paste(AllBuildup$GRID3_ID, AllBuildup$year, sep="_")

# Arrange so that rows are grouped by location and sorted in ascending year order.
# This simplifies the stats approach below, as we can call on the previous row without specifying groups.
# There's a more eloquent way to do this, but I was struggling.
AllBuildup = arrange(AllBuildup, year)
AllBuildup = arrange(AllBuildup, GRID3_ID)



# ------------- SUMMARY STATISTICS, GROWTH ----------------

AllBuildup$Prop1y = AllBuildup$AreaKM / AllBuildup$TotArea # Proportion of the city built in each year. (1985 should be treated as 'Before 1986')

AllBuildup = AllBuildup %>% 
  group_by(GRID3_ID) %>% 
  mutate(CuArea = cumsum(AreaKM)) # Total area of the city in the given year.

AllBuildup = AllBuildup %>% 
  group_by(GRID3_ID) %>% 
  mutate(CuArea5y = rollsumr(AreaKM, k = 5, fill = NA)) # Cumulative area from the last 5 years.

AllBuildup$Prop5y = AllBuildup$CuArea5y / AllBuildup$TotArea # Proportion of the city built in the last 5 years. (1989 should be treated as 'Before 1990')

AllBuildup = AllBuildup %>% 
  arrange(GRID3_ID) %>% 
  mutate(diff_year = year - lag(year),
         diff_area = CuArea - lag(CuArea),
         PcGro1y = (diff_area / diff_year) / lag(CuArea)) # Percent increase in built area compared to previous year.

AllBuildup = AllBuildup %>% 
  arrange(GRID3_ID) %>% 
  mutate(diff_year = year - lag(year, 5),
         diff_area = CuArea - lag(CuArea, 5),
         PcGro5y = ifelse(diff_year==5, (CuArea - lag(CuArea, 5)) / lag(CuArea, 5), NA)) # Percent increase in built area compared to 5 years prior.

AllBuildup = AllBuildup %>% 
  arrange(GRID3_ID) %>% 
  mutate(diff_year = year - lag(year, 10),
         diff_area = CuArea - lag(CuArea, 10),
         PcGro10y = ifelse(diff_year==10, (CuArea - lag(CuArea, 10)) / lag(CuArea, 10), NA)) # Percent increase in built area compared to 10 years prior.

AllBuildup = select(AllBuildup, -c(diff_year, diff_area)) # Remove temporary columns.





# ------------- AGGREGATED TRENDS ----------------

# Make a simpler settlement type variable for generating unique IDs later on.
table(AllBuildup$type)
AllBuildup$type = with(AllBuildup, ifelse(type == "Built-up Area", "BUA",
                                                     ifelse(type == "Small Settlement Area", "SSA", "HA")))

# Create 3 city classes: 
# 1) high density urban (metropoles) and 
# 2,3) all other built-up areas split into 50% above and below their median pop size (high population cities; low population cities)
# The Degree of Urbanization identifies three types of settlements:
#   1)  Cities, which have a population of at least 50,000 inhabitants in contiguous dense grid cells (>1,500 inhabitants per km2);
#   2)  Towns and semi-dense areas, which have a population of at least 5,000 inhabitants in contiguous grid cells with a density of at least 300 inhabitants per km2; and
#   3)  Rural areas, which consist mostly of low-density grid cells (2).

AllBuildup$PopDens2015 = AllBuildup$pop2020 / AllBuildup$TotArea
# Reclass largest cities to high density (Degree of Urb. level 1)
AllBuildup$POPtyp = with(AllBuildup, ifelse(pop2020>=50000 & PopDens2015 >=1500, "HDurban", # High density urban
                                            ifelse(pop2020 >=5000 & PopDens2015 >= 300, "SDurban",  # Semi-dense urban
                                                   ifelse(type=="BUA","LDbua", # Low density population, but buildings are dense according to GRID3
                                                          type)))) # If none of those, then keep the SSA or HA classification.
table(AllBuildup$POPtyp) # Chad has 6 high density urban areas (180 rows / 30 years)

SD_avg = mean(AllBuildup$pop2020[AllBuildup$POPtyp=="SDurban"]) # Mean population size of all semi-dense cities in Chad = 11k
SD_med = median(AllBuildup$pop2020[AllBuildup$POPtyp=="SDurban"]) # Median population = 8207

AllBuildup$POPtyp = with(AllBuildup, ifelse(pop2020>=SD_med & POPtyp=="SDurban", "SDtop50", 
                                            ifelse(pop2020<SD_med & POPtyp=="SDurban", "SDbot50", POPtyp)))
table(AllBuildup$POPtyp) # The new SD rows should be split evenly. (50% each of original SDurban results).


country_iso = "TCD"
savename = paste(country_iso, "growthstats_AllSettlements", sep="_")
st_write(AllBuildup, dsn = file.path(getwd(), 'UrbanGrowth.gpkg'), layer = savename, append=F)

AllCities = AllBuildup[AllBuildup$type=="BUA", ]
savename = paste(country_iso, "growthstats_AllCities", sep="_")
st_write(AllCities, dsn = file.path(getwd(), 'UrbanGrowth.gpkg'), layer = savename, append=F)





# --- 1. Total country annual speed of growth and proportion of build-up. ---

country_iso = "TCD"
savename = paste(country_iso, "growthstats_AllCities", sep="_")
AllCities = st_read(dsn = file.path(getwd(), 'UrbanGrowth.gpkg'), layer = savename)
AllCities = AllCities %>% st_drop_geometry()


TotGroA0 = AllCities %>% 
  group_by(year) %>%
  dplyr::summarise(AreaKM = sum(AreaKM, na.rm=T)) %>% # Country's total urban area (km) built in that year
  as.data.frame() %>%
  ungroup()

TotGroA0 = arrange(TotGroA0, year) # Ensure that the years are ordered sequentially.

TotGroA0$CuArea = cumsum(TotGroA0$AreaKM) # Total cumulative built area by that year.
TotArea = sum(TotGroA0$AreaKM) # Quality control: This value should be the same as CuArea for 2015.

TotGroA0 = TotGroA0 %>% 
  mutate(CuArea5y = rollsumr(AreaKM, k = 5, fill = NA))

TotGroA0 = TotGroA0 %>% 
  mutate(Gro = ((AreaKM / (CuArea - AreaKM)))) # Country's speed of growth per year for all urban land 

TotGroA0 = TotGroA0 %>% 
  mutate(Prop1y = ((AreaKM / TotArea))) # Proportion of all urban area of the country built in that year.
sum(TotGroA0$Prop1y) # Quality control: This value should be 1. (100 percent)

TotGroA0 = TotGroA0 %>% 
  mutate(Prop5y = ((CuArea5y / TotArea)))

AvgGroA0 = AllCities %>%
  group_by(year) %>% 
  dplyr::summarise(AvgGro = mean(PcGro1y, na.rm=T)) %>% # Average urban speed of growth per year, where every city has equal influence in the average.
  as.data.frame() %>%
  ungroup()

AvgCuAreaA0 = AllCities %>%
  group_by(year) %>% 
  dplyr::summarise(AvgCuArea = mean(CuArea, na.rm=T)) %>% # Average total built area each year, where every city has equal influence in the average.
  as.data.frame() %>%
  ungroup()

AvgProp1yA0 = AllCities %>%
  group_by(year) %>%
  dplyr::summarise(AvgProp1y = mean(Prop1y, na.rm=T)) %>% # Average proportion of a city area attributable to that year's build-up. Every city has equal influence in the average.
  as.data.frame() %>%
  ungroup()

AvgProp5yA0 = AllCities %>%
  group_by(year) %>%
  dplyr::summarise(AvgProp5y = mean(Prop5y, na.rm=T)) %>% # Average proportion of a city area attributable to that year's build-up. Every city has equal influence in the average.
  as.data.frame() %>%
  ungroup()

TotGroA0 = Reduce(function(x, y) merge(x, y, by="year", all=TRUE), list(TotGroA0, AvgGroA0, AvgCuAreaA0, AvgProp1yA0, AvgProp5yA0))
savename = paste(country_iso, "growth_adm0.csv", sep="_")
write.csv(TotGroA0, file = savename)
rm(AvgGroA0, AvgCuAreaA0, AvgProp1yA0, AvgProp5yA0)


# CODEBOOK: TotGroA0

# AreaKM      Country's total urban area (km) built in that year
# CuArea      Total cumulative built area by that year.
# CuArea5y    Total cumulative built area in that and the previous 4 years.
# Gro         Country's speed of growth per year for all urban land 
# Prop1y      Proportion of all urban area of the country built in that year.
# Prop5y      Proportion of all urban area of the country built in that and the previous 4 years.
# AvgGro      Average urban speed of growth per year, where every city has equal influence in the average.
# AvgProp1y   Average proportion of a city area attributable to that year's build-up. Every city has equal influence in the average.
# AvgProp5y   Average proportion of a city area attributable to that and the previous 4 years of build-up. Every city has equal influence in the average.





# --- 2. Grouped by urban type: Country-wide annual speed of growth and proportion of build-up. ---

TotGroTypA0 = AllCities %>% 
  group_by(year, POPtyp) %>%
  dplyr::summarise(AreaKM = sum(AreaKM, na.rm=T)) %>% # Urban type's total urban area (km) built in that year
  ungroup() %>%
  as.data.frame()

TotGroTypA0 = arrange(TotGroTypA0, year) # Ensure that the years are ordered sequentially.
TotGroTypA0 = TotGroTypA0 %>% 
  complete(year, POPtyp, fill=list(AreaKM=0)) # The 5 year lagging calculations require complete list of years.

TotGroTypA0 = TotGroTypA0 %>%
  group_by(POPtyp) %>%
  mutate(TotArea = sum(AreaKM)) %>% # Total area by 2015 for each urban type.
  ungroup()

TotGroTypA0 = TotGroTypA0 %>%
  group_by(POPtyp) %>%
  mutate(CuArea = cumsum(AreaKM)) %>% # Total cumulative built area by that year for each urban type.
  ungroup()

TotGroTypA0 = TotGroTypA0 %>% 
  group_by(POPtyp) %>% 
  mutate(CuArea5y = rollsumr(AreaKM, k = 5, fill = NA)) %>%
  ungroup()

TotGroTypA0 = TotGroTypA0 %>% 
  mutate(Prop5y = ((CuArea5y / TotArea)))

TotGroTypA0 = TotGroTypA0 %>% 
  group_by(POPtyp) %>%
  mutate(Gro = ((AreaKM / (CuArea - AreaKM)))) %>% # Country's speed of growth per year for all urban land of each urban type.
  ungroup()

AvgGroTypA0 = AllCities %>% 
  group_by(year, POPtyp) %>%
  dplyr::summarise(AvgGro = mean(PcGro1y, na.rm=T)) %>% # Average speed of growth per year per built area type. Every location within its class has an equal weight.
  ungroup() %>%
  as.data.frame() %>%
  complete(year, POPtyp)

AvgCuAreaTypA0 = AllCities %>% 
  group_by(year, POPtyp) %>%
  dplyr::summarise(AvgCuArea = mean(CuArea, na.rm=T)) %>% # Average total area each year by built area type. Every location within its class has an equal weight.
  ungroup() %>%
  as.data.frame() %>%
  complete(year, POPtyp)

AvgProp1yTypA0 = AllCities %>%
  group_by(year, POPtyp) %>%
  dplyr::summarise(AvgProp1y = mean(Prop1y, na.rm=T)) %>%
  ungroup() %>%
  as.data.frame() %>%
  complete(year, POPtyp)

AvgProp5yTypA0 = AllCities %>%
  group_by(year, POPtyp) %>%
  dplyr::summarise(AvgProp5y = mean(Prop5y, na.rm=T)) %>%
  ungroup() %>%
  as.data.frame() %>%
  complete(year, POPtyp)

TotGroTypA0$TypYr = paste(TotGroTypA0$POPtyp, TotGroTypA0$year, sep="_") # Create unique ID.

AvgGroTypA0$TypYr = paste(AvgGroTypA0$POPtyp, AvgGroTypA0$year, sep="_") 
AvgGroTypA0 = AvgGroTypA0 %>%
  select(-c(year, POPtyp)) # To avoid duplicates in merge

AvgCuAreaTypA0$TypYr = paste(AvgCuAreaTypA0$POPtyp, AvgCuAreaTypA0$year, sep="_") 
AvgCuAreaTypA0 = AvgCuAreaTypA0 %>%
  select(-c(year, POPtyp)) # To avoid duplicates in merge

AvgProp1yTypA0$TypYr = paste(AvgProp1yTypA0$POPtyp, AvgProp1yTypA0$year, sep="_") 
AvgProp1yTypA0 = AvgProp1yTypA0 %>%
  select(-c(year, POPtyp))

AvgProp5yTypA0$TypYr = paste(AvgProp5yTypA0$POPtyp, AvgProp5yTypA0$year, sep="_") 
AvgProp5yTypA0 = AvgProp5yTypA0 %>%
  select(-c(year, POPtyp))

TotGroTypA0 = Reduce(function(x, y) merge(x, y, by="TypYr", all=TRUE), list(TotGroTypA0, AvgGroTypA0, AvgCuAreaTypA0, AvgProp1yTypA0, AvgProp5yTypA0))
savename = paste(country_iso, "growth_UrbanType_adm0.csv", sep="_")
write.csv(TotGroTypA0, file = savename)
rm(AvgGroTypA0, AvgCuAreaTypA0, AvgProp1yTypA0, AvgProp5yTypA0)


# CODEBOOK: TotGroTypA0

# AreaKM      Country's total urban area (km) built in that year for each urban type.
# CuArea      Total cumulative built area by that year for that urban type.
# CuArea5y    Total cumulative built area in that and the previous 5 years for that urban type.
# Gro         Country's speed of growth per year for all urban land within each urban type.
# Prop1y      Proportion of all urban area of the country for that urban type built in that year.
# Prop5y      Proportion of all urban area of the country for that urban type built in that and the previous 4 years.
# AvgGro      Average urban speed of growth per year for that urban type.
# AvgProp1y   Average proportion of a city area attributable to that year's build-up for that urban type. 
# AvgProp5y   Average proportion of a city area attributable to that and the previous 4 years of build-up for that urban type. 





# --- 3. By ADM1 region, all cities. ---

TotGroA1 = AllCities %>%
  group_by(year, ADM1) %>%
  dplyr::summarise(AreaKM = sum(AreaKM, na.rm=T)) %>%
  ungroup() %>%
  as.data.frame()

TotGroA1 = arrange(TotGroA1, year)
TotGroA1 = TotGroA1 %>%
  complete(year, ADM1, fill=list(AreaKM=0)) # ADM levels are where missing years per site are more likely to happen.

TotGroA1 = TotGroA1 %>%
  group_by(ADM1) %>%
  mutate(CuArea = cumsum(AreaKM)) %>%
  ungroup()

TotGroA1 = TotGroA1 %>%
  group_by(ADM1) %>%
  mutate(TotArea = sum(AreaKM)) %>%
  ungroup()

TotGroA1 = TotGroA1 %>% 
  group_by(ADM1) %>% 
  mutate(CuArea5y = rollsumr(AreaKM, k = 5, fill = NA)) %>%
  ungroup()

TotGroA1 = TotGroA1 %>%
  group_by(ADM1) %>%
  mutate(Gro = ((AreaKM / (CuArea - AreaKM)))) %>%
  ungroup()

TotGroA1 = TotGroA1 %>% 
  mutate(Prop5y = ((CuArea5y / TotArea)))

AvgGroA1 = AllCities %>%
  group_by(year, ADM1) %>%
  dplyr::summarise(AvgGro = mean(PcGro1y, na.rm=T)) %>%
  ungroup() %>%
  as.data.frame() %>%
  complete(year, ADM1)

AvgCuAreaA1 = AllCities %>%
  group_by(year, ADM1) %>%
  dplyr::summarise(AvgCuArea = mean(CuArea, na.rm=T)) %>%
  ungroup() %>%
  as.data.frame() %>%
  complete(year, ADM1)

AvgProp1yA1 = AllCities %>%
  group_by(year, ADM1) %>%
  dplyr::summarise(AvgProp1y = mean(Prop1y, na.rm=T)) %>%
  ungroup() %>%
  as.data.frame() %>%
  complete(year, ADM1, fill=list(Prop1y=0))

AvgProp5yA1 = AllCities %>%
  group_by(year, ADM1) %>%
  dplyr::summarise(AvgProp5y = mean(Prop5y, na.rm=T)) %>% # Average proportion of a city area attributable to that year's build-up. Every city has equal influence in the average.
  ungroup() %>%
  as.data.frame() %>%
  complete(year, ADM1, fill=list(Prop5y=0))

TotGroA1$AdmYr = paste(TotGroA1$ADM1, TotGroA1$year, sep="_") # Create unique ID.
AvgGroA1$AdmYr = paste(AvgGroA1$ADM1, AvgGroA1$year, sep="_") 
AvgGroA1 = AvgGroA1 %>%
  select(-c(year, ADM1))
AvgCuAreaA1$AdmYr = paste(AvgCuAreaA1$ADM1, AvgCuAreaA1$year, sep="_") 
AvgCuAreaA1 = AvgCuAreaA1 %>%
  select(-c(year, ADM1))
AvgProp1yA1$AdmYr = paste(AvgProp1yA1$ADM1, AvgProp1yA1$year, sep="_") 
AvgProp1yA1 = AvgProp1yA1 %>%
  select(-c(year, ADM1))
AvgProp5yA1$AdmYr = paste(AvgProp5yA1$ADM1, AvgProp5yA1$year, sep="_") 
AvgProp5yA1 = AvgProp5yA1 %>%
  select(-c(year, ADM1))

TotGroA1 = Reduce(function(x, y) merge(x, y, by="AdmYr", all=TRUE), list(TotGroA1, AvgGroA1, AvgCuAreaA1, AvgProp1yA1, AvgProp5yA1))
savename = paste(country_iso, "growth_adm1.csv", sep="_")
write.csv(TotGroA1, file = savename)
rm(AvgGroA1, AvgCuAreaA1, AvgProp1yA1, AvgProp5yA1)


# CODEBOOK: TotGroA1

# AreaKM     Region's total urban area (km) built in that year.
# CuArea      Total cumulative built area by that year.
# Gro         Speed of growth per year for all urban land.
# Prop1y      Proportion of all urban area built in that year.
# AvgGro      Average urban speed of growth per year.
# AvgProp1y   Average proportion of a city area attributable to that year's build-up. 





# --- 4. By ADM1 region, by urban type. ---

TotGroTypA1 = AllCities %>% 
  group_by(year, POPtyp, ADM1) %>%
  dplyr::summarise(AreaKM = sum(AreaKM, na.rm=T)) %>% # Urban type's total urban area (km) built in that year
  ungroup() %>%
  as.data.frame()

TotGroTypA1 = arrange(TotGroTypA1, year) # Ensure that the years are ordered sequentially.
TotGroTypA1 = TotGroTypA1 %>%
  complete(year, POPtyp, ADM1, fill=list(AreaKM=0)) # Many missing combos expected at this point.

TotGroTypA1 = TotGroTypA1 %>%
  group_by(POPtyp, ADM1) %>%
  mutate(CuArea = cumsum(AreaKM)) %>% # Total cumulative built area by that year for each urban type.
  ungroup()

TotGroTypA1 = TotGroTypA1 %>%
  group_by(POPtyp, ADM1) %>%
  mutate(TotArea = sum(AreaKM)) %>% # Total area by 2015 for each urban type.
  ungroup()
  
TotGroTypA1 = TotGroTypA1 %>% 
  group_by(POPtyp, ADM1) %>%
  mutate(Gro = ((AreaKM / (CuArea - AreaKM)))) %>% # Speed of growth per year for all urban land of each urban type.
  ungroup()

AvgGroTypA1 = AllCities %>% 
  group_by(year, POPtyp, ADM1) %>%
  dplyr::summarise(AvgGro = mean(PcGro1y, na.rm=T)) %>% # Average speed of growth per year per built area type. Every location within its class has an equal weight.
  ungroup() %>%
  as.data.frame() %>%
  complete(year, POPtyp, ADM1)

AvgCuAreaTypA1 = AllCities %>% 
  group_by(year, POPtyp, ADM1) %>%
  dplyr::summarise(AvgCuArea = mean(CuArea, na.rm=T)) %>% # Average speed of growth per year per built area type. Every location within its class has an equal weight.
  ungroup() %>%
  as.data.frame() %>%
  complete(year, POPtyp, ADM1)

AvgProp1yTypA1 = AllCities %>%
  group_by(year, POPtyp, ADM1) %>%
  dplyr::summarise(AvgProp1y = mean(Prop1y, na.rm=T)) %>%
  ungroup() %>%
  as.data.frame() %>%
  complete(year, POPtyp, ADM1)

AvgProp5yTypA1 = AllCities %>%
  group_by(year, POPtyp, ADM1) %>%
  dplyr::summarise(AvgProp5y = mean(Prop5y, na.rm=T)) %>%
  ungroup() %>%
  as.data.frame() %>%
  complete(year, POPtyp, ADM1)

TotGroTypA1$TypAdmYr = paste(TotGroTypA1$POPtyp, TotGroTypA1$ADM1, TotGroTypA1$year, sep="_") # Create unique ID.
AvgGroTypA1$TypAdmYr = paste(AvgGroTypA1$POPtyp, AvgGroTypA1$ADM1, AvgGroTypA1$year, sep="_") 
AvgGroTypA1 = AvgGroTypA1 %>%
  select(-c(year, ADM1, POPtyp))
AvgCuAreaTypA1$TypAdmYr = paste(AvgCuAreaTypA1$POPtyp, AvgCuAreaTypA1$ADM1, AvgCuAreaTypA1$year, sep="_") 
AvgCuAreaTypA1 = AvgCuAreaTypA1 %>%
  select(-c(year, ADM1, POPtyp))
AvgProp1yTypA1$TypAdmYr = paste(AvgProp1yTypA1$POPtyp, AvgProp1yTypA1$ADM1, AvgProp1yTypA1$year, sep="_") 
AvgProp1yTypA1 = AvgProp1yTypA1 %>%
  select(-c(year, ADM1, POPtyp))
AvgProp5yTypA1$TypAdmYr = paste(AvgProp5yTypA1$POPtyp, AvgProp5yTypA1$ADM1, AvgProp5yTypA1$year, sep="_")
AvgProp5yTypA1 = AvgProp5yTypA1 %>%
  select(-c(year, ADM1, POPtyp))

TotGroTypA1 = Reduce(function(x, y) merge(x, y, by="TypAdmYr", all=TRUE), list(TotGroTypA1, AvgGroTypA1, AvgCuAreaTypA1, AvgProp1yTypA1, AvgProp5yTypA1))
savename = paste(country_iso, "growth_UrbanType_adm1.csv", sep="_")
write.csv(TotGroTypA1, file = savename)
rm(AvgGroTypA1, AvgCuAreaTypA1, AvgProp1yTypA1, AvgProp5yTypA1)


# CODEBOOK: TotGroTypA1

# AreaKM     Region's total urban area (km) built in that year for each urban type.
# CuArea      Total cumulative built area by that year for that urban type.
# Gro         Speed of growth per year for all urban land within each urban type.
# Prop1y      Proportion of all urban area for that urban type built in that year.
# AvgGro      Average urban speed of growth per year for that urban type.
# AvgProp1y   Average proportion of a city area attributable to that year's build-up for that urban type. 





# --- 4. Assigning WorldPop Top-down constrained population estimates 2020 ---
PopCities_adm0 = sum(AllCities$pop2020[AllCities$year==2015])

pop_type_adm0 = AllCities %>% 
  filter(year==2015) %>%
  group_by(POPtyp) %>%
  dplyr::summarise(PopCities = sum(pop2020, na.rm=T)) %>% 
  ungroup() %>%
  as.data.frame()
sum(pop_type_adm0$PopCities) # Quality control. Should equal pop20_adm0

pop_adm1 = AllCities %>% 
  filter(year==2015) %>%
  group_by(ADM1) %>%
  dplyr::summarise(PopCities = sum(pop2020, na.rm=T)) %>% 
  ungroup() %>%
  as.data.frame() %>%
  complete(ADM1, fill=list(PopCities=0))
sum(pop_adm1$PopCities)

pop_type_adm1 = AllCities %>%
  filter(year==2015) %>%
  group_by(POPtyp, ADM1) %>%
  dplyr::summarise(PopCities = sum(pop2020, na.rm=T)) %>%
  ungroup() %>%
  as.data.frame() %>%
  complete(POPtyp, ADM1, fill=list(PopCities=0))
sum(pop_type_adm1$PopCities)



# Join onto aggregate tables and save to file.
TotGroA0$PopCities = PopCities_adm0
TotGroTypA0 = merge(TotGroTypA0, pop_type_adm0, by="POPtyp", all.x=T)
TotGroA1 = merge(TotGroA1, pop_adm1, by="ADM1", all.x=T)
TotGroTypA1 = merge(TotGroTypA1, pop_type_adm1, by=c("POPtyp", "ADM1"), all.x=T)
rm(PopCities_adm0, pop_type_adm0, pop_adm1, pop_type_adm1)

savename = paste(country_iso, "growth_adm0.csv", sep="_")
write.csv(TotGroA0, file = savename)
savename = paste(country_iso, "growth_UrbanType_adm0.csv", sep="_")
write.csv(TotGroTypA0, file = savename)
savename = paste(country_iso, "growth_adm1.csv", sep="_")
write.csv(TotGroA1, file = savename)
savename = paste(country_iso, "growth_UrbanType_adm1.csv", sep="_")
write.csv(TotGroTypA1, file = savename)



