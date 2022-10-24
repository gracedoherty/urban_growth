# This script contains basic calculations of urban growth and building construction from World Settlement Footprint.


## PREPARE WORKSPACE -------------------------- 
setwd("C:/Users/grace/GIS/povertyequity/urban_growth")
wd = getwd()
#install.packages("stringr")
Packages = c("ursa", "raster", "sp", "rgdal", "sf", "dplyr", "rgeos", "naniar", # replace_with_na()
             "stringr", # To extract the last x characters of a string
             "zoo", # rollsum()
             "tidyverse", # ggplot2()
             "tidyr", # complete()
             "viridis", "ggnewscale", # The ggnewscale::new_scale_colour() command acts as an instruction to ggplot2 to initialise a new colour scale: scale and guide commands that appear above the new_scale_colour() command will be applied to the first colour scale, and commands that appear below are applied to the second colour scale.
             "svglite") # ggsave()
lapply(Packages, library, character.only = TRUE)



## LOAD DATA ----------------------------- 

# Yearly built up area extents from 2000 - 2015: World Settlement Footprint Evolution (DLR).
# Yearly population from 2000-2015: WorldPop UN-adjusted unconstrained estimates, 100m (WorldPop).
# The extents of each city boundary are based on whichever GRID3 settlement extent boundary they intersect with or are nearest to.
AllBuildup = st_read("CMN_unclean_temp.gdb", layer = "CMN_AllYears_except2010")
AllBuildup$featureID = 1:nrow(AllBuildup) # There wasn't an index. Create one and save back to file to use as merge ID later on when re-georeferencing.
st_write(AllBuildup, "CMN_unclean_temp.gpkg", layer="CMN_AllYears_except2010", append=F)
AllBuildup = AllBuildup %>% 
  st_drop_geometry() %>%
  rename(settlementID = GRID3_splitID) %>%
  select(-c(OBJECTID, Shape_Length, Shape_Area))

# No spatial functions in this script, so we can drop geometry to make the df smaller.
# Table join onto the feature layer by WSFE_ID to map it again.




## PREPARE DATASET ----------------------------- 

### Replace all NAs in the growth indicators with 0. ------------------------------
AllBuildup = AllBuildup %>% mutate_at(c(2:32), ~replace(., is.na(.), 0))



## SUMMARY STATISTICS, ALL SETTLEMENTS ----------------------------- 

### Proportion of the city built in each year. -----------------------
AllBuildup <- AllBuildup %>%
  mutate(LatestArea = Area15) %>%
  pivot_longer(cols = c(starts_with("Area")), # The columns which will be exploded into separate rows for each year
               names_to = "Year", # The name of the new column
               names_prefix = c("Area"), # The values in the names_to column will exclude the characters specified here, containing only the characters that follow it.
               values_to = "Area") %>% # This is the name of the column containing the original values in the columns selected by the cols= parameter.
  mutate(PropArea = Area/LatestArea) %>%
  pivot_wider(names_from = "Year",
              names_sep = "",
              values_from = c("Area", "PropArea"))

### Percent of 2015 population residing in settlement that year. -----------------
AllBuildup <- AllBuildup %>%
  mutate(LatestPop = PopSum15) %>%
  pivot_longer(cols = c(starts_with("PopSum")),
               names_to = "Year",
               names_prefix = c("PopSum"),
               values_to = "PopSum") %>%
  mutate(PropPop = PopSum/LatestPop) %>%
  pivot_wider(names_from = "Year",
              names_sep = "",
              values_from = c("PopSum", "PropPop"))


### Percent increase in area compared to previous year. -----------------
AllBuildup = AllBuildup %>%
  pivot_longer(cols = c(starts_with("Area")), 
               names_to = "Year", 
               names_prefix = c("Area"),
               values_to = "Area") %>%
  arrange(Year) %>%
  group_by(settlementID) %>%
  mutate(Gro1Yr_Area = (Area - lag(Area)) / lag(Area)) %>%
  ungroup() %>%
  pivot_wider(names_from = "Year",
              names_sep = "",
              values_from = c("Area", "Gro1Yr_Area"))


### Percent change (positive = increase) in population compared to previous year. -----------------
AllBuildup = AllBuildup %>%
  pivot_longer(cols = c(starts_with("PopSum")), 
               names_to = "Year", 
               names_prefix = c("PopSum"),
               values_to = "PopSum") %>%
  arrange(Year) %>%
  group_by(settlementID) %>%
  mutate(Gro1Yr_Pop = (PopSum - lag(PopSum)) / lag(PopSum)) %>%
  ungroup() %>%
  pivot_wider(names_from = "Year",
              names_sep = "",
              values_from = c("PopSum", "Gro1Yr_Pop"))


### Population density each year. -----------------
# Was struggling with the pivot table where two variables in the calculation need to share the same year. For loop instead.
YearRange = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
              "11", "12", "13", "14", "15")

AllBuildup[, paste("PopDens0", 0:9, sep="")] = 1 # Have to split into 2 lines of code because we have leading zeroes for 2000-2009.
AllBuildup[, paste("PopDens", 11:15, sep="")] = 1

for (year in YearRange) {
  AllBuildup[, paste("PopDens", year, sep="")] = AllBuildup[, paste("PopSum", year, sep="")] / (AllBuildup[, paste("Area", year, sep="")] / 1000000)
}



## CREATE CITY CLASSES ----------------------------- 
# 1) high density urban (metropoles) and 
# 2,3) all other built-up areas split into 50% above and below their median pop size (high population cities; low population cities)
# The Degree of Urbanization identifies three types of settlements:
#   1)  Cities, which have a population of at least 50,000 inhabitants in contiguous dense grid cells (>1,500 inhabitants per km2);
#   2)  Towns and semi-dense areas, which have a population of at least 5,000 inhabitants in contiguous grid cells with a density of at least 300 inhabitants per km2; and
#   3)  Rural areas, which consist mostly of low-density grid cells (2).


### Reclass largest cities to high density (Degree of Urb. level 1) and others to semi-dense (level 2) -------------------
AllBuildup[, paste("UrbType0", 0:9, sep="")] = "placeholder"
AllBuildup[, paste("UrbType", 11:15, sep="")] = "placeholder"

for (year in YearRange) {
  PopItem = paste("PopSum", year, sep="")
  DensItem = paste("PopDens", year, sep="")
  AllBuildup[, paste("UrbType", year, sep="")] = 
    ifelse(AllBuildup[, PopItem] >= 50000 & AllBuildup[, DensItem] >= 1500, "HDurban", # High density urban
           ifelse(AllBuildup[, PopItem] >= 5000 & AllBuildup[, DensItem] >= 300, "SDurban",
                  "LD"))
}


### Save intermediate file. -------------------------
Geom = st_read("CMN_unclean_temp.gpkg", layer="CMN_AllYears_except2010")
Geom = Geom %>% select(c(featureID, geom))
AllBuildupGeom = left_join(Geom, AllBuildup)
# country_iso = "CMN"
# savename = paste(country_iso, "growthstats", sep="_")
# st_write(AllBuildup, dsn = file.path(getwd(), 'UrbanizationStudy.gpkg'), layer = paste(savename, "table", sep="_"), append=F)
# st_write(AllBuildupGeom, dsn = file.path(getwd(), 'UrbanizationStudy.gpkg'), layer = savename, append=F)
st_write(AllBuildup, "CMN_unclean_temp.gpkg", layer="CMN_growthstats_table", append=F)
st_write(AllBuildupGeom, "CMN_unclean_temp.gpkg", layer="CMN_growthstats", append=F)
rm(Geom, AllBuildupGeom)



## SUMMARY STATISTICS, BY ADMIN AREA ---------------------
YearRange = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
              "11", "12", "13", "14", "15")
country_iso = "CMN"

# Reload settlements file if starting from here.
# country_iso = "CMN"
# savename = paste(country_iso, "growthstats_table", sep="_")
# AllCities = st_read(dsn = file.path(getwd(), 'UrbanizationStudy.gpkg'), layer = savename)
AllCities = st_read(dsn = file.path(getwd(), 'CMN_unclean_temp.gpkg'), layer = "CMN_growthstats")


# Load admin areas if not already on settlements file, and spatial join to settlements.
#ADM = st_read()



# Don't need geometry info from here out.
AllCities = AllCities %>% st_drop_geometry()



### Country-level summaries (ADM0), not grouped. --------------------

#### Total built area per year. -----------
TotGroA0 = data.frame(matrix(ncol = 0, nrow = 1)) # Create an empty dataframe. Number of rows corresponds with number of admin area features.
for (year in YearRange) {
  Item = paste("Area", year, sep="")
  TotGroA0[, paste("AreaKM", year, sep="")] = sum(AllCities[, Item], na.rm=T)/1000000
}

#### Total annual speed of growth for built area. -----------
names(TotGroA0)[1] # Printing the name of the variable to double-check we're using the right figures in the for loop.
names(TotGroA0)[15]
for(i in 2:15) { # For this one, we have to start with column #2 because the calculation will fail with Area00 (Area99 doesn't exist: (Area00 - Area99)/Area99)
  Year = gsub(toString("[^0-9.-]"), "", names(TotGroA0)[i]) # Get the year from the column name (as string, to include the leading zeroes).
  NewVariable = paste("Gro1Yr_Area", Year, sep="")
  TotGroA0[, NewVariable] = (TotGroA0[,i] - TotGroA0[,i-1]) / TotGroA0[,i-1]
}

#### Proportion of 2015 built area built BY that year. (cumulative) ---------------
for(i in 1:15) { 
  Year = gsub(toString("[^0-9.-]"), "", names(TotGroA0)[i]) 
  NewVariable = paste("Prop_CuArea", Year, sep="")
  TotGroA0[, NewVariable] = TotGroA0[,i] / TotGroA0[, "AreaKM15"]
}

#### Proportion of 2015 built area built IN that year. (isolating that year's activity) ---------------
for(i in 2:15) { 
  Year = gsub(toString("[^0-9.-]"), "", names(TotGroA0)[i]) 
  NewVariable = paste("Prop_YrArea", Year, sep="")
  TotGroA0[, NewVariable] = (TotGroA0[,i] - TotGroA0[,i-1]) / TotGroA0[, "AreaKM15"]
}

#### Total population accounted for in observable settlements in that year. -----------
for (year in YearRange) {
  Item = paste("PopSum", year, sep="")
  TotGroA0[, paste("PopSum", year, sep="")] = sum(AllCities[, Item], na.rm=T)
}

#### Total annual speed of growth for population. -----------
names(TotGroA0)[59] # Printing the name of the variable to double-check we're using the right figures in the for loop.
names(TotGroA0)[73]
for(i in 60:73) { # Remember to start on the second year in the list (PopSum01) like we did for built area growth.
  Year = gsub(toString("[^0-9.-]"), "", names(TotGroA0)[i]) 
  NewVariable = paste("Gro1Yr_Pop", Year, sep="")
  TotGroA0[, NewVariable] = (TotGroA0[,i] - TotGroA0[,i-1]) / TotGroA0[,i-1]
}

#### Proportion of 2015 population BY that year. (cumulative) ---------------
for(i in 59:73) { 
  Year = gsub(toString("[^0-9.-]"), "", names(TotGroA0)[i]) 
  NewVariable = paste("Prop_CuPop", Year, sep="")
  TotGroA0[, NewVariable] = TotGroA0[,i] / TotGroA0[, "PopSum15"]
}

#### Proportion of 2015 population which moved to the country's settlements IN that year. (isolating that year's activity) (positive = net growth in pop) ---------------
for(i in 59:73) { 
  Year = gsub(toString("[^0-9.-]"), "", names(TotGroA0)[i]) 
  NewVariable = paste("Prop_YrPop", Year, sep="")
  TotGroA0[, NewVariable] = (TotGroA0[,i] - TotGroA0[,i-1]) / TotGroA0[, "PopSum15"]
}


#### Average speed of built area growth per year, where every settlement has equal influence in the average. -----------------
AvgGroA0 = data.frame(matrix(ncol = 0, nrow = 1)) 
for (year in YearRange) {
  Item = paste("Gro1Yr_Area", year, sep="")
  AvgGroA0[, paste("AvgGro1Yr_Area", year, sep="")] = mean(is.finite(AllCities[, Item]), na.rm=T) # is.finite() removes Inf and NA values.
}

#### Average proportion of 2015 area attributable to that year's buildup, where every settlement has equal influence in the average. -----------------
for (year in YearRange) {
  Item = paste("PropArea", year, sep="")
  AvgGroA0[, paste("AvgProp_Area", year, sep="")] = mean(is.finite(AllCities[, Item]), na.rm=T) # is.finite() removes Inf and NA values.
}


TotGroA0 = Reduce(function(x, y) merge(x, y, by.x=0, by.y=0, all=TRUE), list(TotGroA0, AvgGroA0))
savename = paste(country_iso, "growthstats_adm0.csv", sep="_")
write.csv(TotGroA0, file = savename)
rm(TotGroA0, AvgGroA0)





## Country-level summaries (ADM0), grouped by degree of urbanization as of 2015. ------------------

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
  mutate(TotArea = sum(AreaKM, na.rm=T)) %>% # Total area by 2015 for each urban type.
  ungroup()

TotGroTypA0 = TotGroTypA0 %>%
  group_by(POPtyp) %>%
  mutate(CuArea = cumsum(ifelse(is.na(AreaKM), 0, AreaKM)) + AreaKM*0) %>% # Total cumulative built area by that year for each urban type.
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
  mutate(CuArea = cumsum(ifelse(is.na(AreaKM), 0, AreaKM)) + AreaKM*0) %>%
  ungroup()

TotGroA1 = TotGroA1 %>%
  group_by(ADM1) %>%
  mutate(TotArea = sum(AreaKM, na.rm=T)) %>%
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
  mutate(CuArea = cumsum(ifelse(is.na(AreaKM), 0, AreaKM)) + AreaKM*0) %>% # Total cumulative built area by that year for each urban type.
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
PopCities_adm0 = sum(AllCities$pop2020[AllCities$year==2015], na.rm=T)

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



# Re-save AllCities file with geometry
Cities_geom = st_read("WSFE_MLI_GRID3/WSFE_MLI_GRID3.shp")
Cities_geom = select(Cities_geom, c(WSFE_ID, geometry))
AllCities = merge(Cities_geom, AllCities, by="WSFE_ID", all.x=F, all.y=T)
savename = paste(country_iso, "growthstats_AllCities", sep="_")
st_write(AllCities, dsn = file.path(getwd(), 'UrbanGrowth.gpkg'), layer = savename, append=F)

