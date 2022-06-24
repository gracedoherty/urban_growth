# This script contains basic plotting of urban growth and building construction from World Settlement Footprint.
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
library(ggnewscale) # The ggnewscale::new_scale_colour() command acts as an instruction to ggplot2 to initialise a new colour scale: scale and guide commands that appear above the new_scale_colour() command will be applied to the first colour scale, and commands that appear below are applied to the second colour scale.
library(svglite) # ggsave()
library(showtext) # To expand font options.
library(scales) # To change axes to integer (accuracy=1)



# ------------- LOAD AND PREP DATA ----------------

WSFE_type_adm0_stats = read.csv("TCD_growth_SettlementType_adm0.csv")
WSFE_type_adm1_stats = read.csv("TCD_growth_SettlementType_adm1.csv")
# WSFE_type_adm2_stats = read.csv("TCD_growth_SettlementType_adm2.csv")
# WSFE_type_adm3_stats = read.csv("TCD_growth_SettlementType_adm3.csv")
growth_unw_adm0 = read.csv("TCD_growth_adm0.csv")
growth_unw_adm1 = read.csv("TCD_growth_adm1.csv")
# growth_unw_adm2 = read.csv("TCD_growth_adm2.csv")
# growth_unw_adm3 = read.csv("TCD_growth_adm3.csv")

AllBuildup <-  st_read('WSFE_growthstats_grid3_v2/WSFE_TCD_growthstats2.shp')
AllBuildup <- AllBuildup %>% st_drop_geometry()
# AllBuildup = casestudies
# rm(casestudies)

# --- Prep datasets for visuals ---

# Add name of case study city as new variable field for easy plotting.
AllBuildup$City <- NA
AllCities = AllBuildup[AllBuildup$type == "Built-up Area", ]

# Case study cities:
# BFA 
# country_iso = "BFA"
# country_name = "Burkina Faso"
# AllCities <- within(AllCities, City[NEAR_FID == 354] <- 'Ouagadougou')
# AllCities <- within(AllCities, City[NEAR_FID == 332] <- 'Bobo-Dioulasso')
# AllCities <- within(AllCities, City[NEAR_FID == 347] <- 'Koudougou')
# AllCities <- within(AllCities, City[NEAR_FID == 180] <- 'Madiagdune')
# AllCities <- within(AllCities, City[NEAR_FID == 303] <- 'Kaya')
# CAF
# AllCities <- within(AllCities, City[NEAR_FID == 10028] <- 'Bangui')
# AllCities <- within(AllCities, City[NEAR_FID == 25878] <- 'Bambari')
# AllCities <- within(AllCities, City[NEAR_FID == 929] <- 'Berberati')
# CMN
# country_iso = "CMN"
# country_name = "Cameroon"
# AllCities <- within(AllCities, City[NEAR_FID == 18] <- 'Douala')
# AllCities <- within(AllCities, City[NEAR_FID == 280] <- 'Yaounde')
# AllCities <- within(AllCities, City[NEAR_FID == 266] <- 'Bamenda')
# COG
# AllCities <- within(AllCities, City[NEAR_FID == 8605 <- 'Brazzaville')
# AllCities <- within(AllCities, City[NEAR_FID == 439] <- 'Pointe-Noire')
# AllCities <- within(AllCities, City[NEAR_FID == 14780] <- 'Ouesso')
# GAB
# AllCities <- within(AllCities, City[NEAR_FID == 12] <- 'Libreville')
# AllCities <- within(AllCities, City[NEAR_FID == 18] <- 'Port-Gentil')
# AllCities <- within(AllCities, City[NEAR_FID == 6] <- 'Franceville')
# MLI
# country_iso = "MLI"
# country_name = "Mali"
# AllCities <- within(AllCities, City[NEAR_FID == 97] <- 'Bamako')
# AllCities <- within(AllCities, City[NEAR_FID == 313] <- 'Gao')
# AllCities <- within(AllCities, City[NEAR_FID == 80] <- 'Sikasso')
# AllCities <- within(AllCities, City[NEAR_FID == 184] <- 'Kayes')
# NER
# AllCities <- within(AllCities, City[NEAR_FID == 370] <- 'Niamey')
# AllCities <- within(AllCities, City[NEAR_FID == 161] <- 'Maradi')
# AllCities <- within(AllCities, City[NEAR_FID == 324] <- 'Zinder')
# AllCities <- within(AllCities, City[NEAR_FID == 341] <- 'Diffa')
# TCD
country_iso = "TCD"
country_name = "Chad"
AllCities <- within(AllCities, City[NEAR_FID == 131507] <- 'NDjamena')
AllCities <- within(AllCities, City[NEAR_FID == 278172] <- 'Abeche')
AllCities <- within(AllCities, City[NEAR_FID == 333078] <- 'Faya')
AllCities <- within(AllCities, City[NEAR_FID == 234510] <- 'Bol')
AllCities <- within(AllCities, City[NEAR_FID == 4096] <- 'Moundou')
AllCities <- within(AllCities, City[NEAR_FID == 42500] <- 'Sarh')


# Narrow down to only case studies
casestudy <- AllCities %>%
  filter(!is.na(City))
casestudy86_15 = AllCities %>%
  filter(!is.na(City)) %>%
  filter(year != 1985)
casestudy05_15 = AllCities %>%
  filter(!is.na(City)) %>%
  filter(year > 2004)

# Subset of 5 year increments
casestudy_every5 <- casestudy %>% 
  filter_at(vars(year), 
            any_vars(. %in% c('1985', '1990', '1995', '2000', '2005', '2010', '2015'))) 

# Some of the variable names will need to be changed to overlay onto city plots.
WSFE_type_adm0_stats$pcGRO_1y = WSFE_type_adm0_stats$GRO_w
WSFE_type_adm0_stats$cuArea1y = WSFE_type_adm0_stats$av_cuArea
WSFE_type_adm1_stats$cuArea1y = WSFE_type_adm1_stats$av_cuArea
# WSFE_type_adm2_stats$cuArea1y = WSFE_type_adm2_stats$av_cuArea
# WSFE_type_adm3_stats$cuArea1y = WSFE_type_adm3_stats$av_cuArea

# Narrow down to only Built-Up Areas, and only Settlement Areas.
Major = WSFE_type_adm0_stats %>%
  filter(typ=='bua_large')
Major86_15 = Major %>%
  filter(year != 1985)
Major05_15 = Major %>%
  filter(year > 2004)
Minor = WSFE_type_adm0_stats %>%
  filter(typ=='bua_small')
Minor86_15 = Minor %>%
  filter(year != 1985)
Minor05_15 = Minor %>%
  filter(year > 2004)



# ------------- VISUALIZATION ----------------
# --- Prepare design workspace ---

# Check available fonts
windowsFonts() # If limited, load desired font from computer's font library with showtext package.
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
CITY_LOESS = geom_smooth(aes(color=City), alpha = 1, lwd = 1.25, se=F)

BUAL_AREA = geom_area(data=Major, aes(color="Highest populated cities"), fill = "#E69F00", alpha = .3, lwd = 0) # Create filled area for all BUAs in country
BUAL_LOESS = geom_smooth(data=Major, aes(color="Highest populated cities"), lwd = 1.25, se=F, lty=5)

BUAL_86_15_AREA <- geom_area(data = Major86_15, aes(color="Highest populated cities"), fill = "#E69F00", alpha = .3, lwd = 0)
BUAL_86_15_LOESS = geom_smooth(data=Major86_15, aes(color="Highest populated cities"), lwd = 1.25, se=F, lty=5)

BUAL_05_15_AREA <- geom_area(data = Major05_15, aes(color="Highest populated cities"), fill = "#E69F00", alpha = .3, lwd = 0)
BUAL_05_15_LOESS = geom_smooth(data=Major05_15, aes(color="Highest populated cities"), lwd = 1.25, se=F, lty=5)

BUAS_AREA = geom_area(data = Minor, aes(color="Lowest populated cities"), fill = "#E69F00", alpha = .3, lwd = 0) # Create filled area for all SSAs in country
BUAS_LOESS = geom_smooth(data=Minor, aes(color="Lowest populated cities"), lwd = 1.25, se=F, lty=5)

BUAS_86_15_AREA <- geom_area(data = Minor86_15, aes(color="Lowest populated cities"), fill = "#E69F00", alpha = .3, lwd = 0)
BUAS_86_15_LOESS = geom_smooth(data=Minor86_15, aes(color="Lowest populated cities"), lwd = 1.25, se=F, lty=5)

BUAS_05_15_AREA <- geom_area(data = Minor05_15, aes(color="Lowest populated cities"), fill = "#E69F00", alpha = .3, lwd = 0)
BUAS_05_15_LOESS = geom_smooth(data=Minor05_15, aes(color="Lowest populated cities"), lwd = 1.25, se=F, lty=5)

transparency =   theme(
  panel.background = element_rect(fill='transparent'), #transparent panel bg
  plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
  panel.grid.major = element_blank(), #remove major gridlines
  panel.grid.minor = element_blank(), #remove minor gridlines
  legend.background = element_rect(fill='transparent', colour='transparent'), #transparent legend bg
  legend.box.background = element_rect(fill='transparent', colour='transparent') #transparent legend panel
)




# --- Chart: Cumulative area ---
casestudy %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  CITY_LOESS +
  new_scale_color() +
  BUAL_LOESS + BUAS_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = country_name, subtitle ="Cumulative area of the city, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_casestudies_adm0.png", sep="_")
ggsave(plotsavename)

Major %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  BUAL_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = country_name, subtitle ="Average cumulative area of major and small cities, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_MajorCities_adm0_notitle.png", sep="_")
ggsave(plotsavename)

Minor %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  new_scale_color() +
  BUAL_LOESS + labs(color="Country-wide") +
  BUAS_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = country_name, subtitle ="Average cumulative area of major and small cities, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_AllBUAs_adm0_notitle.png", sep="_")
ggsave(plotsavename)



casestudy %>%
  filter(City=="NDjamena") %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  CITY_LOESS + labs(color=" ") +
  new_scale_color() +
  BUAL_LOESS + BUAS_LOESS +labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: N'Djamena", subtitle ="Cumulative area of the city, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_NDJ_adm0.png", sep="_")
ggsave(plotsavename)

casestudy05_15 %>%
  filter(City=="NDjamena") %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  CITY_LOESS + labs(color=" ") +
  new_scale_color() +
  BUAL_05_15_LOESS + BUAS_05_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: N'Djamena", subtitle ="Cumulative area of the city, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_NDJ_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

casestudy %>%
  filter(City=="Faya") %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  CITY_LOESS + labs(color=" ") +
  new_scale_color() +
  BUAL_LOESS + BUAS_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Faya", subtitle ="Cumulative area of the city, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Faya_adm0.png", sep="_")
ggsave(plotsavename)

casestudy05_15 %>%
  filter(City=="Faya") %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  CITY_LOESS + labs(color=" ") +
  new_scale_color() +
  BUAL_05_15_LOESS + BUAS_05_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Faya", subtitle ="Cumulative area of the city, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Faya_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

casestudy %>%
  filter(City=="Bol") %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  CITY_LOESS + labs(color=" ") +
  new_scale_color() +
  BUAS_LOESS + BUAL_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Bol", subtitle ="Cumulative area of the city, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Bol_adm0.png", sep="_")
ggsave(plotsavename)

casestudy05_15 %>%
  filter(City=="Bol") %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  CITY_LOESS + labs(color=" ") +
  new_scale_color() +
  BUAL_05_15_LOESS + BUAS_05_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Bol", subtitle ="Cumulative area of the city, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Bol_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

casestudy %>%
  filter(City=="Abeche") %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  CITY_LOESS + labs(color=" ") +
  new_scale_color() +
  BUAL_LOESS + BUAS_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Abeche", subtitle ="Cumulative area of the city, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Abeche_adm0.png", sep="_")
ggsave(plotsavename)

casestudy05_15 %>%
  filter(City=="Abeche") %>%
  ggplot(aes(x = year, y = cuArea1y)) +
  CITY_LOESS + labs(color=" ") +
  new_scale_color() +
  BUAL_05_15_LOESS + BUAS_05_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Abeche", subtitle ="Cumulative area of the city, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Abeche_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)


# --- Chart: Proportion of area ---
casestudy86_15 %>%
  ggplot(aes(x = year, y = prop1y)) +
  CITY_LOESS + 
  new_scale_color() +
  BUAL_86_15_LOESS + BUAS_86_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = country_name, subtitle ="Proportion of total city area built each year, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Proportion of built-up area (%)")))
plotsavename = paste(country_iso, "proportion_casestudies_adm0.png", sep="_")
ggsave(plotsavename)

Minor86_15 %>%
  ggplot(aes(x = year, y = prop1y)) +
  new_scale_color() +
  BUAL_86_15_LOESS + labs(color="Country-wide") +
  BUAS_86_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = country_name, subtitle ="Proportion of the total urban area built each year, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Proportion of built-up area (%)")))
plotsavename = paste(country_iso, "proportion_AllBUAs_adm0.png", sep="_")
ggsave(plotsavename)

casestudy05_15 %>%
  ggplot(aes(x = year, y = prop1y)) +
  CITY_LOESS + 
  new_scale_color() +
  BUAL_05_15_LOESS + BUAS_05_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = country_name, subtitle ="Proportion of total city area built each year, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Proportion of built-up area (%)")))
plotsavename = paste(country_iso, "proportion_casestudies_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

Major05_15 %>%
  ggplot(aes(x = year, y = prop1y)) +
  BUAL_05_15_LOESS + BUAS_05_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = country_name, subtitle ="Proportion of the total city area built each year, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Proportion of built-up area (%)")))
plotsavename = paste(country_iso, "proportion_AllBUAs_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)



# --- Chart: Growth rate ---
casestudy86_15 %>%
  ggplot(aes(x = year, y = pcGRO_1y)) +
  CITY_LOESS +
  new_scale_color() +
  BUAL_86_15_LOESS + BUAS_86_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = country_name, subtitle ="Speed of built-up area growth, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year (%)")))
plotsavename = paste(country_iso, "speedofgrowth_casestudies_adm0.png", sep="_")
ggsave(plotsavename)

Minor86_15 %>%
  ggplot(aes(x = year, y = pcGRO_1y)) +
  new_scale_color() +
  BUAL_86_15_LOESS + labs(color="Country-wide") +
  BUAS_86_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = country_name, subtitle ="Percent yearly built-up area growth, 1986-2015", 
       x = "Year", y = expression(paste("Growth in buildup from previous year (%)")))
plotsavename = paste(country_iso, "speedofgrowth_AllBUAs_adm0.png", sep="_")
ggsave(plotsavename)

casestudy05_15 %>%
  ggplot(aes(x = year, y = pcGRO_1y)) +
  CITY_LOESS +
  new_scale_color() +
  BUAL_05_15_LOESS + BUAS_05_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = country_name, subtitle ="Speed of built-up area growth, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year (%)")))
plotsavename = paste(country_iso, "speedofgrowth_casestudies_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

Minor05_15 %>%
  ggplot(aes(x = year, y = pcGRO_1y)) +
  new_scale_color() +
  BUAL_05_15_LOESS + labs(color="Country-wide") +
  BUAS_05_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = country_name, subtitle ="Percent yearly built-up area growth, 2005-2015", 
       x = "Year", y = expression(paste("Growth in buildup from previous year (%)")))
plotsavename = paste(country_iso, "speedofgrowth_AllBUAs_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

casestudy86_15 %>%
  filter(City=="Faya") %>%
  ggplot(aes(x = year, y = pcGRO_1y)) +
  CITY_LOESS +
  new_scale_color() +
  BUAL_86_15_LOESS + BUAS_86_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Faya", subtitle ="Speed of built-up area growth, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year (%)")))
plotsavename = paste(country_iso, "speedofgrowth_Faya_adm0.png", sep="_")
ggsave(plotsavename)

casestudy05_15 %>%
  filter(City=="Faya") %>%
  ggplot(aes(x = year, y = pcGRO_1y)) +
  CITY_LOESS +
  new_scale_color() +
  BUAL_05_15_LOESS + BUAS_05_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Faya", subtitle ="Speed of built-up area growth, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year (%)")))
plotsavename = paste(country_iso, "speedofgrowth_Faya_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

casestudy86_15 %>%
  filter(City=="NDjamena") %>%
  ggplot(aes(x = year, y = pcGRO_1y)) +
  CITY_LOESS +
  new_scale_color() +
  BUAL_86_15_LOESS + BUAS_86_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: NDjamena", subtitle ="Speed of built-up area growth, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year (%)")))
plotsavename = paste(country_iso, "speedofgrowth_NDjamena_adm0.png", sep="_")
ggsave(plotsavename)

casestudy05_15 %>%
  filter(City=="NDjamena") %>%
  ggplot(aes(x = year, y = pcGRO_1y)) +
  CITY_LOESS +
  new_scale_color() +
  BUAL_05_15_LOESS + BUAS_05_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: NDjamena", subtitle ="Speed of built-up area growth, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year (%)")))
plotsavename = paste(country_iso, "speedofgrowth_NDjamena_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)


casestudy86_15 %>%
  filter(City=="Bol") %>%
  ggplot(aes(x = year, y = pcGRO_1y)) +
  CITY_LOESS +
  new_scale_color() +
  BUAL_86_15_LOESS + BUAS_86_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Bol", subtitle ="Speed of built-up area growth, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year (%)")))
plotsavename = paste(country_iso, "speedofgrowth_Bol_adm0.png", sep="_")
ggsave(plotsavename)

casestudy05_15 %>%
  filter(City=="Bol") %>%
  ggplot(aes(x = year, y = pcGRO_1y)) +
  CITY_LOESS +
  new_scale_color() +
  BUAL_05_15_LOESS + BUAS_05_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Bol", subtitle ="Speed of built-up area growth, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year (%)")))
plotsavename = paste(country_iso, "speedofgrowth_Bol_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

casestudy86_15 %>%
  filter(City=="Abeche") %>%
  ggplot(aes(x = year, y = pcGRO_1y)) +
  CITY_LOESS +
  new_scale_color() +
  BUAL_86_15_LOESS + BUAS_86_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Abeche", subtitle ="Speed of built-up area growth, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year (%)")))
plotsavename = paste(country_iso, "speedofgrowth_Abeche_adm0.png", sep="_")
ggsave(plotsavename)

casestudy05_15 %>%
  filter(City=="Abeche") %>%
  ggplot(aes(x = year, y = pcGRO_1y)) +
  CITY_LOESS +
  new_scale_color() +
  BUAL_05_15_LOESS + BUAS_05_15_LOESS + labs(color="Country-wide") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Abeche", subtitle ="Speed of built-up area growth, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year (%)")))
plotsavename = paste(country_iso, "speedofgrowth_Abeche_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)



# --- Chart: Proportion of area, 5 year increments ---
casestudy_every5 %>%
  filter(year != 1985) %>%
  ggplot(aes(x = City, y = prop5y, fill = year)) +
  THEME_BARLOW +
  geom_bar(stat='identity') +
  scale_fill_gradient(low="black", high="#EE6C4D") + 
  transparency +
  labs(title = country_name, subtitle ="Proportion of new build-up, 1986-2015", 
       x = "", y = expression(paste("Proportion of post-1985 built-up area (%)")))
plotsavename = paste(country_iso, "proportion_every5_casestudies.png", sep="_")
ggsave(plotsavename)



# ------------- OTHER ----------------

# Function for labeling the cumulative area by 2015.
max_cuArea1y <- function(city_max){
  points <- data.frame(xpt = rep(NA,length(city_max)),
                       ypt = rep(NA, length(city_max)))
  for(i in 1:length(city_max)){
    points$ypt[i] <- max(casestudy$cuArea1y[casestudy$City == city_max[i] & casestudy$year!=1985],na.rm = T)
    check <- casestudy$year[casestudy$City == city_max[i] & casestudy$cuArea1y == points$ypt[i]]
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
    points$ypt[i] <- max(casestudy$prop1y[casestudy$City == city_max[i] & casestudy$year!=1985],na.rm = T)
    check <- casestudy$year[casestudy$City == city_max[i] & casestudy$prop1y == points$ypt[i]]
    points$xpt[i] <- max(check, na.rm = T)
  }
  list(geom_segment(data = points, aes(x=xpt,y=ypt,xend=xpt+3,yend=ypt), inherit.aes = F, color="#6c757d"),
       annotate(geom = "text", x = points$xpt+4, y=points$ypt, label =points$xpt, fontface="italic", color="#6c757d"))
}



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


