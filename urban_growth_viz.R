# This script contains basic plotting of urban growth and building construction from World Settlement Footprint.
# See end of script for variable codebook.


## PREPARE WORKSPACE ---------------------------------------
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
library(forcats)
library(stringr)
library(scales)


## LOAD AND PREP DATA ---------------------------------------

country_iso = "BFA"
savename = paste(country_iso, "growthstats_AllCities", sep="_")
AllCities = st_read(dsn = file.path(getwd(), 'UrbanGrowth.gpkg'), layer = savename)
AllCities = AllCities %>% st_drop_geometry()

savename = paste(country_iso, "growth_adm0.csv", sep="_")
TotGroA0 = read.csv(savename)
savename = paste(country_iso, "growth_UrbanType_adm0.csv", sep="_")
TotGroTypA0 = read.csv(savename)
savename = paste(country_iso, "growth_adm1.csv", sep="_")
TotGroA1 = read.csv(savename)
savename = paste(country_iso, "growth_UrbanType_adm1.csv", sep="_")
TotGroTypA1 = read.csv(savename)
# savename = "Mines_SSA_HA_growth_UrbanType_adm0.csv"
# MinesGroTypA0 = read.csv(savename)
# country_name = "Within 5 hours of a mine"
# country_iso = "Mines_SSA_HA"


### Country and city organization ---------------------------------------

# Add name of case study city as new variable field for easy plotting.
AllCities$City <- NA

# Case study cities:
# BFA 
# country_iso = "BFA"
# country_name = "Burkina Faso"
AllCities <- within(AllCities, City[GRID3_ID == 227047] <- 'Ouagadougou')
AllCities <- within(AllCities, City[GRID3_ID ==  51196] <- 'Bobo-Dioulasso')
AllCities <- within(AllCities, City[GRID3_ID == 202965] <- 'Koudougou')
AllCities <- within(AllCities, City[GRID3_ID == 456905] <- 'Ouahigouya')
AllCities <- within(AllCities, City[GRID3_ID ==      8] <- 'Banfora')
AllCities <- within(AllCities, City[GRID3_ID == 415487] <- 'Kaya')
# CAF
# AllCities <- within(AllCities, City[GRID3_ID == 10028] <- 'Bangui')
# AllCities <- within(AllCities, City[GRID3_ID == 25878] <- 'Bambari')
# AllCities <- within(AllCities, City[GRID3_ID == 929] <- 'Berberati')
# CMN
# country_iso = "CMN"
# country_name = "Cameroon"
# AllCities <- within(AllCities, City[GRID3_ID == 18] <- 'Douala')
# AllCities <- within(AllCities, City[GRID3_ID == 280] <- 'Yaounde')
# AllCities <- within(AllCities, City[GRID3_ID == 266] <- 'Bamenda')
# COG
# AllCities <- within(AllCities, City[GRID3_ID == 8605 <- 'Brazzaville')
# AllCities <- within(AllCities, City[GRID3_ID == 439] <- 'Pointe-Noire')
# AllCities <- within(AllCities, City[GRID3_ID == 14780] <- 'Ouesso')
# GAB
# AllCities <- within(AllCities, City[GRID3_ID == 12] <- 'Libreville')
# AllCities <- within(AllCities, City[GRID3_ID == 18] <- 'Port-Gentil')
# AllCities <- within(AllCities, City[GRID3_ID == 6] <- 'Franceville')
# MLI
# country_iso = "MLI"
# country_name = "Mali"
# AllCities <- within(AllCities, City[GRID3_ID == 154414] <- 'Bamako')
# AllCities <- within(AllCities, City[GRID3_ID == 379845] <- 'Gao')
# AllCities <- within(AllCities, City[GRID3_ID == 112387] <- 'Sikasso')
# AllCities <- within(AllCities, City[GRID3_ID ==  27564] <- 'Kayes')
# AllCities <- within(AllCities, City[GRID3_ID == 358292] <- 'Tombouctou')
# AllCities <- within(AllCities, City[GRID3_ID == 176434] <- 'Segou')
# NER
# country_iso = "NER"
# country_name = "Niger"
# AllCities <- within(AllCities, City[GRID3_ID ==  35956] <- 'Niamey')
# AllCities <- within(AllCities, City[GRID3_ID == 131184] <- 'Maradi')
# AllCities <- within(AllCities, City[GRID3_ID == 255088] <- 'Zinder')
# AllCities <- within(AllCities, City[GRID3_ID == 306120] <- 'Agadez')
# AllCities <- within(AllCities, City[GRID3_ID == 311568] <- 'Arlit')
# AllCities <- within(AllCities, City[GRID3_ID == 282070] <- 'Tanout')
# TCD
country_iso = "TCD"
country_name = "Chad"
AllCities <- within(AllCities, City[GRID3_ID == 131507] <- 'NDjamena')
AllCities <- within(AllCities, City[GRID3_ID == 278172] <- 'Abeche')
AllCities <- within(AllCities, City[GRID3_ID == 333078] <- 'Faya')
AllCities <- within(AllCities, City[GRID3_ID == 234510] <- 'Bol') # Bol is the only Chadian city in the list that is not HDurban. It's SDbot50.
AllCities <- within(AllCities, City[GRID3_ID == 4096] <- 'Moundou')
AllCities <- within(AllCities, City[GRID3_ID == 42500] <- 'Sarh')


### Data subsets  --------------------------------------
# ggplot has a hard time with multiple datasets plotted with different variable names.
# Where the names are different for a reason but need to be plotted together, make a new variable name.
AllCities$CuArea_CompareToAvg = AllCities$CuArea
TotGroTypA0$CuArea_CompareToAvg = TotGroTypA0$AvgCuArea
AllCities$Prop1y_CompareToAvg = AllCities$Prop1y * 100
TotGroTypA0$Prop1y_CompareToAvg = TotGroTypA0$AvgProp1y * 100
AllCities$Gro1y_CompareToAvg = AllCities$PcGro1y * 100
TotGroTypA0$Gro1y_CompareToAvg = TotGroTypA0$AvgGro * 100
# MinesGroTypA0$Gro1y_CompareToAvg = MinesGroTypA0$AvgGro * 100

# Narrow down to only case studies
examples <- AllCities %>%
  filter(!is.na(City))
examples86_15 = AllCities %>%
  filter(!is.na(City)) %>%
  filter(year != 1985)
examples88_15 = AllCities %>%
  filter(!is.na(City)) %>%
  filter(year > 1987)
examples05_15 = AllCities %>%
  filter(!is.na(City)) %>%
  filter(year > 2004)


# Narrow down to specific urban classes.
HDurban = TotGroTypA0 %>%
  filter(POPtyp=='HDurban')
HDurban86_15 = HDurban %>%
  filter(year != 1985)
HDurban88_15 = HDurban %>%
  filter(year > 1987)
HDurban05_15 = HDurban %>%
  filter(year > 2004)

SDtop = TotGroTypA0 %>%
  filter(POPtyp=='SDtop50')
SDtop86_15 = SDtop %>%
  filter(year != 1985)
SDtop88_15 = SDtop %>%
  filter(year > 1987)
SDtop05_15 = SDtop %>%
  filter(year > 2004)

SDbot = TotGroTypA0 %>%
  filter(POPtyp=='SDbot50')
SDbot86_15 = SDbot %>%
  filter(year != 1985)
SDbot88_15 = SDbot %>%
  filter(year > 1987)
SDbot05_15 = SDbot %>%
  filter(year > 2004)

# MinesSDtop05_15 = MinesGroTypA0 %>%
#   filter(POPtyp=="SDtop50" & year > 2004)
# MinesHDurban05_15 = MinesGroTypA0 %>%
#   filter(POPtyp=="HDurban" & year > 2004)
# MinesSDbot05_15 = MinesGroTypA0 %>%
#   filter(POPtyp=="SDbot50" & year > 2004)
# BFA_SSA = AllCities %>%
#   filter(POPtyp=="SSA")
# BFA_HA = AllCities %>%
#   filter(POPtyp=="HA")
# BFA_SSA00_15 = BFA_SSA %>%
#   filter(year > 1999)
# BFA_HA00_15 = BFA_HA %>%
#   filter(year > 1999)
# MinesSSA = MinesGroTypA0 %>%
#   filter(POPtyp=="SSA")
# MinesHA = MinesGroTypA0 %>%
#   filter(POPtyp=="HA")
# MinesSSA00_15 = MinesSSA %>%
#   filter(year > 1999)
# MinesHA00_15 = MinesHA %>%
#   filter(year > 1999)



# Subset of 5 year increments
examples_every5 <- examples %>% 
  filter_at(vars(year), 
            any_vars(. %in% c('1985', '1990', '1995', '2000', '2005', '2010', '2015'))) 
HDurban_every5 <- TotGroTypA0 %>%
  filter(POPtyp=="HDurban") %>%
  filter_at(vars(year),
            any_vars(. %in% c('1985', '1990', '1995', '2000', '2005', '2010', '2015')))
SDtop_every5 <- TotGroTypA0 %>%
  filter(POPtyp=="SDtop50") %>%
  filter_at(vars(year),
            any_vars(. %in% c('1985', '1990', '1995', '2000', '2005', '2010', '2015')))
SDbot_every5 <- TotGroTypA0 %>%
  filter(POPtyp=="SDbot50") %>%
  filter_at(vars(year),
            any_vars(. %in% c('1985', '1990', '1995', '2000', '2005', '2010', '2015')))
# MinesSSA_every5 <- MinesSSA %>%
#   filter_at(vars(year),
#             any_vars(. %in% c('1985', '1990', '1995', '2000', '2005', '2010', '2015')))
# MinesHA_every5 <- MinesHA %>%
#   filter_at(vars(year),
#             any_vars(. %in% c('1985', '1990', '1995', '2000', '2005', '2010', '2015')))
# BFA_SSA_every5 <- BFA_SSA %>%
#   filter_at(vars(year),
#             any_vars(. %in% c('1985', '1990', '1995', '2000', '2005', '2010', '2015')))
# BFA_HA_every5 <- BFA_HA %>%
#   filter_at(vars(year),
#             any_vars(. %in% c('1985', '1990', '1995', '2000', '2005', '2010', '2015')))



## PREPARE DESIGN WORKSPACE ---------------------------------------
### Load fonts ---------------------------------------

library(remotes)
#remotes::install_version("Rttf2pt1", version = "1.3.8")
library(Rttf2pt1)
library(extrafont)
# font_import()
# y
loadfonts(device = "win")
# fonts()

font_add(family="Barlow", regular="C:/Users/grace/AppData/Local/Microsoft/Windows/Fonts/Barlow-Black.ttf")
font_families() # Desired font should now be available.



### Color palettes  ---------------------------------------
cbPalette <- c("#E69F00", "#009E73", "#999999", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # Color blind friendly
SsaHaPalette = c("#D55E00", "#F0E442")
boldcolorPalette <- c("#CA6702",
                      "#b5838d", # Order this by the alphabetized cities list, based on preference. 
                      "#94D2BD",
                      "#005F73", 
                      "#001219",
                      "#E9D8A6", 
                      "#EE9B00", 
                      "#CA6702", 
                      "#9B2226", 
                      "#6d6875") 
WSFE5yrPalette = c("#ee6c4d", "#c8624c", "#a4574a", "#804c48", "#5c4145", "#363642")
Plum_color = c("#b5838d")
Darkblue_color = c("#005F73")
Orange_color = c("#CA6702")
Darkgrey_color = c("#001219") # Largest city (most prominent color).
Seafoam_color = c("#94D2BD")
Sand_color = c("#E9D8A6")


### Create quick-adds for plotting.  ---------------------------------------
THEME_BARLOW <- theme_light() + theme(text=element_text(family="Barlow"), 
                                      plot.title = element_text(face="bold", family="Barlow"),
                                      plot.subtitle = element_text(size=12, family="Barlow"),
                                      legend.title = element_text(face="bold", size=12, family="Barlow"), 
                                      legend.text = element_text(size=12, family="Barlow"),
                                      axis.title = element_text(size=14, family="Barlow"),
                                      axis.text = element_text(size=12, family="Barlow")) # https://ggplot2.tidyverse.org/reference/theme.html

CITY_LINE <- geom_line(aes(color=City), alpha = 1, lwd = 1.5) # Create line for each city
CITY_LOESS = geom_smooth(aes(color=City), alpha = 1, lwd = 1.5, se=F)

HD_LOESS = geom_smooth(data=HDurban, aes(color="High density metropoles"), lwd = 0.7, se=F, lty=5)
HD_LOESS_thick = geom_smooth(data=HDurban, aes(color="High density metropoles"), lwd = 1.5, se=F, lty=5)
HD_86_15_LOESS = geom_smooth(data=HDurban86_15, aes(color="High density metropoles"), lwd = 0.7, se=F, lty=5)
HD_86_15_LOESS_thick = geom_smooth(data=HDurban86_15, aes(color="High density metropoles"), lwd = 1.5, se=F, lty=5)
HD_88_15_LOESS = geom_smooth(data=HDurban88_15, aes(color="High density metropoles"), lwd = 0.7, se=F, lty=5)
HD_88_15_LOESS_thick = geom_smooth(data=HDurban88_15, aes(color="High density metropoles"), lwd = 1.5, se=F, lty=5)
HD_05_15_LOESS = geom_smooth(data=HDurban05_15, aes(color="High density metropoles"), lwd = 0.7, se=F, lty=5)
HD_05_15_LOESS_thick = geom_smooth(data=HDurban05_15, aes(color="High density metropoles"), lwd = 1.5, se=F, lty=5)

SDtop_LOESS = geom_smooth(data=SDtop, aes(color="Semi-dense cities,  upper 50%"), lwd = 0.7, se=F, lty=5)
SDtop_LOESS_thick = geom_smooth(data=SDtop, aes(color="Semi-dense cities,  upper 50%"), lwd = 1.5, se=F, lty=5)
SDtop_86_15_LOESS = geom_smooth(data=SDtop86_15, aes(color="Semi-dense cities,  upper 50%"), lwd = 0.7, se=F, lty=5)
SDtop_86_15_LOESS_thick = geom_smooth(data=SDtop86_15, aes(color="Semi-dense cities,  upper 50%"), lwd = 1.5, se=F, lty=5)
SDtop_88_15_LOESS = geom_smooth(data=SDtop88_15, aes(color="Semi-dense cities,  upper 50%"), lwd = 0.7, se=F, lty=5)
SDtop_88_15_LOESS_thick = geom_smooth(data=SDtop88_15, aes(color="Semi-dense cities,  upper 50%"), lwd = 1.5, se=F, lty=5)
SDtop_05_15_LOESS = geom_smooth(data=SDtop05_15, aes(color="Semi-dense cities,  upper 50%"), lwd = 0.7, se=F, lty=5)
SDtop_05_15_LOESS_thick = geom_smooth(data=SDtop05_15, aes(color="Semi-dense cities,  upper 50%"), lwd = 1.5, se=F, lty=5)

SDbot_LOESS = geom_smooth(data=SDbot, aes(color="Semi-dense cities, lower 50%"), lwd = 0.7, se=F, lty=5)
SDbot_LOESS_thick = geom_smooth(data=SDbot, aes(color="Semi-dense cities, lower 50%"), lwd = 1.5, se=F, lty=5)
SDbot_86_15_LOESS = geom_smooth(data=SDbot86_15, aes(color="Semi-dense cities, lower 50%"), lwd = 0.7, se=F, lty=5)
SDbot_86_15_LOESS_thick = geom_smooth(data=SDbot86_15, aes(color="Semi-dense cities, lower 50%"), lwd = 1.5, se=F, lty=5)
SDbot_88_15_LOESS = geom_smooth(data=SDbot88_15, aes(color="Semi-dense cities, lower 50%"), lwd = 0.7, se=F, lty=5)
SDbot_88_15_LOESS_thick = geom_smooth(data=SDbot88_15, aes(color="Semi-dense cities, lower 50%"), lwd = 1.5, se=F, lty=5)
SDbot_05_15_LOESS = geom_smooth(data=SDbot05_15, aes(color="Semi-dense cities, lower 50%"), lwd = 0.7, se=F, lty=5)
SDbot_05_15_LOESS_thick = geom_smooth(data=SDbot05_15, aes(color="Semi-dense cities, lower 50%"), lwd = 1.5, se=F, lty=5)

# MinesHDurban_05_15_LOESS_thick = geom_smooth(data=MinesHDurban05_15, aes(color="High density metropoles*"), lwd = 1.5, se=F)
# MinesSDtop_05_15_LOESS_thick = geom_smooth(data=MinesSDtop05_15, aes(color="Semi-dense cities, upper 50%"), lwd = 1.5, se=F)
# MinesSDbot_05_15_LOESS_thick = geom_smooth(data=MinesSDbot05_15, aes(color="Semi-dense cities, lower 50%"), lwd = 1.5, se=F)
# BFA_SSA_00_15_LOESS = geom_smooth(data=BFA_SSA00_15, aes(color=" Small settlement areas (large villages)"), lwd = 0.7, se=F, lty=5)
# BFA_HA_00_15_LOESS = geom_smooth(data=BFA_HA00_15, aes(color="Hamlets (small villages)"), lwd = 0.7, se=F, lty=5)
# MinesSSA_00_15_LOESS_thick = geom_smooth(data=MinesSSA00_15, aes(color=" Small settlement areas (large villages)"), lwd = 1.5, se=F)
# MinesHA_00_15_LOESS_thick = geom_smooth(data=MinesHA00_15, aes(color="Hamlets (small villages)"), lwd = 1.5, se=F)

# HD_AREA = geom_area(data=HDurban, aes(color="Semi-dense cities, upper 50%"), fill = "#E69F00", alpha = .3, lwd = 0) # Create filled area for cities averages
# HD_86_15_AREA <- geom_area(data = HDurban86_15, aes(color="Semi-dense cities, upper 50%"), fill = "#E69F00", alpha = .3, lwd = 0)
# HD_05_15_AREA <- geom_area(data = HDurban05_15, aes(color="Semi-dense cities, upper 50%"), fill = "#E69F00", alpha = .3, lwd = 0)
# SDtop_AREA = geom_area(data=SDtop, aes(color="Semi-dense cities, upper 50%"), fill = "#E69F00", alpha = .3, lwd = 0) # Create filled area for cities averages
# SDtop_86_15_AREA <- geom_area(data = SDtop86_15, aes(color="Semi-dense cities, upper 50%"), fill = "#E69F00", alpha = .3, lwd = 0)
# SDtop_05_15_AREA <- geom_area(data = SDtop05_15, aes(color="Semi-dense cities, upper 50%"), fill = "#E69F00", alpha = .3, lwd = 0)
# SDbot_AREA = geom_area(data = SDbot, aes(color="Semi-dense cities, lower 50%"), fill = "#E69F00", alpha = .3, lwd = 0)
# SDbot_86_15_AREA <- geom_area(data = SDbot86_15, aes(color="Semi-dense cities, lower 50%"), fill = "#E69F00", alpha = .3, lwd = 0)
# SDbot_05_15_AREA <- geom_area(data = SDbot05_15, aes(color="Semi-dense cities, lower 50%"), fill = "#E69F00", alpha = .3, lwd = 0)


transparency =   theme(
  panel.background = element_rect(fill='transparent'), #transparent panel bg
  plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
  panel.grid.major = element_blank(), #remove major gridlines
  panel.grid.minor = element_blank(), #remove minor gridlines
  legend.background = element_rect(fill='transparent', colour='transparent'), #transparent legend bg
  legend.box.background = element_rect(fill='transparent', colour='transparent') #transparent legend panel
)


## CHARTS ---------------------------------------
### Cumulative area ---------------------------------------
examples %>%
  ggplot(aes(x = year, y = CuArea)) +
  THEME_BARLOW +
  scale_color_manual(values = boldcolorPalette) + 
  CITY_LOESS +
  new_scale_color() +
  HD_LOESS + SDtop_LOESS + SDbot_LOESS + labs(color="Country-wide sum") +
  scale_color_manual(values = cbPalette) +
  THEME_BARLOW +
  transparency +
  labs(title = country_name, subtitle ="Cumulative area of the city, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_casestudies_HDSD_adm0.png", sep="_")
ggsave(plotsavename)

HDurban %>%
  ggplot(aes(x = year, y = CuArea)) +
  new_scale_color() +
  HD_LOESS_thick + labs(color="Country-wide sum") +
  SDtop_LOESS_thick + labs(color="Country-wide sum") +
  SDbot_LOESS_thick + labs(color="Country-wide sum") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = country_name, subtitle ="Total cumulative area of cities, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_HDSD_adm0.png", sep="_")
ggsave(plotsavename)

# MinesSSA00_15 %>%
#   ggplot(aes(x = year, y = CuArea)) +
#   new_scale_color() +
#   MinesSSA_00_15_LOESS_thick + labs(color="Within 5 hours of a mine") +
#   MinesHA_00_15_LOESS_thick + labs(color="Within 5 hours of a mine") +
#   THEME_BARLOW +
#   scale_color_manual(values = SsaHaPalette) + 
#   transparency +
#   labs(title = country_name, subtitle ="Total cumulative area of villages, 2000-2015. (smoothing: Loess)", 
#        x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
# plotsavename = "Mines_cumulativeArea_SSAHA_adm0.png"
# ggsave(plotsavename)


examples %>%
  filter(City=="NDjamena") %>%
  ggplot(aes(x = year, y = CuArea_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Darkgrey_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_LOESS + SDtop_LOESS +labs(color="Country-wide average") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: NDjamena", subtitle ="Cumulative area of the city compared to country average, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_NDjamena_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  filter(City=="NDjamena") %>%
  ggplot(aes(x = year, y = CuArea_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Darkgrey_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_05_15_LOESS + SDtop_05_15_LOESS + labs(color="Country-wide average") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: NDjamena", subtitle ="Cumulative area of the city compared to country average, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_NDjamena_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

examples %>%
  filter(City=="Bol") %>%
  ggplot(aes(x = year, y = CuArea_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Plum_color) + 
  CITY_LOESS +
  new_scale_color() +
  SDtop_LOESS + SDbot_LOESS + labs(color="Country-wide average") +
  THEME_BARLOW +
  scale_color_manual(values = c("#009E73", "#999999")) + 
  transparency +
  labs(title = "Chad: Bol", subtitle ="Cumulative area of the city compared to country average, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Bol_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  filter(City=="Bol") %>%
  ggplot(aes(x = year, y = CuArea_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Plum_color) + 
  CITY_LOESS +
  new_scale_color() +
  SDtop_05_15_LOESS + SDbot_05_15_LOESS + labs(color="Country-wide average") +
  THEME_BARLOW +
  scale_color_manual(values = c("#009E73", "#999999")) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Bol", subtitle ="Cumulative area of the city compared to country average, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Bol_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

examples %>%
  filter(City=="Moundou") %>%
  ggplot(aes(x = year, y = CuArea_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Darkblue_color) + 
  CITY_LOESS +
  new_scale_color() +
  SDtop_LOESS + HD_LOESS + labs(color="Country-wide average") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Moundou", subtitle ="Cumulative area of the city compared to country average, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Moundou_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  filter(City=="Moundou") %>%
  ggplot(aes(x = year, y = CuArea_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Darkblue_color) + 
  CITY_LOESS +
  new_scale_color() +
  SDtop_05_15_LOESS + HD_05_15_LOESS + labs(color="Country-wide average") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Moundou", subtitle ="Cumulative area of the city compared to country average, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Moundou_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

examples %>%
  filter(City=="Abeche") %>%
  ggplot(aes(x = year, y = CuArea_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Orange_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_LOESS + SDtop_LOESS +labs(color="Country-wide average") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Abeche", subtitle ="Cumulative area of the city compared to country average, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Abeche_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  filter(City=="Abeche") %>%
  ggplot(aes(x = year, y = CuArea_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Orange_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_05_15_LOESS + SDtop_05_15_LOESS + labs(color="Country-wide average") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Abeche", subtitle ="Cumulative area of the city compared to country average, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Abeche_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

examples %>%
  filter(City=="Faya") %>%
  ggplot(aes(x = year, y = CuArea_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Seafoam_color) + 
  CITY_LOESS +
  new_scale_color() +
  SDbot_LOESS + SDtop_LOESS +labs(color="Country-wide average") +
  THEME_BARLOW +
  scale_color_manual(values = c("#009E73", "#999999")) + 
  transparency +
  labs(title = "Chad: Faya", subtitle ="Cumulative area of the city compared to country average, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Faya_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  filter(City=="Faya") %>%
  ggplot(aes(x = year, y = CuArea_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Seafoam_color) + 
  CITY_LOESS +
  new_scale_color() +
  SDbot_05_15_LOESS + SDtop_05_15_LOESS + labs(color="Country-wide average") +
  THEME_BARLOW +
  scale_color_manual(values = c("#009E73", "#999999")) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Faya", subtitle ="Cumulative area of the city compared to country average, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Faya_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

examples %>%
  filter(City=="Sarh") %>%
  ggplot(aes(x = year, y = CuArea_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Sand_color) + 
  CITY_LOESS +
  new_scale_color() +
  SDtop_LOESS + HD_LOESS + labs(color="Country-wide average") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Sarh", subtitle ="Cumulative area of the city compared to country average, 1985-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Sarh_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  filter(City=="Sarh") %>%
  ggplot(aes(x = year, y = CuArea_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Sand_color) + 
  CITY_LOESS +
  new_scale_color() +
  SDtop_05_15_LOESS + HD_05_15_LOESS + labs(color="Country-wide average") +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Sarh", subtitle ="Cumulative area of the city compared to country average, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Cumulative area (km"^"2", ")")))
plotsavename = paste(country_iso, "cumulativeArea_Sarh_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)



### Proportion of area ---------------------------------------
examples86_15 %>%
  ggplot(aes(x = year, y = Prop1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = boldcolorPalette) +
  CITY_LOESS + 
  new_scale_color() +
  HD_86_15_LOESS + SDtop_86_15_LOESS + SDbot_86_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  transparency +
  labs(title = country_name, subtitle ="Proportion of total city area built each year, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Proportion of built-up area")))
plotsavename = paste(country_iso, "proportion_casestudies_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  ggplot(aes(x = year, y = Prop1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = boldcolorPalette) +
  CITY_LOESS + 
  new_scale_color() +
  HD_05_15_LOESS + SDtop_05_15_LOESS + SDbot_05_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = country_name, subtitle ="Proportion of total city area built each year, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Proportion of built-up area")))
plotsavename = paste(country_iso, "proportion_casestudies_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

HDurban86_15 %>%
  ggplot(aes(x = year, y = Prop1y_CompareToAvg)) +
  new_scale_color() +
  HD_86_15_LOESS_thick + labs(color="Country-wide average") +
  SDtop_86_15_LOESS_thick + labs(color="Country-wide average") +
  SDbot_86_15_LOESS_thick + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = country_name, subtitle ="Average proportion of city area built each year, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Proportion of built-up area")))
plotsavename = paste(country_iso, "proportion_HDSD_adm0.png", sep="_")
ggsave(plotsavename)

HDurban05_15 %>%
  ggplot(aes(x = year, y = Prop1y_CompareToAvg)) +
  new_scale_color() +
  HD_05_15_LOESS_thick + labs(color="Country-wide average") +
  SDtop_05_15_LOESS_thick + labs(color="Country-wide average") +
  SDbot_05_15_LOESS_thick + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = country_name, subtitle ="Average proportion of city area built each year, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Proportion of built-up area")))
plotsavename = paste(country_iso, "proportion_HDSD_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)



### Growth rate ---------------------------------------
examples86_15 %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  CITY_LOESS +
  scale_color_manual(values = boldcolorPalette) +
  new_scale_color() +
  scale_color_manual(values = cbPalette) + 
  HD_86_15_LOESS + SDtop_86_15_LOESS + SDbot_86_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  transparency +
  labs(title = country_name, subtitle ="Speed of built-up area growth, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_casestudies_HDSD_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  CITY_LOESS +
  scale_color_manual(values = boldcolorPalette) +
  new_scale_color() +
  scale_color_manual(values = cbPalette) + 
  HD_05_15_LOESS + SDtop_05_15_LOESS + SDbot_05_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = country_name, subtitle ="Speed of built-up area growth, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_casestudies_HDSD_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

HDurban86_15 %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  new_scale_color() +
  HD_86_15_LOESS_thick + labs(color="Country-wide average") +
  SDtop_86_15_LOESS_thick + labs(color="Country-wide average") +
  SDbot_86_15_LOESS_thick + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = country_name, subtitle ="Average growth in buildup from previous year, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_HDSD_adm0.png", sep="_")
ggsave(plotsavename)

HDurban05_15 %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  new_scale_color() +
  HD_05_15_LOESS_thick + labs(color="Country-wide average") +
  SDtop_05_15_LOESS_thick + labs(color="Country-wide average") +
  SDbot_05_15_LOESS_thick + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  # coord_cartesian(ylim =c(2, 13.5)) +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = country_name, subtitle ="Average growth in buildup from previous year, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_HDSD_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

# HDurban05_15 %>%
#   ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
#   new_scale_color() +
#   HD_05_15_LOESS + labs(color="Country-wide average") +
#   SDtop_05_15_LOESS + labs(color="Country-wide average") +
#   SDbot_05_15_LOESS + labs(color="Country-wide average") +
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#   THEME_BARLOW +
#   scale_color_manual(values = cbPalette) + 
#   new_scale_color() +
#   MinesHDurban_05_15_LOESS_thick + labs(color="Within 5 hours of a mine") +
#   MinesSDtop_05_15_LOESS_thick + labs(color="Within 5 hours of a mine") +
#   MinesSDbot_05_15_LOESS_thick + labs(color="Within 5 hours of a mine") +
#   scale_color_manual(values = cbPalette) + 
#   transparency +
#   scale_x_discrete(limits=c(2005, 2010, 2015)) +
#   labs(title = country_name, subtitle ="Average growth in buildup from previous year, 2005-2015. (smoothing: Loess)", 
#        x = "Year", y = expression(paste("Growth in buildup from previous year")))
# plotsavename = paste("Mines", "speedofgrowth_HDSD_2005-2015_adm0.png", sep="_")
# ggsave(plotsavename)
# 
# BFA_SSA00_15 %>%
#   ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
#   new_scale_color() +
#   BFA_SSA_00_15_LOESS + labs(color="Country-wide average") +
#   BFA_HA_00_15_LOESS + labs(color="Country-wide average") +
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#   THEME_BARLOW +
#   scale_color_manual(values = SsaHaPalette) +
#   new_scale_color() +
#   MinesSSA_00_15_LOESS_thick + labs(color="Within 5 hours of a mine") +
#   MinesHA_00_15_LOESS_thick + labs(color="Within 5 hours of a mine") +
#   scale_color_manual(values = SsaHaPalette) +
#   transparency +
#   scale_x_discrete(limits=c(2005, 2010, 2015)) +
#   labs(title = country_name, subtitle ="Average growth in buildup from previous year, 2005-2015. (smoothing: Loess)",
#        x = "Year", y = expression(paste("Growth in buildup from previous year")))
# plotsavename = paste("Mines", "speedofgrowth_HDSD_2005-2015_adm0.png", sep="_")
# ggsave(plotsavename)

examples86_15 %>%
  filter(City=="NDjamena") %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Darkgrey_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_86_15_LOESS + SDtop_86_15_LOESS + SDbot_86_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: NDjamena", subtitle ="Growth in buildup from previous year compared to country average, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_NDjamena_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  filter(City=="NDjamena") %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Darkgrey_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_05_15_LOESS + SDtop_05_15_LOESS + SDbot_05_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: NDjamena", subtitle ="Growth in buildup from previous year compared to country average, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_NDjamena_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

examples86_15 %>%
  filter(City=="Bol") %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Plum_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_86_15_LOESS + SDtop_86_15_LOESS + SDbot_86_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Bol", subtitle ="Growth in buildup from previous year compared to country average, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_Bol_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  filter(City=="Bol") %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Plum_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_05_15_LOESS + SDtop_05_15_LOESS + SDbot_05_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Bol", subtitle ="Growth in buildup from previous year compared to country average, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_Bol_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

examples86_15 %>%
  filter(City=="Moundou") %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Darkblue_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_86_15_LOESS + SDtop_86_15_LOESS + SDbot_86_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Moundou", subtitle ="Growth in buildup from previous year compared to country average, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_Moundou_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  filter(City=="Moundou") %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Darkblue_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_05_15_LOESS + SDtop_05_15_LOESS + SDbot_05_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Moundou", subtitle ="Growth in buildup from previous year compared to country average, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_Moundou_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

examples86_15 %>%
  filter(City=="Sarh") %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Sand_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_86_15_LOESS + SDtop_86_15_LOESS + SDbot_86_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Sarh", subtitle ="Growth in buildup from previous year compared to country average, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_Sarh_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  filter(City=="Sarh") %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Sand_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_05_15_LOESS + SDtop_05_15_LOESS + SDbot_05_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Sarh", subtitle ="Growth in buildup from previous year compared to country average, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_Sarh_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

examples86_15 %>%
  filter(City=="Abeche") %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Orange_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_86_15_LOESS + SDtop_86_15_LOESS + SDbot_86_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Abeche", subtitle ="Growth in buildup from previous year compared to country average, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_Abeche_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  filter(City=="Abeche") %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Orange_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_05_15_LOESS + SDtop_05_15_LOESS + SDbot_05_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Abeche", subtitle ="Growth in buildup from previous year compared to country average, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_Abeche_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)

examples86_15 %>%
  filter(City=="Faya") %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Seafoam_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_86_15_LOESS + SDtop_86_15_LOESS + SDbot_86_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  labs(title = "Chad: Faya", subtitle ="Growth in buildup from previous year compared to country average, 1986-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_Faya_adm0.png", sep="_")
ggsave(plotsavename)

examples05_15 %>%
  filter(City=="Faya") %>%
  ggplot(aes(x = year, y = Gro1y_CompareToAvg)) +
  THEME_BARLOW +
  scale_color_manual(values = Seafoam_color) + 
  CITY_LOESS +
  new_scale_color() +
  HD_05_15_LOESS + SDtop_05_15_LOESS + SDbot_05_15_LOESS + labs(color="Country-wide average") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  THEME_BARLOW +
  scale_color_manual(values = cbPalette) + 
  transparency +
  scale_x_discrete(limits=c(2005, 2010, 2015)) +
  labs(title = "Chad: Faya", subtitle ="Growth in buildup from previous year compared to country average, 2005-2015. (smoothing: Loess)", 
       x = "Year", y = expression(paste("Growth in buildup from previous year")))
plotsavename = paste(country_iso, "speedofgrowth_Faya_2005-2015_adm0.png", sep="_")
ggsave(plotsavename)




### Proportion of area, 5yr increments ---------------------------------------
examples_every5$year = as.factor(examples_every5$year)
Averages_every5 = rbind(HDurban_every5, SDtop_every5)
Averages_every5 = rbind(Averages_every5, SDbot_every5)
Averages_every5$POPtyp = with(Averages_every5, ifelse(POPtyp == "HDurban", "High density metropoles",
                                                      ifelse(POPtyp == "SDtop50", "Semi-dense cities,  upper 50%", "Semi-dense cities, bottom 50%")))
Averages_every5$year = as.factor(Averages_every5$year)


examples_every5 %>%
  filter(year != 1985) %>%
  ggplot(aes(x = City, y = Prop5y, fill = forcats::fct_rev(year))) +
  THEME_BARLOW +
  geom_bar(stat='identity') +
  scale_fill_manual(values = WSFE5yrPalette, name="year") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  transparency +
  labs(title = country_name, subtitle ="Proportion of all post-1985 build-up", 
       x = "", y = expression(paste("Proportion of post-1985 built-up area")))
plotsavename = paste(country_iso, "proportion_every5_casestudies.png", sep="_")
ggsave(plotsavename)

Averages_every5 %>%
  filter(year != 1985) %>%
  ggplot(aes(x = POPtyp, y = Prop5y, fill = forcats::fct_rev(year))) +
  THEME_BARLOW +
  geom_bar(stat='identity') +
  scale_fill_manual(values = WSFE5yrPalette, name="year") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  transparency +
  labs(title = country_name, subtitle ="Proportion of all post-1985 build-up", 
       x = "", y = expression(paste("Proportion of post-1985 built-up area")))
plotsavename = paste(country_iso, "proportion_every5_HDSD.png", sep="_")
ggsave(plotsavename)




