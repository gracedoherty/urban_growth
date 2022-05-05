

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
library(tidyr) # complete()
library(tidyverse) # ggplot2()
library(viridis)


wsfe = st_read('WSFE_growthstats_grid3_v2/WSFE_BFA_growthstats2.shp')
wsfe = select(wsfe, -c(type, ADM1_CODE, ADM1_NAME))

grid3 = st_read('GRID3_BFA/GRID3_Burkina_Faso_Settlement_Extents%2C_Version_01.01..shp')
grid3 = select(grid3, c(OBJECTID, type, pop_wp_gri, Shape__Are, geometry))
grid3 = rename(grid3, c("grid3_area"="Shape__Are", "pop"="pop_wp_gri"))


# ------------- PREPARE ADMIN AREAS -------------
# Burkina, Mali, and Niger ADM1,2,3
adm3 = st_read('adm3/adm3_BFA_NER_MLI_fromEPI.shp')

adm1 <- adm1 %>% 
  group_by(admin1Pcod) %>% 
  summarise()
adm2 <- adm3 %>% 
  group_by(admin2Pcod) %>% 
  summarise()
adm3 <- select(adm3, c(admin3Pcod, geometry))

# Optional: Validate by plotting.
# ggplot(adm1) + geom_sf(aes(fill=admin1Pcod))
# ggplot(adm2) + geom_sf(aes(fill=admin2Pcod))
# ggplot(adm3) + geom_sf(aes(fill=admin3Pcod))


# Save for later.
st_write(adm1, "adm1_sahel3.shp", driver = "ESRI Shapefile", append=FALSE)
st_write(adm2, "adm2_sahel3.shp", driver = "ESRI Shapefile", append=FALSE)
st_write(adm3, "adm3_sahel3.shp", driver = "ESRI Shapefile", append=FALSE)



# ------------- SPATIAL JOIN BASED ON CENTER OF GRID3 SETTLEMENT -------------
# Set everything to same projection.
st_crs(wsfe) == st_crs(grid3) # False
grid3 <- st_transform(grid3, crs = st_crs(wsfe))

st_crs(grid3) == st_crs(adm1) # False
adm1 = st_transform(adm1, crs = st_crs(grid3))
adm2 = st_transform(adm2, crs = st_crs(grid3))
adm3 = st_transform(adm3, crs = st_crs(grid3))


# GRID3 polygons to point (centroid)
grid3_pt = st_centroid(grid3)
# plot(grid3_pt$geometry, pch=1, cex=0.3, col="purple", bg="gray80") # pch=1 is circles, pch=0 is squares.
# plot(grid3$geometry, border="gray20", col=NA, add=T)
# plot(grid3_pt$geometry, pch=1, cex=0.3, col="purple", bg="gray80", add=F)
# plot(adm1$geometry, border="gray20", col=NA, add=T)


# Spatial join: GRID3 within ADM
grid3_pt = st_join(grid3_pt, adm1, join=st_within, left=T)
grid3_pt = st_join(grid3_pt, adm2, join=st_within, left=T)
grid3_pt = st_join(grid3_pt, adm3, join=st_within, left=T)
# ggplot(grid3_pt) + geom_sf(aes(fill=admin1Pcod))


# Table join: WSFE NEAR_FID, GRID3 OBJECTID
grid3_pt <- grid3_pt %>% st_drop_geometry() # Merge on two sf objects not permitted
wsfe_adm <- merge(wsfe, grid3_pt, by.x="NEAR_FID", by.y="OBJECTID", all.x=TRUE)



# ------------- SAVE TO FILE -------------
wsfe_adm <- select(wsfe_adm, -c(grid3_area, Shape_Leng, Shape_Area)) # Remove temporary columns.

st_write(wsfe_adm, "WSFE_BFA_adm.shp", driver = "ESRI Shapefile", append=FALSE)
