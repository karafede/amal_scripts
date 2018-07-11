

library(RCurl)
library(stringr)
library(plyr)
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)
library(lubridate)
library(gstat)
library(ncdf.tools)


dir <- "D:/MODIS_AOD/UAE_boundary"

### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
# names(shp_UAE)
plot(shp_UAE)



# crop over the UAE area
r <- crop(r, extent(shp_WRF))
r <- mask(r, shp_WRF)
#   plot(r)

