
library(RNetCDF)
library(fields)
library(maptools)
library(rgdal)
library(readr)
library(ggplot2)
library(gstat)
library(sp)
library(maptools)
library(raster)   
library(rgeos)
library(plyr)
library(dplyr)
library(leaflet)
library(htmltools)
library(ncdf4)
library(htmlwidgets)
library(spatialEco)
library(pracma)
library(threadr)
library(webshot)
library(readr)

setwd ("C:/MI Drive/CAMS_NRT_UAE/PM25") 
dir <- "C:/MI Drive/CAMS_NRT_UAE/UAE_boundary"

### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
# names(shp)

shp_UAE@data$name <- 1:nrow(shp_UAE)
plot(shp_UAE)

filenames <- list.files(pattern = "\\.nc") # list all forecast for next 5 days (ECMWF)

# extract points from each .nc file ----------------------------------------
forecasts_files <- function (file) {      ## this is the filenames 
  
  # get list of field names
  # file <- "z_cams_c_ecmf_20161128000000_prod_fc_sfc_107_pm2p5.nc"
  nome <- str_sub(file, start = 1, end = -4)
  PM25_forecast <- file
  
# PM25_forecast <- "z_cams_c_ecmf_20161128000000_prod_fc_sfc_107_pm2p5.nc"
PM25_forecast <- open.nc(PM25_forecast)

#Read data
PM25_forecast <- read.nc(PM25_forecast)

LAT_forecast <- as.vector(PM25_forecast$latitude)  ### get Lat
LON_forecast <- as.vector(PM25_forecast$longitude)  ### get Lon
LAT <- repmat(LAT_forecast,1,900)
LAT <- t(LAT)
LAT <- as.data.frame(LAT)
LON = repmat(LON_forecast,451,1)
LON = c(LON)

LON_west <-subset(LON, LON < 360 & LON > 335)
max(LON_west)
min(LON_west)


LON <- as.data.frame(LON)
LON_east <- LON %>%
  filter(!(LON < 360 & LON > 335))

LON_west <- LON_west - 360
LON_west <- as.data.frame(LON_west)
colnames(LON_west) <- "LON"
LON <- rbind(LON_west, LON_east)

Data_forecast <- PM25_forecast$pm2p5  ### lat , lon
Data_forecast[is.na(Data_forecast)] <- 0 
Data_forecast <- t(Data_forecast)
Data_forecast <- c(Data_forecast)
Data_forecast <- as.data.frame(Data_forecast)

forecast_PM25 <- cbind(LON, LAT, Data_forecast)
colnames(forecast_PM25) <- c("LON", "LAT", "Data_forecast")

# cut data here.....
forecast_PM25_UAE <- subset(forecast_PM25, LON <= 57.11 & LON >= 50.34 & LAT >= 22.24 & LAT <= 29.5)

write.csv(forecast_PM25_UAE, file = paste(nome,"_UAE.csv", sep = ""), row.names=FALSE)

}


# extract data points for each .nc file
BBB <- lapply(filenames, forecasts_files)

######################################################################################
######################################################################################

filenames_forecasts <- list.files(pattern = "\\.csv$")
# filenames_forecasts <- filenames_forecasts[1:5]

LON <- read_csv(filenames_forecasts[1])[,1]
LAT <- read_csv(filenames_forecasts[1])[,2]
# read the PM25 data from the first file to initialize the total matrix
PM25_forecast <- read_csv(filenames_forecasts[1])[,3]


## Bind all data together 
for (i in 2:length(filenames_forecasts)) {
  PM25 <- read_csv(filenames_forecasts[i])[,3]
  PM25_forecast <- cbind(PM25_forecast, PM25)
}


# make an average of all the columns
PM25_forecast <- rowMeans(PM25_forecast, na.rm = TRUE) # kgm-3
PM25_forecast <- PM25_forecast * 1e+9 # ugm-3


PM25_forecast_data <- cbind(LON, LAT, PM25_forecast)
PM25_forecast_data <- subset(PM25_forecast_data, !is.na(values) & !LAT == -999 & !LON == -999)

write.csv(PM25_forecast_data, "C:/MI Drive/CAMS_NRT_UAE/PM25_forecast_29Nov_3Dec_2016.csv")

################################################################################

crs <- projection(shp_UAE) ### get projections from shp file

PM25_forecast <- SpatialPointsDataFrame(PM25_forecast_data[,1:2], PM25_forecast_data, proj4string=CRS(crs)) 
pixels <- SpatialPixelsDataFrame(PM25_forecast, tolerance = 0.916421, PM25_forecast@data)
raster_PM25_forecast <- raster(pixels[,'PM25_forecast'])
plot(raster_PM25_forecast)
plot(shp_UAE, add=TRUE, lwd=1)

# write a nc file
forecast_PM25_nc <- writeRaster(raster_PM25_forecast,
                                 filename="raster_PM25_forecast.nc",
                                 format="CDF", overwrite=TRUE) 
forecast_PM25_nc <- raster("raster_PM25_forecast.nc")

forecast_PM25_nc <- crop(forecast_PM25_nc, extent(shp_UAE))
forecast_PM25_nc <- mask(forecast_PM25_nc, shp_UAE)
plot(forecast_PM25_nc)
plot(shp_UAE, add=TRUE, lwd=1)

# convert nc file into .tiff

library(tiff)
## Output: a GeoTIFF file
file.tiff_forecast_PM25 <- 'forecast_PM25.tif'
forecast_PM25.tif <- writeRaster(forecast_PM25_nc, filename = file.tiff_forecast_PM25, format = 'GTiff', overwrite = T)
plot(forecast_PM25.tif)
forecast_PM25_tiff <- raster("forecast_PM25.tif")
plot(forecast_PM25_tiff)
plot(shp_UAE, add=TRUE, lwd=1)

####### Map data with Leaflet #############################################

# define color palette
pal_forecast_PM25 <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
                                         getValues(forecast_PM25_tiff),na.color = "transparent")

map <- leaflet() %>%
 # setView(lng = -2, lat = 53.5, zoom = 6) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addRasterImage(forecast_PM25_tiff, colors = pal_forecast_PM25, opacity = 0.5,
                 group = "forecast_PM25_40km") %>%
  
  addPolygons(stroke = TRUE, data = shp_UAE,
              weight = 1.5, color = "black",
              fillOpacity = 0,
              group = "shape_UAE") %>%
  
  addLegend("bottomright",pal = pal_forecast_PM25, values = values(forecast_PM25_tiff),
            title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) <br /> 29 Nov - 3 Dec 2016: </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.5) %>%

addLayersControl(
  baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
  overlayGroups = c("forecast_PM25_40km"),
  options = layersControlOptions(collapsed = TRUE)) 
#  hideGroup("forecast_PM25_40km") 

map


# save map
saveWidget(map, 'ECMWF_IFS_forecast_5days_from_28Nov2016.html', selfcontained = FALSE)
webshot('ECMWF_IFS_forecast_5days_from_28Nov2016.html', file='ECMWF_IFS_forecast_5days_from_28Nov2016.png',
        vwidth = 1800, vheight = 900, 
        cliprect = 'viewport')



#####################--------------------------##############################
#####################--------------------------##############################

############################################################################
############################################################################
############################################################################

#####################--------------------------##############################
#####################--------------------------##############################
#####################--------------------------##############################
#############################################################################

# load Donkelaar Satellite-Derived PM2.5, 2015, at 35% RH [ug/m3] (with Geographical regression adjustment)

PM25_2015_GRW <- raster("C:/MI Drive/CAMS_NRT_UAE/New_data_Donkelaar_1km/2015/GlobalGWR_PM25_GL_201501_201512-RH35.nc")
projection(PM25_2015_GRW) <- CRS("+proj=longlat +datum=WGS84")

### crop raster over the UAE shp file  ###############################
PM25_2015_GRW <- crop(PM25_2015_GRW, extent(shp_UAE))
PM25_2015_GRW <- mask(PM25_2015_GRW, shp_UAE)

file.tiff_GWR_PM25_1km_2015 <- 'GWR_PM25_1km_2015.tif'
GWR_PM25_1km_2015_tiff <- writeRaster(PM25_2015_GRW, filename = file.tiff_GWR_PM25_1km_2015, format = 'GTiff', overwrite = T)
plot(GWR_PM25_1km_2015_tiff)

plot(PM25_2015_GRW)
plot(shp_UAE, add=TRUE, lwd=1)

# check resoltuion of Donkelaar data and of ECMWF data
res(GWR_PM25_1km_2015_tiff) # 0.01
res(forecast_PM25_tiff) # 0.4



### Exctract points from raster ######################################
# PM25_2014_GWR <- rasterToPoints(PM25_2014_nc_cropped)
# head(PM25_2014_GWR)
# colnames(PM25_2014_GWR) <- c("Lon", "Lat", "PM25_2014")
# PM25_2014_GWR <- as.data.frame (PM25_2014_GWR)
# PM25_2014_GWR <- subset(PM25_2014_GWR, !is.na(PM25_2014) & PM25_2014>0)
# head(PM25_2014_GWR)
# write.csv(PM25_2014_GWR, file = "PM25_2014_GWR_Donkelaar.csv", row.names=FALSE)

#############################################################################################

## make resolution of CHIMERE as the one of GWR-------------------------------
forecast_PM25_tiff_1km = projectRaster(forecast_PM25_tiff, PM25_2015_GRW)
forecast_PM25_tiff_1km <- crop(forecast_PM25_tiff_1km, extent(shp_UAE))
forecast_PM25_tiff_1km <- mask(forecast_PM25_tiff_1km, shp_UAE)
plot(forecast_PM25_tiff_1km)
plot(shp_UAE, add=TRUE, lwd=1)

# write a nc file
forecast_PM25_1km_nc <- writeRaster(forecast_PM25_tiff_1km,
                               filename="forecast_PM25_1km.nc",
                               format="CDF", overwrite=TRUE) 

file.tiff_forecast_PM25_1km_1km <- 'forecast_PM25_1km.tif'
forecast_PM25_1km_tiff <- writeRaster(forecast_PM25_1km_nc, filename = file.tiff_forecast_PM25_1km_1km, format = 'GTiff', overwrite = T)
plot(forecast_PM25_1km_tiff)



## -------------------------------########################################
### Exctract poitns from TIFF files ######################################
# same resolution 1km as from Donkelaar

forecast_PM25_1km <- raster("forecast_PM25_1km.tif")
plot (forecast_PM25_1km)
GWR <- raster("GWR_PM25_1km_2015.tif")
plot(GWR)


### Exctract poitns from raster ######################################
forecast_PM25_1km_pt <- rasterToPoints(forecast_PM25_1km)
head(forecast_PM25_1km_pt)
colnames(forecast_PM25_1km_pt) <- c("Lon", "Lat", "forecast_PM25_1km")
forecast_PM25_1km_pt <- as.data.frame (forecast_PM25_1km_pt)
forecast_PM25_1km_pt <- subset(forecast_PM25_1km_pt, !is.na(forecast_PM25_1km) & forecast_PM25_1km > 0)
head(forecast_PM25_1km_pt)

GWR_pt <- rasterToPoints(GWR)
head(GWR_pt)
colnames(GWR_pt) <- c("Lon", "Lat", "PM25_2015_GWR")
GWR_pt <- as.data.frame (GWR_pt)
head(GWR_pt)

#Join data on common fields lat, lon
joint_data <- forecast_PM25_1km_pt %>%
  join(GWR_pt, c("Lon", "Lat"))

head(joint_data)


# rescale forecasted data with the GWR_PM2.5 data----------------------------------------
# normalise GWR data first....

joint_data <- na.omit(joint_data)  # remove NA values
max(joint_data$PM25_2015_GWR)

# normalize GWR data (these are the reference data)
joint_data$PM25_2015_GWR <- (joint_data$PM25_2015_GWR)/max(joint_data$PM25_2015_GWR) # normalise GWR PM2.5 data with the maximum value
# rescale data (the one to be rescaled)
joint_data <- joint_data %>%
  mutate(forecast_PM25_1km_scaled = forecast_PM25_1km * PM25_2015_GWR)

# go back to a spatial object

crs <- projection(shp_UAE) ### get projections from shp file

joint_data <- SpatialPointsDataFrame(joint_data[,1:2], joint_data, proj4string=CRS(crs)) 
pix <- SpatialPixelsDataFrame(joint_data, tolerance = 0.916421, joint_data@data)
forecast_PM25_1km_scaled <- raster(pix[,'forecast_PM25_1km_scaled'])
plot(forecast_PM25_1km_scaled)
plot(shp_UAE, add=TRUE, lwd=1)


####### Map data with Leaflet #############################################

# define color palette
pal_forecast_PM25_1km <- colorNumeric(c("#9999FF", "#ffd699", "#FFFF00", "#ffbf00", "#ffc700", "#FF0000", "#994c00"),
                                      getValues(forecast_PM25_1km_tiff),na.color = "transparent")

pal_forecast_PM25_1km_scaled <- colorNumeric(c("#9999FF", "#ffd699", "#FFFF00", "#ffbf00", "#ffc700", "#FF0000", "#994c00"),
                                      getValues(forecast_PM25_1km_scaled),na.color = "transparent")

pal_PM25_2015_GWR <- colorNumeric(c("#9999FF", "#ffd699", "#FFFF00", "#ffbf00", "#ffc700", "#FF0000", "#994c00"),
                                             getValues(GWR_PM25_1km_2015_tiff),na.color = "transparent")


map <- leaflet() %>%
  # setView(lng = -2, lat = 53.5, zoom = 6) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addRasterImage(forecast_PM25_1km_tiff, colors = pal_forecast_PM25_1km, opacity = 0.4,
                 group = "forecast_PM25_1km") %>%
  addRasterImage(forecast_PM25_1km_scaled, colors = pal_forecast_PM25_1km_scaled, opacity = 0.4,
                 group = "forecast_PM25_1km_scaled") %>%
  # Donkelaar data
  addRasterImage(GWR_PM25_1km_2015_tiff, colors = pal_PM25_2015_GWR, opacity = 0.4,
                 group = "PM25_2015_GWR") %>%
  
  # addLegend("bottomright",pal = pal_forecast_PM25_1km_scaled, values = values(forecast_PM25_1km_scaled),
  #           title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) <br /> (IFS - ECMWF) modelled <br />  29 Nov - 3 Dec 2016: </strong>",
  #           labFormat = labelFormat(prefix = ""),
  #           opacity = 0.8) %>%
  
  addLegend("bottomright",pal = pal_PM25_2015_GWR, values = values(GWR_PM25_1km_2015_tiff),
            title = "<br><strong>PM<sub>2.5</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) (1km) <br /> 2015 Annual mean <br /> Donkelaar et al. 2016 : </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.8) %>%
  
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("forecast_PM25_1km", "forecast_PM25_1km_scaled", "PM25_2015_GWR"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup("forecast_PM25_1km") %>%
hideGroup("forecast_PM25_1km_scaled") 

map



#######---------------------------------------------##################################
#######---------------------------------------------##################################

###########################################################################################################################
###########################################################################################################################
########## Land Cover ----------------------------------------------------

library(raster)
library(sp)
library(fields)
library(gstat)

setwd("C:/MI Drive/CAMS_NRT_UAE")
# landcover data
land_cover_UAE <- raster("C:/MI Drive/CAMS_NRT_UAE/Land_Cover_MCD12Q1.tif")
plot(land_cover_UAE)


### crop raster over the EUrope shp file  ###############################
land_cover_UAE <- crop(land_cover_UAE, extent(shp_UAE))
land_cover_UAE <- mask(land_cover_UAE, shp_UAE)

# write a nc file
land_cover_UAE_nc <- writeRaster(land_cover_UAE,
                                 filename="land_cover_UAE.nc",
                                 format="CDF", overwrite=TRUE) 

land_cover_UAE_nc <- raster("land_cover_UAE.nc")
plot(land_cover_UAE_nc)

# write a tiff file
file.tiff_land_cover_UAE_500m <- 'land_cover_UAE_500m.tif'
land_cover_UAE_500m_tiff <- writeRaster(land_cover_UAE, filename = file.tiff_land_cover_UAE_500m, format = 'GTiff', overwrite = T)
plot(land_cover_UAE_500m_tiff)

plot(shp_UAE, add=TRUE, lwd=1)

# check resoltuion 
res(land_cover_UAE_nc)


# exctract data of Land cover
land_cover_UAE_pt <- rasterToPoints(land_cover_UAE)
head(land_cover_UAE_pt)
colnames(land_cover_UAE_pt) <- c("Lon", "Lat", "class")
land_cover_UAE_pt <- as.data.frame (land_cover_UAE_pt)


## make resolution of CHIMERE as the one of land cover at 500m-------------------------------
# CHIMERE_1km_nc <- raster("CHIMERE_1km.nc")
# plot(CHIMERE_1km_nc)
# CHIMERE_500m = projectRaster(CHIMERE_1km_nc, land_cover_EU_nc) 
# plot(CHIMERE_500m)
# res(CHIMERE_500m)


### Exctract poitns from raster ######################################
# CHIMERE_pt <- rasterToPoints(CHIMERE_500m)
# head(CHIMERE_pt)
# colnames(CHIMERE_pt) <- c("Lon", "Lat", "PM25_2014_CHIMERE")
# CHIMERE_pt <- as.data.frame (CHIMERE_pt)
# # CHIMERE_pt <- subset(CHIMERE_pt, !is.na(PM25_2014_CHIMERE) & PM25_2014_CHIMERE>0)
# head(CHIMERE_pt)


# joint_data <- CHIMERE_pt%>%
#   join(land_cover_EU_pt, c("Lon", "Lat"))
# # remove missing values
# joint_data <- subset(joint_data, !is.na(class) & class>0)


##### only keep urban fraction  Lancover == 13 #########

LC <- as.data.frame(land_cover_UAE_pt$class)

LC[LC < 13] <- 0
LC[LC > 13] <- 0

LC_URBAN_UAE <- cbind(land_cover_UAE_pt$Lon, land_cover_UAE_pt$Lat, (LC/13))  ### urban fraction(%)
colnames(LC_URBAN_UAE) <- c("Lon", "Lat", "Land_URB")
max(LC_URBAN_UAE$Land_URB)

extent(land_cover_UAE_nc)
# xmin        : 51.43716 
# xmax        : 56.38299 
# ymin        : 22.49523 
# ymax        : 26.07857 

############## Regridding --------------------------------------
##############--------------------------------------------------

LC_URBAN_UAE$x <- LC_URBAN_UAE$Lon
LC_URBAN_UAE$y <- LC_URBAN_UAE$Lat

coordinates(LC_URBAN_UAE) = ~x + y  ## Set spatial coordinates to create a Spatial object:

x.range <- as.numeric(c(51.43716, 56.38299))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(22.49523, 26.07857))  # min/max latitude of the interpolation area

# regrid at native resolution of 500 m
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.005),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.005))  # expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE
plot(grd)


idw <- idw(formula = Land_URB ~ 1, locations = LC_URBAN_UAE, 
           newdata = grd)  # apply idw model for the data (interpolation)

idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("Lon", "Lat", "LC_UAE_URB_500m")  # give names to the modelled variables

write.csv(idw.output, file = "LC_URB_UAE_interp.csv", row.names=FALSE)

##############
##############

setwd("C:/MI Drive/CAMS_NRT_UAE")
LC_URB_UAE <- read_csv("LC_URB_UAE_interp.csv")
crs <- projection(shp_UAE) ### get projections from shp file

LC_URB_UAE <- SpatialPointsDataFrame(LC_URB_UAE[,1:2], LC_URB_UAE, proj4string=CRS(crs)) 
pixels <- SpatialPixelsDataFrame(LC_URB_UAE, tolerance = 0.916421, LC_URB_UAE@data)
raster_LC_URB_UAE <- raster(pixels[,'LC_UAE_URB_500m'])
plot(raster_LC_URB_UAE)
plot(shp_UAE, add=TRUE, lwd=1)

## crop data
raster_LC_URB_UAE <- crop(raster_LC_URB_UAE, extent(shp_UAE))
raster_LC_URB_UAE <- mask(raster_LC_URB_UAE, shp_UAE)
res(raster_LC_URB_UAE)
plot(raster_LC_URB_UAE)
plot(shp_UAE, add=TRUE, lwd=1)


# write a nc file
LC_URB_UAE_nc <- writeRaster(raster_LC_URB_UAE,
                                filename="LC_URB_UAE.nc",
                                format="CDF", overwrite=TRUE) 
LC_URB_UAE_nc <- raster("LC_URB_UAE.nc")

### colors for raster URB land cover (UAE region)
pal_URB <- colorNumeric(c("#FFFFCC", "#ff0000","#000000"), getValues(LC_URB_UAE_nc),
                        na.color = "transparent")


#### Leafleft map URB land cover

map <- leaflet() %>%
  # setView(lng = -2, lat = 53.5, zoom = 6) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  # URBAN land cover raster
  addRasterImage(LC_URB_UAE_nc, colors = pal_URB, opacity = 0.7,
                 group = "URBAN_fraction") %>%

  
  addLegend("bottomright",pal = pal_URB, values = values(LC_URB_UAE_nc),
            title = "<br><strong> % Urban area </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.8) %>%
  
  addLayersControl(
    baseGroups = c("Road map", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("URBAN_fraction"),
    options = layersControlOptions(collapsed = TRUE))
 # hideGroup("forecast_PM25_1km") 

map




## Geographical regression model -----------------------------------------------------------------------------

library(spgwr)
library(AICcmodavg)
library(usdm)
library(nortest)

# mydata[is.na(mydata)] <- 0

nn <- 100/nrow(LC_URBAN_FR)
memory.limit()

gwr100 <- gwr(PM25_2014_CHIMERE - in_situ ~ Land_URB + Land_RUR + LAND_Desert + model_data  data  = LC_URBAN_FR,
              coords = cbind(LC_URBAN_FR$Lon, LC_URBAN_FR$Lat), 
              adapt=nn, gweight=gwr.bisquare, hatmatrix=FALSE)

gwr100


