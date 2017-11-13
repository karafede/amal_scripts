
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)
library(leaflet)


# list .nc files


########################################################################
##### ORIGINAL LAND COVER - Use Folder: Temperature- ORIGINAL 2 ########
########################################################################

setwd("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Original2")

patt<- ".nc"
filenames <- list.files(pattern = patt)

# erate a time sequence for the WRF run at intervals of 1 hour (should be 744 images), 31 days
start <- as.POSIXct("2060-05-01 00:00")
interval <- 60 #minutes
end <- start + as.difftime(31, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:744]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 
# filenames <- filenames[2]

import_nc_WRF <- function(filenames){
  all_rasters <- stack()    # stack ALL 744 HOURS together in an unique raster
  
  j <- 4
  
  for(j in 1:length(filenames)) {
    
    WRF_file <- open.nc(filenames[j])
    WRF_file <- read.nc(WRF_file)
    name_vari <- names(WRF_file)
    name_vari
    
    #### only one variable (temperature 2m) == var = 14, (RH) == var ==18, and (ws) == var 21
    
    var_value<-(WRF_file[14]) 
    names(var_value) <- "xxyyzz"
    var_value <- (var_value$xxyyzz)
    LON <-WRF_file$lon
    LAT <-WRF_file$lat
    
    xmn = min(LON)
    xmx = max(LON)
    ymn = min(LAT)
    ymx = max(LAT)
    
    # i = 5
    
    MMM <-  t(var_value[ , ])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    all_rasters <- stack(all_rasters,r)
  }
  
  return(all_rasters)
}


BBB <- import_nc_WRF(filenames)

writeRaster(BBB, "T2m_2060_May_Original_Hourly.tif" , options= "INTERLEAVE=BAND", overwrite=T)



#################################################################
##### NEW LAND COVER Use Folder: Temperature --LC2 ##############
#################################################################

setwd("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/LC2")

patt<- ".nc"
filenames <- list.files(pattern = patt)

# gerate a time sequence for the WRF run at intervals of 1 hour (should be 744 images), 31 days
start <- as.POSIXct("2060-05-01 00:00")
interval <- 60 #minutes
end <- start + as.difftime(31, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:744]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 
# filenames <- filenames[2]

import_nc_WRF <- function(filenames){
  all_rasters <- stack()    # stack ALL 744 HOURS together in an unique raster
  
  for(j in 1:length(filenames)) {
    
    WRF_file <- open.nc(filenames[j])
    WRF_file <- read.nc(WRF_file)
    name_vari <- names(WRF_file)
    
    
    #### only one variable (temperature 2m) == var = 14
    
    var_value<-(WRF_file[14])
    names(var_value) <- "xxyyzz"
    var_value <- (var_value$xxyyzz)
    LON <-WRF_file$lon
    LAT <-WRF_file$lat
    
    xmn = min(LON)
    xmx = max(LON)
    ymn = min(LAT)
    ymx = max(LAT)
    
    # i = 5
    
    MMM <-  t(var_value[ , ])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    all_rasters <- stack(all_rasters,r)
  }
  
  return(all_rasters)
}


BBB <- import_nc_WRF(filenames)

 writeRaster(BBB, "T2m_2060_May_LC_Hourly.tif" , options= "INTERLEAVE=BAND", overwrite=T)



########################################################################################
########################################################################################
########################################################################################

###############################################################
##### ORIGINAL LAND COVER - Use Folder: RH--- ORIGINAL 2 ######
###############################################################

setwd("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Original2")

patt<- ".nc"
filenames <- list.files(pattern = patt)

# erate a time sequence for the WRF run at intervals of 1 hour (should be 744 images), 31 days
start <- as.POSIXct("2060-05-01 00:00")
interval <- 60 #minutes
end <- start + as.difftime(31, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:744]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 
# filenames <- filenames[2]

import_nc_WRF <- function(filenames){
  all_rasters <- stack()    # stack ALL 744 HOURS together in an unique raster
  
  j <- 4
  
  for(j in 1:length(filenames)) {
    
    WRF_file <- open.nc(filenames[j])
    WRF_file <- read.nc(WRF_file)
    name_vari <- names(WRF_file)
    name_vari
    
    #### only one variable (Relative Humidty 10m) == var = 18
    
  
    var_value<-(WRF_file[18])
    names(var_value) <- "xxyyzz"
    var_value <- (var_value$xxyyzz)
    LON <-WRF_file$lon
    LAT <-WRF_file$lat
    
    xmn = min(LON)
    xmx = max(LON)
    ymn = min(LAT)
    ymx = max(LAT)
    
    # i = 5
    
    MMM <-  t(var_value[ , ]) # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r_rh <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r_rh)
    all_rasters <- stack(all_rasters,r_rh)
  }
  
  return(all_rasters)
}


CCC <- import_nc_WRF(filenames)

writeRaster(CCC, "RH_2060_May_Original_Hourly.tif" , options= "INTERLEAVE=BAND", overwrite=T)


#################################################################
##### NEW LAND COVER Use Folder: RH -- LC2 #####################
#################################################################

setwd("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/LC2")

patt<- ".nc"
filenames <- list.files(pattern = patt)

# gerate a time sequence for the WRF run at intervals of 1 hour (should be 744 images), 31 days
start <- as.POSIXct("2060-05-01 00:00")
interval <- 60 #minutes
end <- start + as.difftime(31, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:744]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 
# filenames <- filenames[2]

import_nc_WRF <- function(filenames){
  all_rasters <- stack()    # stack ALL 744 HOURS together in an unique raster
  
  for(j in 1:length(filenames)) {
    
    WRF_file <- open.nc(filenames[j])
    WRF_file <- read.nc(WRF_file)
    name_vari <- names(WRF_file)
    
    
    #### only one variable (Relative Humidity 10m) == var = 18
    
    var_value<-(WRF_file[18])
    names(var_value) <- "xxyyzz"
    var_value <- (var_value$xxyyzz)
    LON <-WRF_file$lon
    LAT <-WRF_file$lat
    
    xmn = min(LON)
    xmx = max(LON)
    ymn = min(LAT)
    ymx = max(LAT)
    
    # i = 5
    
    MMM <-  t(var_value[ , ])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r_rh <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r_rh)
    all_rasters <- stack(all_rasters,r_rh)
  }
  
  return(all_rasters)
}


CCC <- import_nc_WRF(filenames)

writeRaster(CCC, "RH_2060_May_LC_Hourly.tif" , options= "INTERLEAVE=BAND", overwrite=T)


#############################################################################################
#############################################################################################
#############################################################################################


###############################################################
##### ORIGINAL LAND COVER - Use Folder: WS- ORIGINAL 2 ########
###############################################################

setwd("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Original2")

patt<- ".nc"
filenames <- list.files(pattern = patt)

# erate a time sequence for the WRF run at intervals of 1 hour (should be 744 images), 31 days
start <- as.POSIXct("2060-05-01 00:00")
interval <- 60 #minutes
end <- start + as.difftime(31, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:744]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 
# filenames <- filenames[2]

import_nc_WRF <- function(filenames){
  all_rasters <- stack()    # stack ALL 744 HOURS together in an unique raster
  
  j <- 4
  
  for(j in 1:length(filenames)) {
    
    WRF_file <- open.nc(filenames[j])
    WRF_file <- read.nc(WRF_file)
    name_vari <- names(WRF_file)
    name_vari
    
    #### only one variable (Wind Speed) == var = 21
    
        var_value<-(WRF_file[21])
    names(var_value) <- "xxyyzz"
    var_value <- (var_value$xxyyzz)
    LON <-WRF_file$lon
    LAT <-WRF_file$lat
    
    xmn = min(LON)
    xmx = max(LON)
    ymn = min(LAT)
    ymx = max(LAT)
    
    # i = 5
    
    MMM <-  t(var_value[ , ])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r_ws <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r_ws)
    all_rasters <- stack(all_rasters,r_ws)
  }
  
  return(all_rasters)
}


KKK <- import_nc_WRF(filenames)

writeRaster(KKK, "WS_2060_May_Original_Hourly.tif" , options= "INTERLEAVE=BAND", overwrite=T)



#################################################################
##### NEW LAND COVER Use Folder: WS --- LC2 #####################
#################################################################

setwd("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/LC2")

patt<- ".nc"
filenames <- list.files(pattern = patt)

# gerate a time sequence for the WRF run at intervals of 1 hour (should be 744 images), 31 days
start <- as.POSIXct("2060-05-01 00:00")
interval <- 60 #minutes
end <- start + as.difftime(31, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:744]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 
# filenames <- filenames[2]

import_nc_WRF <- function(filenames){
  all_rasters <- stack()    # stack ALL 744 HOURS together in an unique raster
  
  for(j in 1:length(filenames)) {
    
    WRF_file <- open.nc(filenames[j])
    WRF_file <- read.nc(WRF_file)
    name_vari <- names(WRF_file)
    
    
    #### only one variable (temperature 2m) == var = 14
    
    var_value<-(WRF_file[21])
    names(var_value) <- "xxyyzz"
    var_value <- (var_value$xxyyzz)
    LON <-WRF_file$lon
    LAT <-WRF_file$lat
    
    xmn = min(LON)
    xmx = max(LON)
    ymn = min(LAT)
    ymx = max(LAT)
    
    # i = 5
    
    MMM <-  t(var_value[ , ])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r_ws <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r_ws)
    all_rasters <- stack(all_rasters,r_ws)
  }
  
  return(all_rasters)
}


KKK <- import_nc_WRF(filenames)

writeRaster(KKK, "WS_2060_May_LC_Hourly.tif" , options= "INTERLEAVE=BAND", overwrite=T)



########################################################################################
########################################################################################
########################################################################################


library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)


setwd("C:/Users/aaldababseh/R_Codes_PostPorcessing")
source("extract_pnt_raster.R")


# read all AWS NCMS data
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs")
# All_AWS_data <- read_csv("AWS_concatenated_DUST_2_April_2015.csv")


# load coordinates of the cities:
STATIONS_COORDS <- read.csv("C:/Users/aaldababseh/WRF_PostProcessing_DATA/List of Stations2_wrf.csv") 
colnames(STATIONS_COORDS) <- c("station", "latitude", "longitude")

# join coordinated of the the station with the total dataset
# All_AWS_data <- All_AWS_data %>%
#   left_join(STATIONS_COORDS, by = c("station"))


# select unique sites of the AWS NCMS data
# sites_stations_AWS_NCMS <- All_AWS_data[!duplicated(All_AWS_data[c("station", "latitude", "longitude" )]),]
# rename latitude and longitude
colnames(STATIONS_COORDS)[colnames(STATIONS_COORDS) == 'latitude'] <- 'Latitude'
colnames(STATIONS_COORDS)[colnames(STATIONS_COORDS) == 'longitude'] <- 'Longitude'

# omit stations without latitude and longitude
# sites_stations_AWS_NCMS <- sites_stations_AWS_NCMS[!(is.na(sites_stations_AWS_NCMS$Latitude)), ]


##############################################################################
# read temperature data from WRF ORGININAL2 ###################################
##############################################################################

# read all bands in a stack raster
WRF_STACK_Temp_ORIGINAL <- stack("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Original2/T2m_2060_May_Original_Hourly.tif")
n <- length(WRF_STACK_Temp_ORIGINAL@layers)



# generate a time sequence for the WRF run at intervals of 1 hour
start <- as.POSIXct("2060-05-01 00:00")
interval <- 60 #minutes
end <- start + as.difftime(31, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:744]


#### Temperature from WRF #############################################################
###########################################################################################

# make an empty vector

extracted_WRF_Temp <- NULL
DateTime_Temp <- NULL
site_Temp <- NULL

i <- 4

for (i in 1:n) {   # this is a time
  
  WRF_STACK_Temp_ORIGINAL <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Original2/T2m_2060_May_Original_Hourly.tif", band = i)
  plot(WRF_STACK_Temp_ORIGINAL)
  EXTRACTED_WRF_Temp <- extract_points(WRF_STACK_Temp_ORIGINAL, STATIONS_COORDS)
  extracted_WRF_Temp = rbind(extracted_WRF_Temp, EXTRACTED_WRF_Temp)    # data vector
  DATETIME_Temp <- as.data.frame(rep(TS[i], nrow(STATIONS_COORDS)))           # time vector
  DateTime_Temp <- rbind(DateTime_Temp, DATETIME_Temp)
  SITE_Temp <- as.data.frame(STATIONS_COORDS$station)
  site_Temp <- rbind(site_Temp, SITE_Temp)
  
}

extracted_WRF_Temp <- cbind(DateTime_Temp, extracted_WRF_Temp, site_Temp)
colnames(extracted_WRF_Temp) <- c("DateTime", "WRF_Temp_ORIGINAL", "station")


# save data-------------------------------------
write.csv(extracted_WRF_Temp, "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Original2/extracted_WRF_Temp_ORIGINAL.csv")



##############################################################################
# read temperature data from WRF new LAND COVER LC2 ##############################
##############################################################################

# read all bands in a stack raster
WRF_STACK_Temp_LC <- stack("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/LC2/T2m_2060_May_LC_Hourly.tif")
n <- length(WRF_STACK_Temp_LC@layers)



# gerate a time sequence for the WRF run at intervals of 1 hour
start <- as.POSIXct("2060-05-01 00:00")
interval <- 60 #minutes
end <- start + as.difftime(31, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:744]


#### Temperature from WRF #############################################################
###########################################################################################

# make an empty vector

extracted_WRF_Temp <- NULL
DateTime_Temp <- NULL
site_Temp <- NULL

i <- 4

for (i in 1:n) {   # this is a time
  
  WRF_STACK_Temp_LC <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/LC2/T2m_2060_May_LC_Hourly.tif", band = i)
  plot(WRF_STACK_Temp_LC)
  EXTRACTED_WRF_Temp <- extract_points(WRF_STACK_Temp_LC, STATIONS_COORDS)
  extracted_WRF_Temp = rbind(extracted_WRF_Temp, EXTRACTED_WRF_Temp)    # data vector
  DATETIME_Temp <- as.data.frame(rep(TS[i], nrow(STATIONS_COORDS)))           # time vector
  DateTime_Temp <- rbind(DateTime_Temp, DATETIME_Temp)
  SITE_Temp <- as.data.frame(STATIONS_COORDS$station)
  site_Temp <- rbind(site_Temp, SITE_Temp)
  
}

extracted_WRF_Temp <- cbind(DateTime_Temp, extracted_WRF_Temp, site_Temp)
colnames(extracted_WRF_Temp) <- c("DateTime", "WRF_Temp_LC", "station")


# save data-------------------------------------
write.csv(extracted_WRF_Temp, "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/LC2/extracted_WRF_Temp_LC.csv")

############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################




# add 4 hours to WRF(UTC) DateTime ##############################################

# str(extracted_WRF_Temp)
# 
# extracted_WRF_Temp <- extracted_WRF_Temp %>%
#   mutate(DateTime = ymd_hms(DateTime))
# 
# str(extracted_WRF_Temp)
# 
# extracted_WRF_Temp <- extracted_WRF_Temp %>%
#   mutate(DateTime = DateTime + 14400)

######################################################################################################
######################################################################################################

######################################################################################################
######################################################################################################


library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(ggplot2)


# merge extracted WRF-data ORIGINAL + LC --------------------------------------------

extracted_WRF_Temp_LC <- read.csv("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/LC2/extracted_WRF_Temp_LC.csv")
extracted_WRF_Temp_ORIGINAL <- read.csv("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Original2/extracted_WRF_Temp_ORIGINAL.csv")



all_WRF_Temp_data <- extracted_WRF_Temp_ORIGINAL %>%
  left_join(extracted_WRF_Temp_LC, by = c("station", "DateTime"))


write.csv(all_WRF_Temp_data, "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Time_series_all2.csv")

################################################################################
################################################################################


###################################################################################################################
######### plot TIME-SERIES of WRF Original LC and New LC data #########################################
###################################################################################################################


all_WRF_Temp_data <- read.csv("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Time_series_all2.csv")
str(all_WRF_Temp_data)

all_WRF_Temp_data$DateTime <- ymd_hms(all_WRF_Temp_data$DateTime)


plot <- ggplot(all_WRF_Temp_data, aes(DateTime, WRF_Temp_ORIGINAL)) +
  theme_bw() +
  geom_line(aes(y = WRF_Temp_ORIGINAL, col = "WRF_Temp_ORIGINAL"), alpha=1, col="blue") +
  geom_line(aes(y = WRF_Temp_LC, col = "WRF_Temp_LC"), alpha=1, col="red") +
  scale_color_discrete(name = "Y series", labels = c("WRF_Temp_ORIGINAL", "WRF_Temp_LC")) +
  stat_smooth(method = "loess") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Temperature ", "(", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  ylim(15, 45)
plot



#### save plot ###############################################################
##############################################################################

output_folder <- "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/"


png(paste0(output_folder,"T_Time_series_comparison2.png"), width = 1800, height = 600,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


################################################

############################################################################
########### Temperature frequency between original and LC  ################# 
############################################################################

all_WRF_Temp_data$diff <- all_WRF_Temp_data$WRF_Temp_LC - all_WRF_Temp_data$WRF_Temp_ORIGINAL



# filter LC - ORIG > 0  for ABU DHABI
all_WRF_Temp_ABUDHABI_positive <- all_WRF_Temp_data %>%
#  filter(diff > 0) %>%
  filter(station == "ABU DHABI INTL") 

# frequency ABU DHABI
all_WRF_Temp_ABUDHABI_positive$perc <- ((all_WRF_Temp_ABUDHABI_positive$diff)/nrow(all_WRF_Temp_data))*100




  
# filter LC - ORIG > 0  for Al AIN
all_WRF_Temp_ALAIN_positive <- all_WRF_Temp_data %>%
#  filter(diff > 0) %>%
  filter(station == "AL AIN INTL") 

# frequency Al AIN
all_WRF_Temp_ALAIN_positive$perc <- ((all_WRF_Temp_ALAIN_positive$diff)/nrow(all_WRF_Temp_data))*100




# filter LC - ORIG > 0  for MEZAIRA
all_WRF_Temp_MEZAIRA_positive <- all_WRF_Temp_data %>%
  #  filter(diff > 0) %>%
  filter(station == "MEZAIRA") 

# frequency MEZAIRA
all_WRF_Temp_MEZAIRA_positive$perc <- ((all_WRF_Temp_MEZAIRA_positive$diff)/nrow(all_WRF_Temp_data))*100


# concatenate the data AGAIN!!!! #######

all_WRF_Temp_data <- rbind(all_WRF_Temp_ABUDHABI_positive,
                           all_WRF_Temp_ALAIN_positive,
                           all_WRF_Temp_MEZAIRA_positive)



## plot Frequency Temperature for 3 stations ############


plot <- ggplot(all_WRF_Temp_data, aes(x=perc)) +
  theme_bw() +
    geom_histogram(binwidth=.005, colour="black", fill="white") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("Counts of % changes- T2M"))) +
  xlab(expression(paste("Percent of changes for Temperature (%)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  geom_vline(aes(xintercept=0),  
             color="red", linetype="dashed", size=0.5)
plot


output_folder <- "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/"


png(paste0(output_folder,"frequency_temperature.png"), width = 1800, height = 600,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


################################################################################################################
## plot Frequency Temperature for 3 stations TIME-Series (histograms)  #########################################


plot <- ggplot(all_WRF_Temp_data, aes(DateTime, perc)) +
  theme_bw() +
  geom_bar(stat="identity") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("T2M percent of changes (%) "))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  ylim(-0.5, 0.5)
plot

output_folder <- "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/"


png(paste0(output_folder,"frequency_time-series_temperature.png"), width = 1800, height = 600,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()



############################################################################
########### Diurnal averages ###############################################

all_WRF_Temp_data <- all_WRF_Temp_data %>%
  mutate(date = date(DateTime))



all_WRF_Temp_data_daily <- all_WRF_Temp_data %>%
  group_by(date,
           station) %>%
  summarise(diurnal_AVG_ORIG = mean(WRF_Temp_ORIGINAL),
            diurnal_AVG_LC = mean(WRF_Temp_LC))



plot <- ggplot(all_WRF_Temp_data_daily, aes(date, diurnal_AVG_ORIG)) +
  theme_bw() +
  geom_line(aes(y = diurnal_AVG_ORIG, col = "diurnal_AVG_ORIG"), alpha=1, col="black", size = 0.75) +
  geom_line(aes(y = diurnal_AVG_LC, col = "diurnal_AVG_LC"), alpha=1, col="red", size = 0.75) +
  scale_color_discrete(name = "Y series", labels = c("diurnal_AVG_ORIG", "diurnal_AVG_LC")) +
  stat_smooth(method = "loess", size = 1) +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste("24h Average T2m ", "(", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=10, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  ylim(20, 45)
plot



#### save plot ###############################################################
##############################################################################

output_folder <- "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/"


png(paste0(output_folder,"T_24h_Time_series_comparison2.png"), width = 1800, height = 600,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()







#####################################################################################
#####################################################################################
#####################################################################################
# get data from smooth fit curve --------------------------------------------
# AAA$Date <- as.numeric(all_WRF_Temp_data$DateTime)  # need Date to be numeric


AAA <- all_WRF_Temp_data %>%
  dplyr::select(DateTime,
                WRF_Temp_ORIGINAL,
                WRF_Temp_LC)

AAA$DateTime <- as.numeric(AAA$DateTime)  # need Date to be numeric

smoothed_curve_ORIGINAL <- predict(loess(WRF_Temp_ORIGINAL ~ DateTime , AAA),
                                   AAA$DateTime)

smoothed_curve_LC <- predict(loess(WRF_Temp_LC ~ DateTime , AAA),
                             AAA$DateTime)



fit_ORIGINAL <- lm(WRF_Temp_ORIGINAL ~ DateTime , AAA)
summary(fit_ORIGINAL)

fit_LC <- lm(WRF_Temp_LC ~ DateTime , AAA)
summary(fit_LC)

###############################################################
###############################################################
fit_ORIGINAL_vs_LC <- lm(WRF_Temp_ORIGINAL ~ WRF_Temp_LC , AAA)
summary(fit_ORIGINAL_vs_LC)

###############################################################
###############################################################



