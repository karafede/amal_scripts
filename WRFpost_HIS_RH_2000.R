
library(RNetCDF)
library(rgdal)
# library(ncdf4)
library(raster)
library(stringr)
library(leaflet)
library(NISTunits)
library(lubridate)
library(dplyr)

setwd("Z:/HIS PostProcessing")
source("extract_pnt_raster.R")

dir <- "Z:/HIS PostProcessing/UAE_boundary"

### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
# names(shp_UAE)
plot(shp_UAE)


# load coordinates of the cities:
STATIONS_COORDS <- read.csv("C:/Users/aaldababseh/WRF_PostProcessing_DATA/List of Stations2_wrf.csv") 
colnames(STATIONS_COORDS) <- c("station", "latitude", "longitude")


colnames(STATIONS_COORDS)[colnames(STATIONS_COORDS) == 'latitude'] <- 'Latitude'
colnames(STATIONS_COORDS)[colnames(STATIONS_COORDS) == 'longitude'] <- 'Longitude'

extracted_WRF_RH <- NULL
DateTime_RH <- NULL
site_RH <- NULL


###############################################
##### HIS WRF #################################
###############################################

setwd("Z:/HIS/2000")

# list .nc files


patt<- c(".nc", "*_d02")
# patt<- c(".nc")
filenames <- list.files(pattern = patt)

# gerate a time sequence for the WRF HIS run at intervals of 1 hour for year 2000
start <- as.POSIXct("2000-01-01 00:00") 
interval <- 60 #minutes
end <- start + as.difftime(366, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:8618]

name <- str_sub(filenames, start = 1, end = -4)

    
 # j <- 4

  for(j in 1:length(filenames)) {
 #   for(j in 1:20) {

  WRF_file <- open.nc(filenames[j])
  WRF <- read.nc(WRF_file)
  name_vari <- names(WRF)
  name_vari
  
  close.nc(WRF_file)

#### only one variable (RH) == var = 18
  
     var_value<-(WRF[18])
     names(var_value) <- "xxyyzz"
     var_value <- (var_value$xxyyzz)
     LON <-WRF$lon
     LAT <-WRF$lat

     
     xmn = min(LON)
     xmx = max(LON)
     ymn = min(LAT)
     ymx = max(LAT)
     

    MMM <-  t(var_value[ , ])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    r <- crop(r, extent(shp_UAE))
    r <- mask(r, shp_UAE)
    plot(r)
    

    # extract points from each raster at coordinate locations of the site
    EXTRACTED_WRF_RH <- extract_points(r, STATIONS_COORDS)
    extracted_WRF_RH = rbind(extracted_WRF_RH, EXTRACTED_WRF_RH)    # data vector
    DATETIME_RH <- as.data.frame(rep(TS[j], nrow(STATIONS_COORDS)))           # time vector
    DateTime_RH <- rbind(DateTime_RH, DATETIME_RH)
    SITE_RH <- as.data.frame(STATIONS_COORDS$station)
    site_RH <- rbind(site_RH, SITE_RH)
    
    remove(r)
  }


 extracted_WRF_RH <- cbind(DateTime_RH, extracted_WRF_RH, site_RH)
 colnames(extracted_WRF_RH) <- c("DateTime", "WRF_RH_2000", "station")
 
 
 # save data-------------------------------------
 write.csv(extracted_WRF_RH, "Z:/HIS/HIS_tif/extracted_WRF_RH_2000.csv")
 

 
   

#########################################################################################
#########################################################################################

# load NCMS data ###########

ABU <- read.csv("Z:/HIS/NCMS/AbuDhabi_00_05.csv")
ALAIN <- read.csv("Z:/HIS/NCMS/Alain00_05.csv")
MEZAIRA <- read.csv("Z:/HIS/NCMS/Mezaira03_05.csv")


# add station name
ABU$station <- "ABU DHABI INTL"
ALAIN$station <- "AL AIN INTL"
MEZAIRA$station <- "MEZAIRA"

ABU <- ABU %>%
  mutate(DateTime = mdy_hm(Date)) 

ALAIN <- ALAIN %>%
  mutate(DateTime = mdy_hm(Date)) 


MEZAIRA <- MEZAIRA %>%
  mutate(DateTime = mdy_hm(Date)) 

NCMS_data <- rbind(ABU,
                   ALAIN,
                   MEZAIRA)
str(NCMS_data)


# load WRF data

extracted_WRF_RH <- read.csv("Z:/HIS/HIS_tif/extracted_WRF_RH_2000.csv")
str(extracted_WRF_RH)

extracted_WRF_RH <- extracted_WRF_RH %>%
  mutate(DateTime = ymd_hms(DateTime))

# merge extracted WRF-data with NCMS data--------------------------------------------

NCMS_AND_WRF_RH <- extracted_WRF_RH %>%
  merge(NCMS_data, by = c("station", "DateTime"))



write.csv(NCMS_AND_WRF_RH, "Z:/HIS/HIS_tif/NCMS_WRF_RH_2000.csv")

######################################################################################################

##########################################################################################################
######### plot TIME-SERIES of HOURLY NCMS data and WRF RH data ##################################
##########################################################################################################

library(ggplot2)

NCMS_AND_WRF_RH <- read.csv("Z:/HIS/HIS_tif/NCMS_WRF_RH_2000.csv")
str(NCMS_AND_WRF_RH)


NCMS_AND_WRF_RH$DateTime <- ymd_hms(NCMS_AND_WRF_RH$DateTime)


plot <- ggplot(NCMS_AND_WRF_RH, aes(DateTime, WRF_RH_2000)) +
  theme_bw() +
  geom_line(aes(y = WRF_RH_2000, col = "WRF_RH_2000"), alpha=1, col="black") +
  geom_line(aes(y = RH, col = "RH"), alpha=1, col="red") +
  scale_color_discrete(name = "Y series", labels = c("WRF_RH_2000", "RH")) +
  stat_smooth(method = "loess") +
  facet_wrap(~ station, nrow = 2) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Hourly Relative Humdity (%)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(0, 100)
plot


#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/HIS/HIS_tif/"


png(paste0(output_folder,"RH_HOURLY_Time_series_2000_NCMS_WRF.png"), width = 1500, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


################################################


# get data from smooth fit curve --------------------------------------------
# AAA <- all_WRF_RH_data %>%
#   dplyr::select(DateTime,
#                 WRF_RH_2000,
#                 RH)
# 
# AAA$DateTime <- as.numeric(AAA$DateTime)  # need Date to be numeric
# 
# smoothed_curve_ORIGINAL <- predict(loess(WRF_RH_2000 ~ DateTime , AAA),
#                                    AAA$DateTime)
# 
# smoothed_curve_LC <- predict(loess(RH ~ DateTime , AAA),
#                              AAA$DateTime)
# 
# 
# 
# fit_ORIGINAL <- lm(WRF_RH_2000 ~ DateTime , AAA)
# summary(fit_2000)
# 
# fit_RH <- lm(RH ~ DateTime , AAA)
# summary(fit_RH)

###############################################################
###############################################################
# fit_2000_vs_RH <- lm(WRF_RH_2000 ~ RH , AAA)
# summary(fit_2000_vs_RH)

###############################################################
###############################################################


###############################################################
# DAILIY AVERAGES #############################################
###############################################################

NCMS_AND_WRF_RH_AVG <- NCMS_AND_WRF_RH %>%
  mutate(DATE = date(DateTime)) %>%
  group_by(DATE,
           station) %>%
  summarize(daily_AVG_WRF_RH = mean(WRF_RH_2000),
            daily_AVG_NCMS_RH = mean(RH))


plot <- ggplot(NCMS_AND_WRF_RH_AVG, aes(DATE, daily_AVG_WRF_RH)) +
  theme_bw() +
  geom_line(aes(y = daily_AVG_WRF_RH, col = "daily_AVG_WRF_RH"), alpha=1, col="black", size =1) +
  geom_line(aes(y = daily_AVG_NCMS_RH, col = "daily_AVG_NCMS_RH"), alpha=1, col="red", size =1) +
  scale_color_discrete(name = "Y series", labels = c("daily_AVG_WRF_RH", "daily_AVG_NCMS_RH")) +
  stat_smooth(method = "loess") +
  facet_wrap(~ station, nrow = 2) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Hourly Relative Humdity (%)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(0, 100)
plot

#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/HIS/HIS_tif/"


png(paste0(output_folder,"RH_BIAS_Time_series_2000_NCMS_WRF.png"), width = 1500, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


################################################

 