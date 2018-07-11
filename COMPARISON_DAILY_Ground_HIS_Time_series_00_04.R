
library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
library(NISTunits)
library(stringr)
library(reshape2)


setwd("Z:/Data_analysis")


# merge extracted WRF-data HISTORICAL with ground observations 2000-2004--------------------------------------------

####################################################################################################
############################# Extract HISTORICAL Variables #########################################
####################################################################################################

extracted_WRF_HIS_Rain <- read.csv("Z:/Data_analysis/HIS_tif/extracted_WRF_Rain_2000_2004.csv")
extracted_WRF_HIS_RH <- read.csv("Z:/Data_analysis/HIS_tif/extracted_WRF_RH_2000_2004.csv")
extracted_WRF_HIS_SWD <- read.csv("Z:/Data_analysis/HIS_tif/extracted_WRF_SWD_2000_2004.csv")
extracted_WRF_HIS_Temp <- read.csv("Z:/Data_analysis/HIS_tif/extracted_WRF_Temp_2000_2004.csv")
extracted_WRF_HIS_WS <- read.csv("Z:/Data_analysis/HIS_tif/extracted_WRF_WS_2000_2004.csv")

all_WRF_data_HIS <- extracted_WRF_HIS_Rain %>%
  left_join(extracted_WRF_HIS_RH, by = c("station", "DateTime"))

all_WRF_data_HIS <- all_WRF_data_HIS %>%
  left_join(extracted_WRF_HIS_SWD, by = c("station", "DateTime"))

all_WRF_data_HIS <- all_WRF_data_HIS %>%
  left_join(extracted_WRF_HIS_Temp, by = c("station", "DateTime"))

all_WRF_data_HIS <- all_WRF_data_HIS %>%
  left_join(extracted_WRF_HIS_WS, by = c("station", "DateTime"))

all_WRF_data_HIS <- all_WRF_data_HIS %>%
  dplyr::select(DateTime,
                WRF_Rain_2000_2004,
                WRF_RH_2000_2004,
                WRF_SWD_2000_2004,
                WRF_Temp_2000_2004,
                WRF_WS_2000_2004,
                station)

write.csv(all_WRF_data_HIS, "Z:/Data_analysis/HIS_tif/WRF_HIS_time_series.csv")   #hourly climate readings

all_WRF_data_HIS <- all_WRF_data_HIS %>%
  mutate(Date = date(DateTime))

all_WRF_data_HIS_DAILY <- all_WRF_data_HIS %>%   
  group_by(Date,
           station) %>%
  summarize(WS_WRF_AVG = mean(WRF_WS_2000_2004),
            TEMP_WRF_AVG = mean(WRF_Temp_2000_2004),
            RH_WRF_AVG = mean(WRF_RH_2000_2004),
            RAIN_WRF = sum(WRF_Rain_2000_2004),
            SWD_WRF_AVG = mean(WRF_SWD_2000_2004))

write.csv(all_WRF_data_HIS_DAILY, "Z:/Data_analysis/HIS_tif/WRF_HIS_time_series_DAILY.csv")   #hourly climate readings



#################################################################################
############## Ground observations ##############################################
#################################################################################

GROUND_Mezaira_all <- read.csv("Z:/Data_analysis/Ground_Observations/Final/Mezaira03_05.csv")  #RH, Temp, WS not rainfall
GROUND_AbuDhabi_all <- read.csv("Z:/Data_analysis/Ground_Observations/Final/AbuDhabi00_05.csv")
GROUND_AlAin_all <- read.csv("Z:/Data_analysis/Ground_Observations/Final/AlAin00_04.csv")
GROUND_Mezaira_Rainfall <- read.csv("Z:/Data_analysis/Ground_Observations/Final/Mezaira_Rainfall_03_04.csv")
GROUND_AD_AlAin_Rainfall <- read.csv("Z:/Data_analysis/Ground_Observations/Final/Rainfall_AD_Ain_00_04.csv")


############## Join and Unify Ground observations for Mezaira ###########################
#########################################################################################

GROUND_Mezaira_all <- GROUND_Mezaira_all %>%  #all climate variables excluding rain
  mutate(DateTime = mdy_hm(DateTime)) 

GROUND_Mezaira_all <- GROUND_Mezaira_all %>%
  mutate(Date = date(DateTime))

GROUND_Mezaira_all_DAILY <- GROUND_Mezaira_all %>%   
  group_by(Date) %>%
  summarize(WS_AVG = mean(WS),
            TEMP_AVG = mean(Temp),
            RH_AVG = mean(RH),
            MAX_WS = max(WS),
            MAX_TEMP = max(Temp),
            MAX_RH = max(RH),
            MIN_WS = min(WS),
            MIN_RH = min(RH),
            MIN_TEMP = min(Temp))

GROUND_Mezaira_Rainfall$Date <- paste0(GROUND_Mezaira_Rainfall$Year, "-", GROUND_Mezaira_Rainfall$Month, "-",
                                       GROUND_Mezaira_Rainfall$Day)
GROUND_Mezaira_Rainfall <- GROUND_Mezaira_Rainfall %>%
  mutate(Date = ymd(Date))
  
Ground_Mezaira_ALL_rain <- GROUND_Mezaira_all_DAILY %>%          #all climate variables including rain
  left_join(GROUND_Mezaira_Rainfall, by = c("Date"))

Ground_Mezaira_ALL_rain <- Ground_Mezaira_ALL_rain %>%
  dplyr::select(-Year,
                - Month,
                - Day)

Ground_Mezaira_ALL_rain$station <- "MEZAIRA"



############## Join and Unify Ground observations for Abu Dhabi##########################
#########################################################################################

GROUND_AbuDhabi_all <- GROUND_AbuDhabi_all %>%
  mutate(DateTime = mdy_hm(DateTime))

GROUND_AbuDhabi_all <- GROUND_AbuDhabi_all %>%
  mutate(Date = date(DateTime))

GROUND_AbuDhabi_all_DAILY <- GROUND_AbuDhabi_all %>%   
  group_by(Date) %>%
  summarize(WS_AVG = mean(WS),
            TEMP_AVG = mean(Temp),
            RH_AVG = mean(RH),
            MAX_WS = max(WS),
            MAX_TEMP = max(Temp),
            MAX_RH = max(RH),
            MIN_WS = min(WS),
            MIN_RH = min(RH),
            MIN_TEMP = min(Temp))

GROUND_AD_AlAin_Rainfall$Date <- paste0(GROUND_AD_AlAin_Rainfall$Year, "-", GROUND_AD_AlAin_Rainfall$Month, "-",
                                        GROUND_AD_AlAin_Rainfall$Day)
GROUND_AD_AlAin_Rainfall <- GROUND_AD_AlAin_Rainfall %>%
  mutate(Date = ymd(Date))


GROUND_AbuDhabi_ALL_rain <- GROUND_AbuDhabi_all_DAILY %>%          #all climate variables including rain
  left_join(GROUND_AD_AlAin_Rainfall, by = c("Date"))

GROUND_AbuDhabi_ALL_rain <- GROUND_AbuDhabi_ALL_rain %>%
  dplyr::select(-Year,
         - Month,
         - Day,
          - Al.Ain)
names(GROUND_AbuDhabi_ALL_rain)[names(GROUND_AbuDhabi_ALL_rain) == 'Abu.Dhabi'] <- 'Rain'
GROUND_AbuDhabi_ALL_rain$station <- "ABU DHABI INTL"

############## Join and Unify Ground observations for Al Ain   ##########################
#########################################################################################

GROUND_AlAin_all <- GROUND_AlAin_all %>%
  mutate(DateTime = mdy_hm(DateTime))

GROUND_AlAin_all <- GROUND_AlAin_all %>%
  mutate(Date = date(DateTime))

GROUND_AlAin_all_DAILY <- GROUND_AlAin_all %>%   
  group_by(Date) %>%
  summarize(WS_AVG = mean(WS),
            TEMP_AVG = mean(Temp),
            RH_AVG = mean(RH),
            MAX_WS = max(WS),
            MAX_TEMP = max(Temp),
            MAX_RH = max(RH),
            MIN_WS = min(WS),
            MIN_RH = min(RH),
            MIN_TEMP = min(Temp))

GROUND_AD_AlAin_Rainfall$Date <- paste0(GROUND_AD_AlAin_Rainfall$Year, "-", GROUND_AD_AlAin_Rainfall$Month, "-",
                                        GROUND_AD_AlAin_Rainfall$Day)
GROUND_AD_AlAin_Rainfall <- GROUND_AD_AlAin_Rainfall %>%
  mutate(Date = ymd(Date))


GROUND_AlAin_All_rain <- GROUND_AlAin_all_DAILY %>%          #all climate variables including rain
  left_join(GROUND_AD_AlAin_Rainfall, by = c("Date"))

GROUND_AlAin_All_rain <- GROUND_AlAin_All_rain %>%
  dplyr::select(-Year,
                - Month,
                - Day,
                - Abu.Dhabi)
names(GROUND_AlAin_All_rain)[names(GROUND_AlAin_All_rain) == 'Al.Ain'] <- 'Rain'

GROUND_AlAin_All_rain$station <- "AL AIN INTL"

################################################################################
################################################################################

# bind all the data-sets
GROUND_ALL_DAILY <- rbind(GROUND_AlAin_All_rain,
                          GROUND_AbuDhabi_ALL_rain,
                          Ground_Mezaira_ALL_rain)

write.csv(GROUND_ALL_DAILY, "Z:/Data_analysis/HIS_tif/ALL_GROUND_OBSERVATION_DAILY.csv")


##########################################################################################################
######### JOINING WRF HIS runs with the Ground Observations data #########################################
##########################################################################################################

# load HISTORICAL WRF runs and GROUND OBSERVATIONS

WRF_HIS_DAILY <- read.csv("Z:/Data_analysis/HIS_tif/WRF_HIS_time_series_DAILY.csv")
GROUND_ALL_DAILY <- read.csv("Z:/Data_analysis/HIS_tif/ALL_GROUND_OBSERVATION_DAILY.csv")

str(WRF_HIS_DAILY)
str(GROUND_ALL_DAILY)

WRF_HIS_DAILY$Date <- ymd(WRF_HIS_DAILY$Date)
GROUND_ALL_DAILY$Date <- ymd(GROUND_ALL_DAILY$Date)

# join WRF HISTORICAL with GROUND OBSERVATIONS
WRF_HIS_OBSERVATIONS <- WRF_HIS_DAILY %>%          
  left_join(GROUND_ALL_DAILY, by = c("Date", "station"))


write.csv(WRF_HIS_OBSERVATIONS, "Z:/Data_analysis/HIS_tif/ALL_GROUND_WRF_HIS_DAILY.csv")

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
################  TEMPERATURE Time-Series ####################
##############################################################


WRF_HIS_OBSERVATIONS <- read.csv("Z:/Data_analysis/HIS_tif/ALL_GROUND_WRF_HIS_DAILY.csv")

str(WRF_HIS_OBSERVATIONS)
WRF_HIS_OBSERVATIONS$Date <- ymd(WRF_HIS_OBSERVATIONS$Date)

WRF_HIS_OBSERVATIONS <- WRF_HIS_OBSERVATIONS %>%
  filter(TEMP_WRF_AVG > 0) 

WRF_HIS_OBSERVATIONS$Date <- as.POSIXct(WRF_HIS_OBSERVATIONS$Date)

library(ggplot2)
library(scales)

plot <- ggplot(WRF_HIS_OBSERVATIONS, aes(Date, TEMP_WRF_AVG)) +
  theme_bw() +
  geom_line(aes(y = TEMP_WRF_AVG, col = "TEMP_WRF_AVG"), alpha=1, col="blue") +
  geom_line(aes(y = TEMP_AVG, col = "TEMP_AVG"), alpha=1, col="red") +
  scale_color_discrete(name = "Y series", labels = c("TEMP_WRF_AVG", "TEMP_AVG")) +
  stat_smooth(method = "lm") +
  # geom_smooth(method="lm", aes(y = TEMP_AVG, col = "TEMP_AVG"), formula = y ~ poly(x, 26), size = 1, fill = "yellow", col = "black") +  
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Daily Average Temperature ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(10, 45) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
plot


#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/Data_analysis/plots_comparison/"

png(paste0(output_folder,"TREND_Temp_GROUND_HIS_Time_series_comparison.png"), width = 1800, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


#############################################################################

# make an average of all daily concentrations over the last 5 years (seasonality)

OBSERVATIONS_mean <- WRF_HIS_OBSERVATIONS %>%
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date)) %>%
  group_by(month,
           day,
           station) %>%
  summarise(daily_SEASONALITY_TEMP_mean = mean(TEMP_AVG, na.rm = TRUE))


seasonality_means <- rbind(OBSERVATIONS_mean[1:1083,],
                           OBSERVATIONS_mean[1:1092,],
                           OBSERVATIONS_mean[1:1089,],
                           OBSERVATIONS_mean[1:1089,],
                           OBSERVATIONS_mean[1:1092,])

seasonality_means <- as.data.frame(seasonality_means)

WRF_HIS_OBSERVATIONS <- cbind(WRF_HIS_OBSERVATIONS, seasonality_means)


######################################################
# ### fit function and label for Ground Observation ---------
#### this funtion FORCE regression to pass through the origin
######################################################

lm_eqn <- function(df){
  m <- lm(WRF_HIS_OBSERVATIONS ~ -1 + seasonality_means, df);
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(b = format(coef(m)[1], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


## this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#####################################################################################

# plot with regression line-----

# # jpeg('Z:/Data_analysis/plots_comparison/Ground_vs_HIS_WRF.jpg',
# jpeg('Z:/Data_analysis/plots_comparison/Ground_vs_HIS_WRF.jpg',    
#      quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# # define regression equation for each season
# eq <- ddply(seasonality_means, .(OBSERVATIONS_mean),lm_eqn)
# 
# 
# # ggplot(PM25_AOD, aes(x=Val_AOD, y=Val_PM25, color = season)) +
# ggplot(PM25_AOD, aes(x=Val_AOD, y=Val_PM25)) +
#   theme_bw() +
#   # geom_point(size = 2) +
#   geom_jitter(colour=alpha("black",0.15) ) +
#   facet_grid(season ~ .) +
#   theme( strip.text = element_text(size = 20)) + 
#   scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
#   # geom_smooth(method="lm") +  # Add linear regression line
#   geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
#   ylab(expression(paste(PM[2.5], " (µg/",m^3, ")"))) +
#   xlab(expression("AOD (MODIS)")) +
#   ylim(c(0,200)) + 
#   xlim(c(0.25,2)) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=15),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=15)) +
#   theme(axis.title.x = element_text(face="bold", colour="black", size=15),
#         axis.text.x  = element_text(angle=0, vjust=0.5, size=15)) +
#   
#   geom_text(data = eq, aes(x = 1.7, y = 20, label = V1),
#             parse = TRUE, inherit.aes=FALSE, size = 5, color = "black" ) +
#   facet_grid(season ~.)
# 
# # geom_text(x = 1.5, y = 55, label = lm_eqn(PM25_AOD),
# #           size = 5,
# #           color = "red",
# #           parse = TRUE)
# 
# 
# par(oldpar)
# dev.off()
# 
# 
# 
# fit <- lm(Val_PM25 ~ -1 + Val_AOD, PM25_AOD)
# summary(fit)

###############################################################
##############################################################



######################################################
# fit the smoothing curve for the seasonality data ###
######################################################



AAA <- WRF_HIS_OBSERVATIONS
AAA$Date <- as.numeric(AAA$Date)  # need Date to be numeric
fit <- lm(formula = AAA$daily_SEASONALITY_TEMP_mean ~ poly(AAA$Date, 26))
# fit$residuals

smoothed_curve_Temp <- predict(lm(formula = AAA$daily_SEASONALITY_TEMP_mean ~ poly(AAA$Date, 26)))
summary(smoothed_curve_Temp)

#Function for Root Mean Squared Error
RMSE <- function(error) {sqrt(mean(error^2))}
RMSE(fit$residuals) 

#Function for Mean Absolute Error
mae <- function(error) {mean(abs(error))}
mae(fit$residuals)

WRF_HIS_OBSERVATIONS <- cbind(WRF_HIS_OBSERVATIONS, smoothed_curve_Temp)


WRF_HIS_OBSERVATIONS$BIAS_Temp <- WRF_HIS_OBSERVATIONS$TEMP_AVG - WRF_HIS_OBSERVATIONS$smoothed_curve_Temp
# WRF_HIS_OBSERVATIONS$BIAS <- WRF_HIS_OBSERVATIONS$TEMP_AVG - WRF_HIS_OBSERVATIONS$daily_SEASONALITY_TEMP_mean
WRF_HIS_OBSERVATIONS$Corr_TEMP_WRF <- WRF_HIS_OBSERVATIONS$TEMP_WRF_AVG - WRF_HIS_OBSERVATIONS$BIAS_Temp

write.csv(WRF_HIS_OBSERVATIONS, "WRF_HIS_OBSERVATIONS.csv") 


#############################################################################################################
### copy the file WRF_HIS_OBSERVATIONS.csv manually and get the BIAS column saved alone with date and station

WRF_HIS_OBSERVATIONS <- as.data.frame(WRF_HIS_OBSERVATIONS)

# BIAS_Temp <- WRF_HIS_OBSERVATIONS %>%
#   dplyr::select(Date,
#          station,
#          BIAS_Temp)

plot <- ggplot(WRF_HIS_OBSERVATIONS, aes(Date, TEMP_WRF_AVG)) +
  theme_bw() +
  geom_line(aes(y = TEMP_WRF_AVG, col = "TEMP_WRF_AVG"), alpha=1, col="blue") +
  geom_line(aes(y = TEMP_AVG, col = "TEMP_AVG"), alpha=1, col="black") +
 # geom_line(aes(y = smoothed_curve_Temp, col = "smoothed_curve_Temp"), alpha=1, col="black") +
  geom_line(aes(y = Corr_TEMP_WRF, col = "Corr_TEMP_WRF"), alpha=1, col="green") +
  scale_color_discrete(name = "Y series", labels = c("TEMP_WRF_AVG", "TEMP_AVG", "daily_SEASONALITY_TEMP_mean")) +
  # stat_smooth(method = "lm") +
  # geom_smooth(method="lm", aes(y = TEMP_AVG, col = "TEMP_AVG"), formula = y ~ poly(x, 26), size = 1, fill = "yellow", col = "black") +  
  facet_wrap(~ station, nrow=3) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Daily Average Temperature ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(10, 45) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
plot

##############################################################################
#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/Data_analysis/plots_comparison/"

png(paste0(output_folder,"Corrected_Temp_GROUND_HIS_Time_series_comparison.png"), width = 1800, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()

##############################################################################
# plot bias_Temp##############################################################
############################################################################## 

plot <- ggplot(WRF_HIS_OBSERVATIONS, aes(Date, BIAS_Temp)) +
  theme_bw() +
  geom_line(aes(y = BIAS_Temp, col = "BIAS_Temp"), alpha=1, col="blue") +
  scale_color_discrete(name = "Y series", labels = c("BIAS_Temp")) +
  # stat_smooth(method = "lm", col="red") +
  geom_smooth(method="lm", aes(y = BIAS_Temp, col = "BIAS_Temp"), formula = y ~ poly(x, 26), size = 0.5, fill = "yellow", col = "black") +  
  facet_wrap(~ station, nrow=3) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Daily Average Temperature ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(-10, 10) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
plot


output_folder <- "Z:/Data_analysis/plots_comparison/"

png(paste0(output_folder,"BIAS_Temp_Time_series_comparison_WRFHIS_GroundObservations2000_2004.png"), width = 1800, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()




#######################################################################################################
#######################################################################################################
#######################################################################################################
################ Maximum Temperautre Time-Series #####################################################
#######################################################################################################
#######################################################################################################


WRF_HIS_OBSERVATIONS <- read.csv("Z:/Data_analysis/HIS_tif/ALL_GROUND_WRF_HIS_DAILY.csv")

Temp_STATISTICS_2000_2004 <- read.csv("Z:/Data_analysis/HIS_tif/Temp_STATISTICS_2000_2004.csv")
names(Temp_STATISTICS_2000_2004)[names(Temp_STATISTICS_2000_2004) == 'DATE'] <- 'Date'


WRF_HIS_OBSERVATIONS <- WRF_HIS_OBSERVATIONS %>%
  left_join(Temp_STATISTICS_2000_2004, by = c("Date", "station"))

# join MIN and MAX temp ground observations


write.csv(WRF_HIS_OBSERVATIONS, "Z:/Data_analysis/HIS_tif/WRF_HIS_Tmax_time_series_DAILY.csv")   #hourly climate readings


str(WRF_HIS_OBSERVATIONS)
WRF_HIS_OBSERVATIONS$Date <- ymd(WRF_HIS_OBSERVATIONS$Date)

WRF_HIS_OBSERVATIONS <- WRF_HIS_OBSERVATIONS %>%
  filter(daily_MAX_WRF_TEMP > 0)

WRF_HIS_OBSERVATIONS$Date <- as.POSIXct(WRF_HIS_OBSERVATIONS$Date)

library(ggplot2)
library(scales)

plot <- ggplot(WRF_HIS_OBSERVATIONS, aes(Date, daily_MAX_WRF_TEMP)) +
  theme_bw() +
  geom_line(aes(y = daily_MAX_WRF_TEMP, col = "daily_MAX_WRF_TEMP"), alpha=1, col="blue") +
  geom_line(aes(y = MAX_TEMP, col = "MAX_TEMP"), alpha=1, col="red") +
  scale_color_discrete(name = "Y series", labels = c("daily_MAX_WRF_TEMP", "MAX_TEMP")) +
  stat_smooth(method = "lm") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Daily Maximum Temperature ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(10, 45) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
plot
  

#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/Data_analysis/plots_comparison/"

png(paste0(output_folder,"TREND_Tmax_GROUND_HIS_Time_series_comparison.png"), width = 1800, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


#############################################################################


# make an average of all daily concentrations over the last 5 years (seasonality)

OBSERVATIONS_mean <- WRF_HIS_OBSERVATIONS %>%
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date)) %>%
  group_by(month,
           day,
           station) %>%
  summarise(daily_SEASONALITY_Tmax_mean = mean(MAX_TEMP, na.rm = TRUE))


seasonality_means <- rbind(OBSERVATIONS_mean[1:1083,],
                           OBSERVATIONS_mean[1:1092,],
                           OBSERVATIONS_mean[1:1089,],
                           OBSERVATIONS_mean[1:1089,],
                           OBSERVATIONS_mean[1:1092,])

seasonality_means <- as.data.frame(seasonality_means)

WRF_HIS_OBSERVATIONS <- cbind(WRF_HIS_OBSERVATIONS, seasonality_means)


######################################################
# fit the smoothing curve for the seasonality data ###
######################################################

AAA <- WRF_HIS_OBSERVATIONS
AAA$Date <- as.numeric(AAA$Date)  # need Date to be numeric
fit <- lm(formula = AAA$daily_SEASONALITY_Tmax_mean ~ poly(AAA$Date, 26))
# fit$residuals

smoothed_curve_Tmax <- predict(lm(formula = AAA$daily_SEASONALITY_Tmax_mean ~ poly(AAA$Date, 26)))
summary(smoothed_curve_Tmax)


WRF_HIS_OBSERVATIONS <- cbind(WRF_HIS_OBSERVATIONS, smoothed_curve_Tmax)


WRF_HIS_OBSERVATIONS$BIAS_Tmax <- WRF_HIS_OBSERVATIONS$MAX_TEMP - WRF_HIS_OBSERVATIONS$smoothed_curve_Tmax
# WRF_HIS_OBSERVATIONS$BIAS <- WRF_HIS_OBSERVATIONS$MAX_TEMP - WRF_HIS_OBSERVATIONS$daily_SEASONALITY_TEMP_mean
WRF_HIS_OBSERVATIONS$Corr_MAX_TEMP_WRF <- WRF_HIS_OBSERVATIONS$daily_MAX_WRF_TEMP + WRF_HIS_OBSERVATIONS$BIAS_Tmax


write.csv(WRF_HIS_OBSERVATIONS, "WRF_HIS_OBSERVATIONS.csv")

######################### make a copy of the BIAS_Tmax manualy ############################################

plot <- ggplot(WRF_HIS_OBSERVATIONS, aes(Date, daily_MAX_WRF_TEMP)) +
  theme_bw() +
  geom_line(aes(y = daily_MAX_WRF_TEMP, col = "daily_MAX_WRF_TEMP"), alpha=1, col="blue") +
  geom_line(aes(y = MAX_TEMP, col = "MAX_TEMP"), alpha=1, col="black") +
  #geom_line(aes(y = smoothed_curve, col = "smoothed_curve"), alpha=1, col="black") +
  geom_line(aes(y = Corr_MAX_TEMP_WRF, col = "Corr_MAX_TEMP_WRF"), alpha=1, col="green") +
  scale_color_discrete(name = "Y series", labels = c("TEMP_WRF_AVG", "TEMP_AVG", "daily_SEASONALITY_TEMP_mean")) +
# stat_smooth(method = "lm") +
# geom_smooth(method="lm", aes(y = TEMP_AVG, col = "TEMP_AVG"), formula = y ~ poly(x, 26), size = 1, fill = "yellow", col = "black") +  
  facet_wrap(~ station, nrow=3) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Daily Average Temperature ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(10, 45) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
plot



#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/Data_analysis/plots_comparison/"

png(paste0(output_folder,"Corrected_Tmax_GROUND_HIS_Time_series_comparison.png"), width = 1800, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()

##############################################################################
# plot bias_Temp##############################################################
############################################################################## 

plot <- ggplot(WRF_HIS_OBSERVATIONS, aes(Date, BIAS_Tmax)) +
  theme_bw() +
  geom_line(aes(y = BIAS_Tmax, col = "BIAS_Tmax"), alpha=1, col="blue") +
  scale_color_discrete(name = "Y series", labels = c("BIAS_Tmax")) +
  # stat_smooth(method = "lm", col="red") +
  geom_smooth(method="lm", aes(y = BIAS_Tmax, col = "BIAS_Tmax"), formula = y ~ poly(x, 26), size = 0.5, fill = "yellow", col = "black") +  
  facet_wrap(~ station, nrow=3) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Daily Max Temperature Bias ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(-10, 10) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
plot


output_folder <- "Z:/Data_analysis/plots_comparison/"

png(paste0(output_folder,"BIAS_Tmax_Time_series_comparison_WRFHIS_GroundObservations2000_2004.png"), width = 1800, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()



#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
################ Minimum Temperautre Time-Series ######################################################
#######################################################################################################


WRF_HIS_OBSERVATIONS <- read.csv("Z:/Data_analysis/HIS_tif/ALL_GROUND_WRF_HIS_DAILY.csv")

Temp_STATISTICS_2000_2004 <- read.csv("Z:/Data_analysis/HIS_tif/Temp_STATISTICS_2000_2004.csv")
names(Temp_STATISTICS_2000_2004)[names(Temp_STATISTICS_2000_2004) == 'DATE'] <- 'Date'


WRF_HIS_OBSERVATIONS <- WRF_HIS_OBSERVATIONS %>%
  left_join(Temp_STATISTICS_2000_2004, by = c("Date", "station"))

# join MIN and MAX temp ground observations


write.csv(WRF_HIS_OBSERVATIONS, "Z:/Data_analysis/HIS_tif/WRF_HIS_Tmin_time_series_DAILY.csv")   #hourly climate readings


str(WRF_HIS_OBSERVATIONS)
WRF_HIS_OBSERVATIONS$Date <- ymd(WRF_HIS_OBSERVATIONS$Date)

WRF_HIS_OBSERVATIONS <- WRF_HIS_OBSERVATIONS %>%
  filter(daily_MIN_WRF_TEMP > 0)

WRF_HIS_OBSERVATIONS$Date <- as.POSIXct(WRF_HIS_OBSERVATIONS$Date)

library(ggplot2)
library(scales)

plot <- ggplot(WRF_HIS_OBSERVATIONS, aes(Date, daily_MIN_WRF_TEMP)) +
  theme_bw() +
  geom_line(aes(y = daily_MIN_WRF_TEMP, col = "daily_MIN_WRF_TEMP"), alpha=1, col="blue") +
  geom_line(aes(y = MIN_TEMP, col = "MIN_TEMP"), alpha=1, col="red") +
  scale_color_discrete(name = "Y series", labels = c("daily_MIN_WRF_TEMP", "MIN_TEMP")) +
  stat_smooth(method = "lm") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Daily Minimum Temperature ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(0, 40) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
plot


#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/Data_analysis/plots_comparison/"

png(paste0(output_folder,"TREND_Tmin_GROUND_HIS_Time_series_comparison.png"), width = 1800, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


#############################################################################


# make an average of all daily concentrations over the last 5 years (seasonality)

OBSERVATIONS_mean <- WRF_HIS_OBSERVATIONS %>%
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date)) %>%
  group_by(month,
           day,
           station) %>%
  summarise(daily_SEASONALITY_Tmin_mean = mean(MIN_TEMP, na.rm = TRUE))

seasonality_means <- rbind(OBSERVATIONS_mean[1:1083,],
                           OBSERVATIONS_mean[1:1083,],
                           OBSERVATIONS_mean[1:1088,],
                           OBSERVATIONS_mean[1:1088,],
                           OBSERVATIONS_mean[1:1088,])

seasonality_means <- as.data.frame(seasonality_means)

WRF_HIS_OBSERVATIONS <- cbind(WRF_HIS_OBSERVATIONS, seasonality_means)


######################################################
# fit the smoothing curve for the seasonality data ###
######################################################

AAA <- WRF_HIS_OBSERVATIONS
AAA$Date <- as.numeric(AAA$Date)  # need Date to be numeric
fit <- lm(formula = AAA$daily_SEASONALITY_Tmin_mean ~ poly(AAA$Date, 26))
# fit$residuals

smoothed_curve_Tmin <- predict(lm(formula = AAA$daily_SEASONALITY_Tmin_mean ~ poly(AAA$Date, 26)))
summary(smoothed_curve_Tmin)


WRF_HIS_OBSERVATIONS <- cbind(WRF_HIS_OBSERVATIONS, smoothed_curve_Tmin)


WRF_HIS_OBSERVATIONS$BIAS_Tmin <- WRF_HIS_OBSERVATIONS$MIN_TEMP - WRF_HIS_OBSERVATIONS$smoothed_curve_Tmin
# WRF_HIS_OBSERVATIONS$BIAS <- WRF_HIS_OBSERVATIONS$MIN_TEMP - WRF_HIS_OBSERVATIONS$daily_SEASONALITY_TEMP_mean
WRF_HIS_OBSERVATIONS$Corr_MIN_TEMP_WRF <- WRF_HIS_OBSERVATIONS$daily_MIN_WRF_TEMP + WRF_HIS_OBSERVATIONS$BIAS_Tmin


write.csv(WRF_HIS_OBSERVATIONS, "WRF_HIS_OBSERVATIONS.csv")

####################### Make a copy of the BIAS_Tmin manually ###########################


plot <- ggplot(WRF_HIS_OBSERVATIONS, aes(Date, daily_MIN_WRF_TEMP)) +
  theme_bw() +
  geom_line(aes(y = daily_MIN_WRF_TEMP, col = "daily_MIN_WRF_TEMP"), alpha=1, col="blue") +
  geom_line(aes(y = MIN_TEMP, col = "MIN_TEMP"), alpha=1, col="black") +
  #geom_line(aes(y = smoothed_curve, col = "smoothed_curve"), alpha=1, col="black") +
  geom_line(aes(y = Corr_MIN_TEMP_WRF, col = "Corr_MIN_TEMP_WRF"), alpha=1, col="green") +
  scale_color_discrete(name = "Y series", labels = c("daily_MIN_WRF_TEMP", "MIN_TEMP", "daily_SEASONALITY_Tmin_mean")) +
  # stat_smooth(method = "lm") +
  # geom_smooth(method="lm", aes(y = MIN_TEMP, col = "MIN_TEMP"), formula = y ~ poly(x, 26), size = 1, fill = "yellow", col = "black") +  
  facet_wrap(~ station, nrow=3) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Daily Minimum Temperature ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(0, 40) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
plot



#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/Data_analysis/plots_comparison/"

png(paste0(output_folder,"Corrected_Tmin_GROUND_HIS_Time_series_comparison.png"), width = 1800, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


##############################################################################
# plot bias_Temp##############################################################
############################################################################## 

plot <- ggplot(WRF_HIS_OBSERVATIONS, aes(Date, BIAS_Tmin)) +
  theme_bw() +
  geom_line(aes(y = BIAS_Tmin, col = "BIAS_Tmin"), alpha=1, col="blue") +
  scale_color_discrete(name = "Y series", labels = c("BIAS_Tmin")) +
  # stat_smooth(method = "lm", col="red") +
  geom_smooth(method="lm", aes(y = BIAS_Tmin, col = "BIAS_Tmin"), formula = y ~ poly(x, 26), size = 0.5, fill = "yellow", col = "black") +  
  facet_wrap(~ station, nrow=3) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Daily Min Temperature Bias ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(-10, 10) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
plot


output_folder <- "Z:/Data_analysis/plots_comparison/"

png(paste0(output_folder,"BIAS_Tmin_Time_series_comparison_WRFHIS_GroundObservations2000_2004.png"), width = 1800, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()



#######################################################################################################
#######################################################################################################
#######################################################################################################
######################################################################################################
################ Relative Humidity Time-Series #######################################################
######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################


### NOT COMPLETE 

library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
library(NISTunits)
library(stringr)
library(reshape2)
library(stats)


setwd("Z:/Data_analysis")
WRF_HIS_OBSERVATIONS <- read.csv("Z:/Data_analysis/HIS_tif/ALL_GROUND_WRF_HIS_DAILY.csv")

str(WRF_HIS_OBSERVATIONS)
WRF_HIS_OBSERVATIONS$Date <- ymd(WRF_HIS_OBSERVATIONS$Date)

WRF_HIS_OBSERVATIONS <- WRF_HIS_OBSERVATIONS %>%
  filter(RH_WRF_AVG > 0)

WRF_HIS_OBSERVATIONS$Date <- as.POSIXct(WRF_HIS_OBSERVATIONS$Date)

library(ggplot2)
library(scales)

plot <- ggplot(WRF_HIS_OBSERVATIONS, aes(Date, RH_WRF_AVG)) +
  theme_bw() +
  geom_line(aes(y = RH_WRF_AVG, col = "RH_WRF_AVG"), alpha=1, col="blue") +
  geom_line(aes(y = RH_AVG, col = "RH_AVG"), alpha=1, col="red") +
  scale_color_discrete(name = "Y series", labels = c("RH_WRF_AVG", "RH_AVG")) +
  stat_smooth(method = "lm") +
  # geom_smooth(method="lm", aes(y = TEMP_AVG, col = "RH_AVG"), formula = y ~ poly(x, 26), size = 1, fill = "yellow", col = "black") +  
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Daily Average Relative Humidity ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(0, 100) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
plot


#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/Data_analysis/plots_comparison/"

png(paste0(output_folder,"TREND_RH_GROUND_HIS_Time_series_comparison.png"), width = 1800, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


#############################################################################



# make an average of all daily concentrations over the last 5 years (seasonality)

OBSERVATIONS_mean <- WRF_HIS_OBSERVATIONS %>%
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date)) %>%
  group_by(month,
           day,
           station) %>%
  summarise(daily_SEASONALITY_RH_mean = mean(RH_AVG, na.rm = TRUE))


# WRF_HIS_OBSERVATIONS <- WRF_HIS_OBSERVATIONS[order(WRF_HIS_OBSERVATIONS$station),] 

# bind the above averaged to the  data to create a typical seasonal trend
# seasonality_means <- rbind(OBSERVATIONS_mean[1:360, ],    # 2000
#                            OBSERVATIONS_mean[1:364, ],    # 2001
#                            OBSERVATIONS_mean[1:363, ],    # 2002
#                            OBSERVATIONS_mean[1:363, ],    # 2003
#                            OBSERVATIONS_mean[1:365, ])    # 2004




# repeat seasonality by 3 times (3 stations)
# seasonality_means <- rbind(seasonality_means,seasonality_means,seasonality_means)

seasonality_means <- rbind(OBSERVATIONS_mean[1:1083,],
                           OBSERVATIONS_mean[1:1092,],
                           OBSERVATIONS_mean[1:1089,],
                           OBSERVATIONS_mean[1:1089,],
                           OBSERVATIONS_mean[1:1092,])

seasonality_means <- as.data.frame(seasonality_means)

WRF_HIS_OBSERVATIONS <- cbind(WRF_HIS_OBSERVATIONS, seasonality_means)


######################################################
# fit the smoothing curve for the seasonality data ###
######################################################

AAA <- WRF_HIS_OBSERVATIONS
AAA$Date <- as.numeric(AAA$Date)  # need Date to be numeric
fit <- lm(formula = AAA$daily_SEASONALITY_RH_mean ~ poly(AAA$Date, 25))
# fit$residuals

smoothed_curve_RH <- predict(lm(formula = AAA$daily_SEASONALITY_RH_mean ~ poly(AAA$Date, 26)))
summary(smoothed_curve_RH)


WRF_HIS_OBSERVATIONS <- WRF_HIS_OBSERVATIONS[1:5355 , ]
WRF_HIS_OBSERVATIONS <- cbind(WRF_HIS_OBSERVATIONS, smoothed_curve_RH)

WRF_HIS_OBSERVATIONS$BIAS_RH <- WRF_HIS_OBSERVATIONS$RH_AVG - WRF_HIS_OBSERVATIONS$smoothed_curve_RH
# WRF_HIS_OBSERVATIONS$BIAS <- WRF_HIS_OBSERVATIONS$TEMP_AVG - WRF_HIS_OBSERVATIONS$daily_SEASONALITY_TEMP_mean
WRF_HIS_OBSERVATIONS$Corr_RH_WRF <- WRF_HIS_OBSERVATIONS$RH_WRF_AVG - WRF_HIS_OBSERVATIONS$BIAS_RH


write.csv(WRF_HIS_OBSERVATIONS, "WRF_HIS_OBSERVATIONS.csv")

plot <- ggplot(WRF_HIS_OBSERVATIONS, aes(Date, RH_WRF_AVG)) +
  theme_bw() +
  geom_line(aes(y = RH_WRF_AVG, col = "RH_WRF_AVG"), alpha=1, col="blue") +
  geom_line(aes(y = smoothed_curve_RH, col = "smoothed_curve_RH"), alpha=1, col="black") +
  geom_line(aes(y = Corr_RH_WRF, col = "Corr_RH_WRF"), alpha=1, col="green") +
#  scale_color_discrete(name = "Y series", labels = c("RH_WRF_AVG", "RH_AVG", "smoothed_curve_RH")) +
  facet_wrap(~ station, nrow=3) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Daily Average Relative Humidity %"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(0, 100) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
plot



#### save plot ###############################################################
##############################################################################

output_folder <- "Z:/Data_analysis/plots_comparison/"

png(paste0(output_folder,"Corrected_Temp_GROUND_HIS_Time_series_comparison.png"), width = 1800, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()




# plot bias

plot <- ggplot(WRF_HIS_OBSERVATIONS, aes(Date, BIAS)) +
  theme_bw() +
  geom_line(aes(y = BIAS, col = "BIAS"), alpha=1, col="blue") +
  scale_color_discrete(name = "Y series", labels = c("BIAS")) +
  # stat_smooth(method = "lm", col="red") +
  geom_smooth(method="lm", aes(y = BIAS, col = "BIAS"), formula = y ~ poly(x, 26), size = 0.5, fill = "yellow", col = "black") +  
  facet_wrap(~ station, nrow=3) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Daily Average Temperature ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(-10, 10) +
  scale_x_datetime(breaks = date_breaks("1 year"), labels = date_format("%Y"))
plot


output_folder <- "Z:/Data_analysis/plots_comparison/"

png(paste0(output_folder,"BIAS_Time_series_comparison_WRFHIS_GroundObservations2000_2004.png"), width = 1800, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()






