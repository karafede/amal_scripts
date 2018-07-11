
library(stringr)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)


# STATION 1 (field station)--------------------------------------------------

# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/In situ data/5_minute_Masdar_Stations/Station_1")
# 
# station_1 <- read_csv("Masdar_Station_1.csv")
# colnames(station_1) <- c("date", "PM25", "PM10", "PM1", "TSP")
# 
# str(station_1)
# 
# 
# station_1 <- station_1 %>%
#   mutate(date = ymd_hm(date))



setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/In situ data/5_minute_Masdar_Stations/fieldstation")

path <- ".AQL$"
filenames <- list.files(pattern = path)

All_data <- NULL

 i <- 100

for (i in 1:length(filenames)) {


# file <- read.table("20170917.AQL", skip = 1, sep="\t")
  file <- read.table(filenames[i], skip = 1, sep="\t")
# extract hour, minutes, second
file$hour <- str_sub(file$V4, start = 1, end = -7)
file$minute <- str_sub(file$V4, start = 4, end = -4)
file$second <- str_sub(file$V4, start = 7, end = -1)

file$date <- paste0(file$V1, file$V2, file$V3, file$hour, file$minute, file$second)


file <- file %>%
  select(date, V5, V6, V7, V8)
# remove first line
file <- file[-1,]

colnames(file) <- c("date", "TSP", "PM1", "PM25", "PM10")

str(file)
file$date <- as.character(file$date)
file$PM25 <- as.numeric(file$PM25)
file$PM10 <- as.numeric(file$PM10)
file$PM1 <- as.numeric(file$PM1)
file$TSP <- as.numeric(file$TSP)

file <- file %>%
   mutate(date = ymd_hms(date))


# bind all data together
All_data <- rbind(All_data, file)

}
  
 
All_data$PM25_recal <-  (All_data$PM25)*3.69
All_data$PM10_recal <-  (All_data$PM10)*0.76
write.csv(All_data, "field_station_Masdar_18_Oct_2016.csv")

####################################################################
# time series ######################################################

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(splines)
library(plyr)

jpeg('TimeSeries_Station_1_Field.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(station_1, aes(date, PM25)) + 
  theme_bw() +
  geom_line(aes(y = PM25, col = "PM25"), alpha=1, col="red") +
  geom_line(aes(y = PM10, col = "PM10"), alpha=1, col="blue") +
#  facet_wrap( ~ Site, ncol=4) +
  theme(legend.position="none") + 
#  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste(PM[10], " (µg/",m^3, ")", " AQ & WRFChem (hourly)"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=14, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14, colour = "black")) +
  ylim(0, 2000)  
plot


par(oldpar)
dev.off()

### interactive map ######
library(threadr)
library(dygraphs)
library(tidyr)
library(leaflet)
library(readr)
library(lubridate)
library(ggplot2)

# Build timeseries for plots
time_series <- data_frame_to_timeseries(station_1)

# Return
time_series

# make interactive time-series plot
colour_vector <- threadr::ggplot2_colours(45)


plot <- dygraph(time_series$`PM25`) %>% 
  dyOptions(colors = colour_vector[1]) %>% 
  dySeries(label = "PM2.5") %>% 
  dyAxis("y", label = "Daily PM<sub>25</sub> (&#956;g m<sup>-3</sup>)") %>% 
  dyRangeSelector()
plot


##### make overlaps

### then Github

##############################################################################
##############################################################################

# ratio PM1/PM2.5
station_1$ratio_PM1_PM25 <- station_1$PM1/station_1$PM25
mean_ratio_PM1_PM25 <- mean(station_1$ratio_PM1_PM25)

# correlation PM25 vs PM1 ##############################################

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/In situ data/5_minute_Masdar_Stations/Station_1/PM25_vs_PM1_stn1.jpg',    
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


lm_eqn <- function(df){
  m <- lm(PM25 ~ -1 + PM1, station_1);
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(b = format(coef(m)[1], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


ggplot(station_1, aes(x=PM25, y=PM1)) +
  theme_bw() +
  geom_point(size = 2, color='black') + # Use hollow circles
  geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")"))) +
  xlab(expression(paste(PM[1], " (µg/",m^3, ")"))) +
  ylim(c(0,100)) + 
  xlim(c(0,400)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
  geom_text(x = 250, y = 25, label = lm_eqn(station_1),
            size = 5,
            color = "red",
            parse = TRUE)

par(oldpar)
dev.off()

########################################################################

station_1 <- gather(station_1, PM_class, value, 2:5)


jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/In situ data/5_minute_Masdar_Stations/Station_1/field_stn1_hist_Masdar.jpg',    
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

# Overlaid histograms
ggplot(station_1, aes(value, fill = PM_class)) +
  geom_histogram(binwidth=.5, alpha=.5,position="identity") +
  theme_bw() +
  xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
  ylab("count") +
  theme(axis.title.y = element_text(face="bold", colour="black", size=25),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=25)) +
  xlim(0, 300) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=25),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=25, colour = "black", face="bold")) +
  ggtitle(expression(paste("Distribution of particles (Masdar Stn. 1 (Field))"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))


par(oldpar)
dev.off()




jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/In situ data/5_minute_Masdar_Stations/Station_1/field_stn1_dens_Masdar.jpg',    
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


# Overlaid density
ggplot(station_1, aes(value, fill = PM_class)) +
  geom_density(alpha=.5,position="identity") +
  theme_bw() +
  xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
  ylab("density") +
  theme(axis.title.y = element_text(face="bold", colour="black", size=25),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=25)) +
  xlim(0, 300) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=25),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=25, colour = "black", face="bold")) +
  ggtitle(expression(paste("Distribution of particles (Masdar Stn. 1)"))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))



par(oldpar)
dev.off()

# Find the mean of each class of PM
library(plyr)
mean_value <- ddply(na.exclude(station_1), "PM_class", summarise, mean=mean(value))
median_value <- ddply(na.exclude(station_1), "PM_class", summarise, median=median(value))



# With mean lines
  ggplot(station_1, aes(value, fill = PM_class)) +
  geom_histogram(binwidth=.5, alpha=.5,position="identity") +
  facet_grid(PM_class ~ .) +
    theme_bw() +
  geom_vline(data= mean_value, aes(xintercept = mean),
             linetype="dashed", size=0.5, colour="red") +
    xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
    ylab("count") +
    theme(axis.title.y = element_text(face="bold", colour="black", size=25),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=25)) +
    xlim(0, 300) +
    theme(axis.title.x = element_text(face="bold", colour="black", size=25),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=25, colour = "black", face="bold")) +
    ggtitle(expression(paste("Distribution of particles (Masdar Stn. 1 (Field))"))) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))

  
  
  
  
  jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/In situ data/5_minute_Masdar_Stations/Station_1/field_stn1_hist_facets.jpg',    
       quality = 100, bg = "white", res = 200, width = 9, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  

  # With median lines
  ggplot(station_1, aes(value, fill = PM_class)) +
    geom_histogram(binwidth=.5, alpha=.5,position="identity") +
    facet_grid(PM_class ~ .) +
    theme_bw() +
    geom_vline(data= median_value, aes(xintercept = median),
               linetype="dashed", size=0.5, colour="red") +
    xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
    ylab("count") +
    theme(axis.title.y = element_text(face="bold", colour="black", size=20),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
    xlim(0, 300) +
    theme(axis.title.x = element_text(face="bold", colour="black", size=20),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=20, colour = "black", face="bold")) +
    ggtitle(expression(paste("Distribution of particles (Masdar Stn. 1 (Field))"))) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))
  
  
  
  par(oldpar)
  dev.off()
  
  
# ratio PM1/PM2.5


    
# STATION 2 (Masdar Institue)-------------------------------------------------
  
  setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/In situ data/5_minute_Masdar_Stations/Station_2")
  
  station_2 <- read_csv("Masdar_Station_2.csv")
  colnames(station_2) <- c("date", "PM25", "PM10", "PM1", "TSP")
  
  
  station_2 <- station_2 %>%
    mutate(date = ymd_hm(date))
  
  # ratio PM1/PM2.5
  station_2$ratio_PM1_PM25 <- station_2$PM1/station_2$PM25
  mean_ratio_PM1_PM25 <- mean(station_2$ratio_PM1_PM25)
  
  station_2 <- gather(station_2, PM_class, value, 2:5)
  
  # Overlaid histograms
  ggplot(station_2, aes(value, fill = PM_class)) +
    geom_histogram(binwidth=.5, alpha=.5,position="identity") +
    theme_bw() +
    xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
    ylab("count") +
    theme(axis.title.y = element_text(face="bold", colour="black", size=25),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=25)) +
    xlim(0, 300) +
    theme(axis.title.x = element_text(face="bold", colour="black", size=25),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=25, colour = "black", face="bold")) +
    ggtitle(expression(paste("Distribution of particles (Masdar Stn. 2)"))) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))
  
  
  
  # Overlaid histograms
  ggplot(station_2, aes(value, fill = PM_class)) +
    geom_density(alpha=.5,position="identity") +
    theme_bw() +
    xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
    ylab("density") +
    theme(axis.title.y = element_text(face="bold", colour="black", size=25),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=25)) +
    xlim(0, 300) +
    theme(axis.title.x = element_text(face="bold", colour="black", size=25),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=25, colour = "black", face="bold")) +
    ggtitle(expression(paste("Distribution of particles (Masdar Stn. 2)"))) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))
  
  
  
  # Find the mean of each class of PM
  library(plyr)
  mean_value <- ddply(na.exclude(station_2), "PM_class", summarise, mean=mean(value))
  median_value <- ddply(na.exclude(station_2), "PM_class", summarise, median=median(value))
  
  
  
  
  # With mean lines
  ggplot(station_2, aes(value, fill = PM_class)) +
    geom_histogram(binwidth=.5, alpha=.5,position="identity") +
    facet_grid(PM_class ~ .) +
    theme_bw() +
    geom_vline(data= mean_value, aes(xintercept = mean),
               linetype="dashed", size=0.5, colour="red") +
    xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
    ylab("count") +
    theme(axis.title.y = element_text(face="bold", colour="black", size=25),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=25)) +
    xlim(0, 300) +
    theme(axis.title.x = element_text(face="bold", colour="black", size=25),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=25, colour = "black", face="bold")) +
    ggtitle(expression(paste("Distribution of particles (Masdar Stn. 2)"))) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))
  
  
  
  
  jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/In situ data/5_minute_Masdar_Stations/Station_2/Masdar_stn2_hist_facets.jpg',    
       quality = 100, bg = "white", res = 200, width = 9, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  
  
  # With median lines
  ggplot(station_2, aes(value, fill = PM_class)) +
    geom_histogram(binwidth=.5, alpha=.5,position="identity") +
    facet_grid(PM_class ~ .) +
    theme_bw() +
    geom_vline(data= median_value, aes(xintercept = median),
               linetype="dashed", size=0.5, colour="red") +
    xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
    ylab("count") +
    theme(axis.title.y = element_text(face="bold", colour="black", size=20),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
    xlim(0, 300) +
    theme(axis.title.x = element_text(face="bold", colour="black", size=20),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=20, colour = "black", face="bold")) +
    ggtitle(expression(paste("Distribution of particles (Masdar Stn. 2)"))) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))
  

  par(oldpar)
  dev.off()
  
  
  
  # STATION 3 (Beam Down)---------------------------------------------------------

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/In situ data/5_minute_Masdar_Stations/Station_3")
  
  station_3 <- read_csv("Masdar_Station_3.csv")
  colnames(station_3) <- c("date", "PM25", "PM10", "PM1", "TSP")
  
  
  station_3 <- station_3 %>%
    mutate(date = ymd_hm(date))
  
  # ratio PM1/PM2.5
  station_3$ratio_PM1_PM25 <- station_3$PM1/station_3$PM25
  mean_ratio_PM1_PM25 <- mean(station_3$ratio_PM1_PM25)
  
  station_3 <- gather(station_3, PM_class, value, 2:5)
  
  # Overlaid histograms
  ggplot(station_3, aes(value, fill = PM_class)) +
    geom_histogram(binwidth=.5, alpha=.5,position="identity") +
    theme_bw() +
    xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
    ylab("count") +
    theme(axis.title.y = element_text(face="bold", colour="black", size=25),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=25)) +
    xlim(0, 300) +
    theme(axis.title.x = element_text(face="bold", colour="black", size=25),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=25, colour = "black", face="bold")) +
    ggtitle(expression(paste("Distribution of particles (Masdar Stn. 3)"))) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))
  
  
  
  # Overlaid histograms
  ggplot(station_3, aes(value, fill = PM_class)) +
    geom_density(alpha=.5,position="identity") +
    theme_bw() +
    xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
    ylab("density") +
    theme(axis.title.y = element_text(face="bold", colour="black", size=25),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=25)) +
    xlim(0, 300) +
    theme(axis.title.x = element_text(face="bold", colour="black", size=25),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=25, colour = "black", face="bold")) +
    ggtitle(expression(paste("Distribution of particles (Masdar Stn. 3)"))) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))
  
  
  
  # Find the mean of each class of PM
  library(plyr)
  mean_value <- ddply(na.exclude(station_3), "PM_class", summarise, mean=mean(value))
  median_value <- ddply(na.exclude(station_3), "PM_class", summarise, median=median(value))
  
  
  
  # With mean lines
  ggplot(station_3, aes(value, fill = PM_class)) +
    geom_histogram(binwidth=.5, alpha=.5,position="identity") +
    facet_grid(PM_class ~ .) +
    theme_bw() +
    geom_vline(data= mean_value, aes(xintercept = mean),
               linetype="dashed", size=0.5, colour="red") +
    xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
    ylab("count") +
    theme(axis.title.y = element_text(face="bold", colour="black", size=25),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=25)) +
    xlim(0, 300) +
    theme(axis.title.x = element_text(face="bold", colour="black", size=25),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=25, colour = "black", face="bold")) +
    ggtitle(expression(paste("Distribution of particles (Masdar Stn. 3)"))) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))
  
  
  
  
  
  jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/In situ data/5_minute_Masdar_Stations/Station_3/BeamDown_stn3_hist_facets.jpg',    
       quality = 100, bg = "white", res = 200, width = 9, height = 7, units = "in")
  par(mar=c(4, 10, 9, 2) + 0.3)
  oldpar <- par(las=1)
  
  
  # With median lines
  ggplot(station_3, aes(value, fill = PM_class)) +
    geom_histogram(binwidth=.5, alpha=.5,position="identity") +
    facet_grid(PM_class ~ .) +
    theme_bw() +
    geom_vline(data= median_value, aes(xintercept = median),
               linetype="dashed", size=0.5, colour="red") +
    xlab(expression(paste("concentration", " (µg/",m^3, ")"))) +
    ylab("count") +
    theme(axis.title.y = element_text(face="bold", colour="black", size=20),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
    xlim(0, 300) +
    theme(axis.title.x = element_text(face="bold", colour="black", size=20),
          axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 1, size=20, colour = "black", face="bold")) +
    ggtitle(expression(paste("Distribution of particles (BeamDown Stn. 3)"))) + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 30, hjust = 0.5))
  
  par(oldpar)
  dev.off()