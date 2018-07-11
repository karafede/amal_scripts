
###############################################################################
##############################################################################
####################### Wet Pie Charts #######################################
##############################################################################
###############################################################################

library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(ggplot2)
library(sqldf)

setwd("C:/PieChart")
data = read.csv('PieChartR_Wet.csv')

data$Vis_wet_perc <- round(data$Vis_wet_perc)


pie <- ggplot(data = data, aes(x = " ", y = Vis_wet_perc, fill = Classes)) + 
  theme_bw() +
  geom_bar(stat = "identity", position = position_fill(), width = 1) +
  geom_text(aes(label = Vis_wet_perc), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y", start = 0) +
  facet_wrap(~ Station, nrow=2) +
  theme(strip.text = element_text(size = 10)) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid =element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()) +
  theme(legend.position='bottom') +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
   ggtitle("A. Visibility percentage (%) under wet weather conditions") 
pie

pie <- pie +
  scale_fill_manual(values=c("#78A5A3","#ffe996", "#ff9e00", "#89e8f6" ))
pie

#### save plot ###############################################################
##############################################################################

output_folder <- "C:/PieChart/"


png(paste0(output_folder,"Pie_chart_WET.png"), width = 1500, height = 900,
    units = "px", pointsize = 12,
    bg = "white", res = 200)
print(pie)
dev.off()



###############################################################################
##############################################################################
####################### Dry Pie Charts #######################################
##############################################################################
###############################################################################

library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(ggplot2)
library(sqldf)


setwd("C:/PieChart")
data = read.csv('PieChartR_Dry.csv')

data$Vis_dry_perc <- round(data$Vis_dry_perc)


pie <- ggplot(data = data, aes(x = " ", y = Vis_dry_perc, fill = Classes)) + 
  theme_bw() +
  geom_bar(stat = "identity", position = position_fill(), width = 1) +
  geom_text(aes(label = Vis_dry_perc), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y", start = 0) +
  facet_wrap(~ Station, nrow=2) +
  theme(strip.text = element_text(size = 10)) + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid =element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()) +
  theme(legend.position='bottom') +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  ggtitle("B. Visibility percentage (%) under dry weather conditions") 
pie


pie <- pie +
  scale_fill_manual(values=c("#aeb089","#94d0ff","#fed456", "#00a6ff" ))
pie


#pie <- pie +
#scale_fill_manual(values=c("#ae872a","#ffe996", "#997379", "#3cabff" ))
#pie


#### save plot ###############################################################
##############################################################################

output_folder <- "C:/PieChart/"


png(paste0(output_folder,"Pie_chart_DRY.png"), width = 1500, height = 900,
    units = "px", pointsize = 12,
    bg = "white", res = 200)
print(pie)
dev.off()

