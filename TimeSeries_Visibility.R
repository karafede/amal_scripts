
library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(ggplot2)
library(sqldf)

setwd("C:/Users/aaldababseh/Pie_Chart")
#df = read.csv('PieChartR.csv')
myts = read.csv('AbuDhabi.csv')

# save a numeric vector containing 33 years hourly observations
# from Jan 1982 to Jan 2016 as a time series object
myts <- ts(myts, start=c(1982, 1), end=c(2016, 1), frequency=288340) 

j <- 4

for(j in 1:length(myts)) {
  
  myts <- open.cvs(myts[j])
  myts <- read.cvs(myts)
  name_vari <- names(myts)
  name_vari
  
  #### only one variable (visibility) == var = 14
  
  var_value<-(WRF_file[14])
  names(var_value) <- "xxyyzz"
  var_value <- (var_value$xxyyzz)
  
}

# plot series
plot(myts)
