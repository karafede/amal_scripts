
library(raster)
library(rgdal)
library(stringr)
library(lubridate)
library(leaflet)
library(webshot)
library(htmlwidgets)
library(mapview)

setwd("D:/img_files_prova")

time <- Sys.time()
year <- str_sub(time, start = 0, end = -16)
month <- str_sub(time, start = 6, end = -13)
day <- str_sub(time, start = 9, end = -10)

DATE <- date(time)

# DATE <- paste0(year,month,day)
# DATE <- "20171205"
# DATE <- as.numeric(DATE)

# map <- readGDAL(filenames_T04[1]) 
# map <- as.matrix(map)

# summary(map) 
# image(map) 
# e <- extent(map)

##################

DATE <- "2017-12-11"
DATE <- as.Date(DATE) 

# current_date <- paste0(year,month,day)
current_date <- "20171211"
filenames_T07 <- dir("D:/img_files_prova/T07", pattern = current_date)
filenames_T07 <- filenames_T07[grep(".img", filenames_T07, fixed = T)]
filenames_T09 <- dir("D:/img_files_prova/T09", pattern = current_date)
filenames_T09 <- filenames_T09[grep(".img", filenames_T09, fixed = T)]
filenames_T10 <- dir("D:/img_files_prova/T10", pattern = current_date)
filenames_T10 <- filenames_T10[grep(".img", filenames_T10, fixed = T)]
filenames_T04 <- dir("D:/img_files_prova/T04", pattern = current_date)
filenames_T04 <- filenames_T04[grep(".img", filenames_T04, fixed = T)]
filenames_R01 <- dir("D:/img_files_prova/R01", pattern = current_date)
filenames_R01 <- filenames_R01[grep(".img", filenames_R01, fixed = T)]
filenames_R03 <- dir("D:/img_files_prova/R03", pattern = current_date)
filenames_R03 <- filenames_R03[grep(".img", filenames_R03, fixed = T)]

# DateTime <- str_sub(filenames_T04[1], start = 1, end = -19)

#########################################################################################


# x = 3315 lines
# y = 3712 lines

LON <- seq(from= 30.0055, to = 59.9945, by =0.009047)  #3315
LAT <- seq(from= 10.0065, to = 39.9949, by =0.008079)  #3712
xmn = min(LON)
xmx = max(LON)
ymn = min(LAT)
ymx = max(LAT)

# make an empty matrix
Missing_file <- data.frame(matrix(0, ncol = 1, nrow = 30))


# inizialize an empty raster stack for each DAY  
# all_rasters <- stack() 
count1 <- 0

i <- 1

for (i in 1:length(filenames_T07)) {
  remove(A1, A2)
tryCatch({
  A1 <- readGDAL(paste0("D:/img_files_prova/T07/",filenames_T07[i]))
  A1 <- as.matrix(A1)+273  # conversion into degrees Kelvin
 # image(A1)
  A2 <- readGDAL(paste0("D:/img_files_prova/T09/",filenames_T09[i]))
  A2 <- as.matrix(A2)+273
  count1 <- count1 + 1
}, error= function(err) { print(paste0("no band T07 & T09"))
  
}, finally = {
  
})
  
#  }

count2 <- 0
count3 <- 0


  remove(B1, B2, B3)
  
  tryCatch({
    B1 <- readGDAL(paste0("D:/img_files_prova/T10/",filenames_T10[i]))
    B1 <- as.matrix(B1)+273
  }, error= function(err) { print(paste0("no band T10"))
  }, finally = { 
    count3 <- count3 + 1;
    Missing_file = filenames_T10[i]
  })
  
  tryCatch({
    B2 <- readGDAL(paste0("D:/img_files_prova/T09/",filenames_T09[i]))
    B2 <- as.matrix(B2)+273
    
  }, error= function(err) { print(paste0("no band T09"))
  }, finally = { 
    count3 <- count3 + 1;
    Missing_file = filenames_T09[i]
  })
  
  tryCatch({
    B3 <- readGDAL(paste0("D:/img_files_prova/T07/",filenames_T07[i]))
    B3 <- as.matrix(B3)+273
  }, error= function(err) { print(paste0("no band T07"))
  }, finally = { 
    count3 <- count3 + 1;
    Missing_file = filenames_T07[i]
  })
  
  
  tryCatch({
    A4 <- readGDAL(paste0("D:/img_files_prova/T04/",filenames_T04[i]))
    A4 <- as.matrix(A4)+273
  }, error= function(err) { print(paste0("no band T04"))
  }, finally = { 
    count3 <- count3 + 1;
    Missing_file = filenames_T04[i]
  })
  
  
  tryCatch({
    R01 <- readGDAL(paste0("D:/img_files_prova/R01/",filenames_R01[i]))
    R01 <- as.matrix(R01)+273
  }, error= function(err) { print(paste0("no band R01"))
  }, finally = { 
    count3 <- count3 + 1;
    Missing_file = filenames_R01[i]
  })
  
  
  tryCatch({
    R02 <- readGDAL(paste0("D:/img_files_prova/R02/",filenames_R02[i]))
    R02 <- as.matrix(R02)+273
  }, error= function(err) { print(paste0("no band R02"))
  }, finally = { 
    count3 <- count3 + 1;
    Missing_file = filenames_R01[i]
  })
  
  
  tryCatch({
    R03 <- readGDAL(paste0("D:/img_files_prova/R03/",filenames_R03[i]))
    R03 <- as.matrix(R03)+273
  }, error= function(err) { print(paste0("no band R03"))
  }, finally = { 
    count3 <- count3 + 1;
    Missing_file = filenames_R03[i]
  })
  

  # original Meteofrance Algorithm
  TB039_TB108 = A4 - A2
  TB120_TB108 = B1 - B2
  R006_R016 = R01 / R03
  R01_P3 = R01
  TB087_TB108 = B3 - A2
  Dust_daily_each_time_step <- ((((TB039_TB108 > -10) & (TB120_TB108 > 2.5)) | ((TB039_TB108 > 12) & (TB120_TB108 > 0.6))) | (((TB120_TB108 > -1) & (TB087_TB108 > -1) &  (R006_R016 < 0.8)) | ((TB120_TB108 > -1) & (TB087_TB108 > min(-1,2.5-0.18*R01_P3)) & (R006_R016 < 0.7))))
  
  

  
  # convert logical vector (TRUE & FALSE) into 0 & 1
  Dust_daily_each_time_step <- Dust_daily_each_time_step*2
  max(Dust_daily_each_time_step)
  Dust_daily_each_time_step[Dust_daily_each_time_step == 0] <- 1
  max(Dust_daily_each_time_step)
  
  r1 <- R01*Dust_daily_each_time_step
  r2 <- R02*Dust_daily_each_time_step
  r3 <- R03*Dust_daily_each_time_step
  
  
  r1 <-  t(r1[ , ])
  r1 <- raster(r1, 30.1, 59.9, 10.4,  41.55, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  plot(r1)
  
  r2 <-  t(r2[ , ])
  r2 <- raster(r2, 30.1, 59.9, 10.4,  41.55, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  plot(r2)
  
  r3 <-  t(r3[ , ])
  r3 <- raster(r3, 30.1, 59.9, 10.4,  41.55, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  plot(r3)
  
  # create and RGB image ########
  rgbRaster <- stack(r3,r2,r1)   #RGB == R03, R02, R01 (Red, Gree, Blue)
  # plot an RGB version of the stack
  raster::plotRGB(rgbRaster,r=1,g=2,b=3, stretch = "lin")
  
  
  my_leaflet_map <- leaflet() %>% 
    addTiles() %>% 
    addLayersControl(
      baseGroups = c("Road map", "Toner Lite"),
      # overlayGroups = "SEVIRI",
      options = layersControlOptions(collapsed = TRUE))
  
  map <- mapview::viewRGB(rgbRaster, 1, 2, 3, map = my_leaflet_map)
  map
  
  
  mapshot(map, url = paste0(getwd(), "/map.html"))
  webshot('map.html', file = paste0("D:/img_files_prova/II_Method/",str_sub(filenames_R01[i], 
                                                                  start = 1, end = -19),"_RGB.png"), 
          vwidth = 680, vheight = 803.5,
          cliprect = 'viewport')
  
  # save tif image
  writeRaster(rgbRaster, paste0("D:/img_files_prova/II_Method/",str_sub(filenames_R01[i], start = 1, end = -19),"_RGB.tif") , options= "INTERLEAVE=BAND", overwrite=T)
  
  
}

  
  
  
  
  
  Dust_daily_each_time_step <-  t(Dust_daily_each_time_step[ , ])    # IF map is upside down 
  ####  A1 <- A1[nrow(A1):1, ]
  r <- raster(Dust_daily_each_time_step, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  plot(r)
#  writeRaster(r, "D:/img_files_prova/T04_prova.tif" , options= "INTERLEAVE=BAND", overwrite=T)
  writeRaster(r, paste0("D:/img_files_prova/II_Method/",str_sub(filenames_T07[i], start = 1, end = -19),".tif") , options= "INTERLEAVE=BAND", overwrite=T)
  
  # all_rasters <- stack(all_rasters,r)


pal <- colorNumeric(c("#ff0000"), values(r),
                    na.color = "transparent")

map <- leaflet() %>% 
  addTiles() %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addRasterImage(r, colors = pal)
map

saveWidget(map, 'temp.html', selfcontained = FALSE)
webshot('temp.html', file = paste0("D:/img_files_prova/II_Method/",str_sub(filenames_T07[i], 
                start = 1, end = -19),"_DUST_II_method_OrigMetFrance_FK.png"), 
                vwidth = 750, vheight = 790,
                cliprect = 'viewport')



