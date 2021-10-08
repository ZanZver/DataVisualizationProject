library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)
library(readxl)
library(ggmap)
mydata <- read.csv(file.path("~/Documents/BCU/Year2/DataVisualisation/Assignemnt/Code/NYPD_Arrests_Data__Historic_-2.csv"))
col1 = "#011f4b"
col2 = "#6497b1"
col3 = "#b3cde0"
col4 = "#CC0000"
ggmap::register_google(key = "AIzaSyCtaWFQLGwZFoFM-CYyUBN5Xu-Cs2Dw-qM")

#The Bronx
gBronx<-ggmap(get_googlemap(center =c(lon = -73.864830, lat = 40.844784),
                            zoom =12, scale = 2,maptype ='terrain',color = 'color')) +
  geom_point(data = mydata,
             aes(x = mydata$Longitude, y =  mydata$Latitude),size = 0.1,alpha = 0.01)

ggsave(path = "/Users/zanzver/Documents/BCU/Year2/DataVisualisation/Assignemnt/Code/map", filename = "Bronx.png")

#The Brooklyn
gBrooklyn <- ggmap(get_googlemap(center =c(lon = -73.944160, lat = 40.678177),
                                 zoom =12, scale = 2,maptype ='terrain',color = 'color')) +
  geom_point(data = mydata,
             aes(x = mydata$Longitude, y =  mydata$Latitude),size = 0.1,alpha = 0.01)

ggsave(path = "/Users/zanzver/Documents/BCU/Year2/DataVisualisation/Assignemnt/Code/map", filename = "Brooklyn.png")

#The Manhattan
gManhattan <- ggmap(get_googlemap(center =c(lon = -73.971252, lat = 40.783058),
                                  zoom =12, scale = 2,maptype ='terrain',color = 'color')) +
  geom_point(data = mydata,
             aes(x = mydata$Longitude, y =  mydata$Latitude),size = 0.1,alpha = 0.01)

ggsave(path = "/Users/zanzver/Documents/BCU/Year2/DataVisualisation/Assignemnt/Code/map", filename = "Manhattan.png")

#The Queens
gQueens <- ggmap(get_googlemap(center =c(lon = -73.794853, lat = 40.728226),
                               zoom =12, scale = 2,maptype ='terrain',color = 'color')) +
  geom_point(data = mydata,
             aes(x = mydata$Longitude, y =  mydata$Latitude),size = 0.1,alpha = 0.01)

ggsave(path = "/Users/zanzver/Documents/BCU/Year2/DataVisualisation/Assignemnt/Code/map", filename = "Queens.png")

#The Staten Island
gStatenIsland <- ggmap(get_googlemap(center =c(lon = -74.150200, lat = 40.579533),
                                     zoom =12, scale = 2,maptype ='terrain',color = 'color')) +
  geom_point(data = mydata,
             aes(x = mydata$Longitude, y =  mydata$Latitude),size = 0.1,alpha = 0.01)

ggsave(path = "/Users/zanzver/Documents/BCU/Year2/DataVisualisation/Assignemnt/Code/map", filename = "StatenIsland.png")
