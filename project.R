setwd('~/R')
denver<-read.csv('listingsproject.csv')
calender<-read.csv('calendar.csv')
reviews<-read.csv('reviews.csv')
install.packages('tidyverse')
install.packages('sf')
install.packages('mapview')
install.packages('rgdal')
install.packages('sp')
install.packages('geojsonio')
library(rgdal)
library(sp)
library(data.table)
library(tidyverse)
library(sf)
library(mapview)
library(ggplot2)
library(maps)
library(geojsonio)
library(ggplot2)

#Neighborhood Count
Neighborhoods = denver %>% count(neighbourhood_cleansed)
Neighborhoods
str(Neighborhoods)

#Property Type
Property_Types= denver %>% count(property_type)
Property_Types

str(denver)
summary(denver)
#Top 10 Neighborhood by Listings
top10<-head(Neighborhoods[order(-Neighborhoods$n),], 10)
top10

top10table<-table(top10)

#Room Type per Neighborhood
ggplot(x) + geom_histogram(aes(denver, fill = room_type ), stat = "count",alpha = 0.85, position = 'fill') + 
  theme_minimal(base_size=13)+ xlab("") + ylab("") + 
  ggtitle("The Proportion of Property Type in Each Area")



#Top 10 Most Popular Neighborhoods
top10visual<-ggplot(data=top10, aes(x=neighbourhood_cleansed, y=n)) +
  geom_bar(stat="identity", fill="forestgreen")+
  theme_minimal()+coord_flip()
top10visual


#Room Type by Neighborhoods
room_types=denver%>%
  group_by(neighbourhood_cleansed,room_type)%>%
  summarise(freq=n())


#Map of Neighborhoods

airbnb=as.data.table(denver)
coordinates(airbnb)=c("longitude","latitude")

#Coordinate reference system
crs.geo1 = CRS("+proj=longlat")  
proj4string(airbnb) = crs.geo1  

plot(airbnb, pch=20, col = "steelblue")

#Read shapefile
denco<-geojson_read("neighbourhoods.geojson",  what = "sp")
plot(denco)
points(airbnb, pch = 20, col = "forestgreen")

