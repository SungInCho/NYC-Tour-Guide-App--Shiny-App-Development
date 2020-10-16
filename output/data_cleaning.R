library(tidyverse)

### Films
filming <- read.csv('../data/Filming Locations.csv')
filming <- filming %>%
  select(Film,Year,Director.Filmmaker.Name,Location.Display.Text,
         LATITUDE,LONGITUDE,Borough,Neighborhood,IMDB.LINK)
colnames(filming) <- c('film','year','director','loc','lat','lon',
                       'borough','nbhd','imdb')
filming$lon <- as.numeric(as.character(filming$lon))
colnames(filming)[1] <-'name'
colnames(filming)[4] <-'address'
filming$type <- 'Films'
write_csv(filming, "../output/Final_Filming.csv")

### Restaurants
restaurant <- read.csv('../data/restaurant_NYC.csv',as.is = T)
colnames(restaurant) <- tolower(colnames(restaurant))
colnames(restaurant)[3] <- 'name'
colnames(restaurant)[4] <- 'borough'
colnames(restaurant)[6:8] <- c('zipcode','lat','lon')
colnames(restaurant)[10] <- 'tel'
restaurant <- restaurant %>%      
  select(type,name,borough,address,zipcode,lat,lon,categories,tel,review_count,rating,price,inspection.date,inspection.result) 
restaurant$type <- 'Restaurants'
write_csv(restaurant, "../output/Final_Restaurant.csv")

### Landmarks
landmark <- read.csv('../output/landmarks_final_clean.csv',as.is = T)
colnames(landmark) <- tolower(colnames(landmark))
colnames(landmark)[10:11] <- c('lat','lon')
landmark$borough[which(landmark$borough=='MN')] <- 'Manhattan' 
landmark$borough[which(landmark$borough=='BK')] <- 'Brooklyn'
landmark$borough[which(landmark$borough=='QN')] <- 'Queens'
landmark$borough[which(landmark$borough=='BX')] <- 'Bronx' 
landmark$type <- 'Landmarks'
landmark$name <- landmark$address
write_csv(landmark, "../output/Final_Landmarks.csv")

### Libraries 
libraries <- read.csv('../output/Library.csv',as.is = T)
colnames(libraries) <- tolower(colnames(libraries))
colnames(libraries)[2] <-'zipcode'
libraries$type <- 'Libraries'
write_csv(libraries, "../output/Final_Libraries.csv")

### Museums
museum <- read.csv('../output/Museum.csv',as.is = T)
colnames(museum) <- tolower(colnames(museum))
museum$type <- 'Museums'
write_csv(museum, "../output/Final_Museums.csv")


## Mahattant Neighborhoods
# heatmap of Mahattan Restaurant
library(rgeos)
library(maptools)
library(base)
library(geojsonio)
library(ggplot2)
library(maps)
library(mapproj)
library(plyr)

#1 data cleaning
# this is the geojson of the NYC community districts
URL <- "http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nycd/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson"
fil <- "nyc_community_districts.geojson"
if (!file.exists(fil)) download.file(URL, fil)
nyc_districts <- geojson_read(fil, what="sp")

# the @data slot of nyc_districts has 2 fields. BoroCD is the district #
nyc_districts_map <- fortify(nyc_districts, region="BoroCD")
# only show mahanttan area
nyc_districts_map<-nyc_districts_map[which((nyc_districts_map$id>100)&(nyc_districts_map$id<200)),]

# let's see which id is what
mids <- cbind.data.frame(as.data.frame(gCentroid(nyc_districts, byid=TRUE)), 
                         id=nyc_districts$BoroCD)
mids<-mids[which((mids$id>100)&(mids$id<200)),]


lat_max<-data.frame(ddply(nyc_districts_map,.(id),function(sub){data.frame(Latitude.max=max(sub$lat))}))
lat_min<-data.frame(ddply(nyc_districts_map,.(id),function(sub){data.frame(Latitude.min=min(sub$lat))}))
long_max<-data.frame(ddply(nyc_districts_map,.(id),function(sub){data.frame(Longitude.max=max(sub$long))}))
long_min<-data.frame(ddply(nyc_districts_map,.(id),function(sub){data.frame(Longitude.min=min(sub$long))}))
neighborhoods<-data.frame(lat_max,lat_min$Latitude.min,long_max$Longitude.max,long_min$Longitude.min,rating=NA,price=NA)
res<-data[which(data$Type=="restaurant"),]
res1<-data.frame(res,price2=NA)
price_fun<-function(i){
  if (res1$Price[i]=='$'){
    return (10)
  }
  else if (res1$Price[i]=='$$')
  {
    return(20)
  }
  else if (res1$Price[i]=='$$$')
  {
    return(30)
  }
  else if (res1$Price[i]=='$$$$')
  {
    return(40)
  }
}

for (i in 1:nrow(res1))
{res1$price2[i]=price_fun(i)}



rating_ave<-function(i){
  df<-res[which((res$Lat>neighborhoods$lat_min.Latitude.min[i])&(res$Lat<neighborhoods$Latitude.max[i])&(res$Lon>neighborhoods$long_min.Longitude.min[i])&(res$Lon<neighborhoods$long_max.Longitude.max[i])),]
  return(round(mean(df$Rating),2))
}



for (i in 1:nrow(neighborhoods))
{neighborhoods$rating[i]=rating_ave(i)}


price_ave<-function(i){
  df<-res1[which((res$Lat>neighborhoods$lat_min.Latitude.min[i])&(res$Lat<neighborhoods$Latitude.max[i])&(res$Lon>neighborhoods$long_min.Longitude.min[i])&(res$Lon<neighborhoods$long_max.Longitude.max[i])),]
  ave<-round(mean(df$price2),2)
  if (ave<16){
    return ('$')
  }
  else if (ave>=16 & ave<21)
  {
    return('$$')
  }
  else if (ave>=21 & ave<26)
  {
    return('$$$')
  }
  
}


for (i in 1:nrow(neighborhoods))
{neighborhoods$price[i]=price_ave(i)}

mids1<-join(mids, neighborhoods, by = 'id')


# write data to csv
write.csv(neighborhoods,file = "/Users/amandan_yanan/Google Drive/学习/学习资料/Semester 3/GR5243--applied data science/project2/neighborhood.csv")
write.csv(nyc_districts_map,file = "/Users/amandan_yanan/Google Drive/学习/学习资料/Semester 3/GR5243--applied data science/project2/nyc_districts_map.csv")
write.csv(choro,file = "/Users/amandan_yanan/Google Drive/学习/学习资料/Semester 3/GR5243--applied data science/project2/choro.csv")
write.csv(mids1,file = "/Users/amandan_yanan/Google Drive/学习/学习资料/Semester 3/GR5243--applied data science/project2/mids1.csv")
