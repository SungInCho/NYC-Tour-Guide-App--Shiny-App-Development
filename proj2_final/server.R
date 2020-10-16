library(tigris)
library(png)
library(dtplyr)
library(dplyr)
library(DT)
library(lubridate)
library(shiny)
library(choroplethr)
library(choroplethrZip)
library(ggplot2)
library(ggmap)
library(readr)
library(plyr)
library(tmap)
library(sf)
library(leaflet)
library(raster)
library(spData)
library(shinydashboard)
library(shinythemes)
library(leaflet.extras)
library(magrittr)
library(gmapsdistance)
library(plotly)
library(googleway)
library(mapview)
library(shinyBS)
library(shinyjs)
library(htmltools)
library(bsplus)
library(shinyWidgets)
library(shinycssloaders)
library(shinycustomloader)
library(shinyFeedback)
library(geosphere)
library(ggthemes)
library(stringr)
library(mapproj)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
register_google(key="AIzaSyC37N09VQDrlBw-myPO42263tqOj_He9xA")

data <- read.csv('FINAL.csv')
filming <- read.csv('Final_Filming.csv')
landmark <- read.csv('Final_Landmarks.csv')
libraries <- read.csv('Final_Libraries.csv')
museums <- read.csv('Final_Museums.csv')
restaurant <- read.csv('Final_Restaurant.csv')
final_data <- merge(filming,landmark,all = T)
final_data <- merge(final_data,libraries,all = T)
final_data <- merge(final_data,museums,all = T)
final_data <- merge(final_data,restaurant,all = T)
good_restaurant <- data.frame(restaurant %>%filter(rating>=4.0)%>%group_by(borough) %>% summarise(n()))
neighborhoods<-read.csv("neighborhood.csv")
nyc_districts_map<-read.csv("nyc_districts_map.csv")
choro<-read.csv("choro.csv")
mids1<-read.csv("mids1.csv")

server <- function(input, output) {
  #################### Map ####################
  output$map1 <- renderLeaflet({
    current_loc <- geocode(input$inaddress)
    dlat <- final_data$lat
    dlon <- final_data$lon
    loc <- cbind(dlon,dlat)
    loc <- as.matrix(loc)
    dist <- distHaversine(loc, current_loc)
    
    data <- final_data[as.logical(match(final_data$type, input$type, nomatch=0)) & dist<input$range*1609.34,]
    
    leaflet(data) %>%
      addTiles() %>%
      addMarkers(lng = ~current_loc$lon,
                 lat = ~current_loc$lat,
                 popup = "You are here") %>%
      addCircles(lng = ~current_loc$lon,
                 lat = ~current_loc$lat,
                 radius = input$range*1609.34) %>%
      setView(-73.968285, 40.785091, zoom=12) %>%
      addMarkers(lng = ~lon, 
                 lat = ~lat, 
                 popup = paste(
                   "Type:", data$type, "<br>",
                   "Name:", data$name, "<br>",
                   "Address:", data$address, "<br>"
                 ),
                 icon=list(iconUrl=paste(input$type,'.png',sep = ""),iconSize=c(18,18)),
                 clusterOptions = markerClusterOptions()
      ) 
  })
  
  output$text1_1 <- renderText({ 
    "Tell us where you are. There are so many great places near you!"
  })
  
  #################### List ####################
  output$map2 <- renderLeaflet({
    land_loc <- geocode(na.omit(as.character(final_data[final_data$name==input$land_list,]$address)))
    film_loc <- geocode(na.omit(as.character(final_data[final_data$name==input$film_list,]$address)))
    museum_loc <- geocode(na.omit(as.character(final_data[final_data$name==input$museum_list,]$address)))
    library_loc <- geocode(na.omit(as.character(final_data[final_data$name==input$library_list,]$address)))
    
    rest_cat <- restaurant[restaurant$categories==input$rest_type,]
    
    rlat <- rest_cat$lat
    rlon <- rest_cat$lon
    rloc <- cbind(rlon,rlat)
    rloc <- as.matrix(rloc)
    r_land_dist <- distHaversine(rloc, land_loc)
    r_film_dist <- distHaversine(rloc, film_loc)
    r_museum_dist <- distHaversine(rloc, museum_loc)
    r_library_dist <- distHaversine(rloc, library_loc)
    
    rest_list <- rest_cat[r_land_dist<input$range_list |
                            r_film_dist<input$range_list |
                            r_museum_dist<input$range_list |
                            r_library_dist<input$range_list,]
    
    leaflet() %>%
      addTiles() %>%
      setView(-73.968285, 40.785091, zoom=12) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addMarkers(data = land_loc,#LANDMARK
                 lng = ~lon, 
                 lat = ~lat,
                 popup = paste("Landmark: ", input$land_list),
                 icon=list(iconUrl='Landmarks.png',iconSize=c(18,18))
      ) %>%
      addCircles(data = land_loc,
                 lng = ~lon,
                 lat = ~lat,
                 radius = input$range_list,
                 popup = paste("Landmark: ", input$land_list),
                 stroke = FALSE,
                 color = "green"
      ) %>% ##
      addMarkers(data = film_loc,# FILM
                 lng = ~lon, 
                 lat = ~lat,
                 popup = paste("Film: ", input$film_list),
                 icon=list(iconUrl='Films.png',iconSize=c(18,18))
      ) %>%
      addCircles(data = film_loc,
                 lng = ~lon,
                 lat = ~lat,
                 radius = input$range_list,
                 popup = paste("Film: ", input$film_list),
                 stroke = FALSE,
                 color ="black"
      ) %>% ##
      addMarkers(data = museum_loc,# MUSEUM
                 lng = ~lon, 
                 lat = ~lat,
                 popup = paste("Museum: ", input$museum_list),
                 icon=list(iconUrl='Museums.png',iconSize=c(18,18))
      ) %>%
      addCircles(data = museum_loc,
                 lng = ~lon,
                 lat = ~lat,
                 radius = input$range_list,
                 popup = paste("Museum: ", input$museum_list),
                 stroke = FALSE,
                 color= "brown"
      ) %>% ##
      addMarkers(data = library_loc,# LIBRARY
                 lng = ~lon, 
                 lat = ~lat,
                 popup = paste("Library: ", input$library_list),
                 icon=list(iconUrl='Libraries.png',iconSize=c(18,18))
      ) %>%
      addCircles(data = library_loc,
                 lng = ~lon,
                 lat = ~lat,
                 radius = input$range_list,
                 popup = paste("Library: ", input$library_list),
                 stroke = FALSE,
                 color="red"
      ) %>% ##
      
      addMarkers(data = rest_list,
                 lng = ~lon, 
                 lat = ~lat, 
                 popup = paste(
                   "Name:", rest_list$name, "<br>",
                   "Category:", rest_list$categories, "<br>",
                   "Rating / Price level:", rest_list$rating, " / ", 
                   rest_list$price, "<br>",
                   "Address:", rest_list$address, "<br>",
                   "Phone #:", rest_list$tel, "<br>"),
                 icon=list(iconUrl='restaurant_red.png',iconSize=c(18,18))
      )
    
  })
  
  ################ Top 10 Restaurants ################
  output$top_rest <- renderTable({
    rest_c <- restaurant[!is.na(numextract(restaurant$inspection.result)),]
    rest_c <- rest_c[rest_c$categories==input$rest_cat,]
    
    # Price
    rest_pr <- rest_c$price
    levels(rest_pr) <- c(4, 3, 2, 1)
    rest_pr <- as.numeric(rest_pr)/4
    
    # Rating
    rest_rat <- rest_c$rating/5
    
    # Inspection score
    rest_insp <- as.numeric(numextract(rest_c$inspection.result))
    rest_insp <- rest_insp+107
    rest_insp <- log(rest_insp[!is.na(rest_insp)])/max(log(rest_insp[!is.na(rest_insp)]))
    
    # Distance score
    loc_score <- geocode(input$rest_address)
    rlat2 <- rest_c$lat
    rlon2 <- rest_c$lon
    rloc2 <- cbind(rlon2, rlat2)
    rloc2 <- as.matrix(rloc2)
    rest_dist <- distHaversine(rloc2, loc_score)
    rest_dist <- (max(rest_dist)-rest_dist)/max(rest_dist)
    
    # Calculating score
    r_i <- input$rest_inspection
    r_r <- input$rest_rate
    r_d <- input$rest_distance
    r_p <- input$rest_price
    r_i <- 0.25*r_i
    r_r <- 0.25*r_r
    r_d <- 0.25*r_d
    r_p <- 0.25*r_p
    
    U.Score <- round(r_i*rest_insp + r_r*rest_rat + r_d*rest_dist + r_p*rest_pr,3)
    
    # table with scores
    rest_c_s <- cbind(rest_c, U.Score)
    colnames(rest_c_s)[14] <- "inspection" 
    final_top_rest <- rest_c_s[,-c(1,3,5,6,7,8,10,13)]
    Ranking <- 1:10
    final_top_rest <- cbind(Ranking,
                            head(final_top_rest[order(U.Score,decreasing=TRUE),],
                                 10))
    final_top_rest
  })
  
  #################### Stats ####################
  restaurant1<-data[which(data$Type=="restaurant"),][sample(1:3165,400),]
  df<-rbind(data[which((data$Type=="film")|(data$Type=="landmarks")|(data$Type=="library")),],restaurant1)
  
  x<-data.frame(df %>%filter((Type=="film")|(Type=="landmarks")|(Type=="library")|(Type=="restaurant"))%>%group_by(Type) %>% summarise(n()))
  y<-data.frame(df %>% filter((Borough=="Brooklyn")|(Borough=="Manhattan")|(Borough=="Queens")|(Borough=="The Bronx"))%>%group_by(Type,Borough) %>% summarise(n()))
  # plot 1
  scatter<-data.frame("X"=c(rep(x[1,2],4),rep(x[2,2],4),rep(x[3,2],3),rep(x[4,2],3)),"Y"=y)
  axis1=list(
    title = "Entertainment Type",
    range = c(100,600),
    autorange = FALSE,                           
    rangemode = "normal",                                                   
    fixedrange = TRUE,
    showticklabels = FALSE
  )
  axis2=list(
    title = "The Number of Size"                       
  )
  p <- plot_ly(scatter, x = ~X, y = ~Y.n.., text = ~Y.Borough, type = 'scatter', mode = 'markers', size = ~Y.n.., color = ~Y.Borough, colors = 'Paired',
               sizes = c(10, 50),
               marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
    layout(title = 'The Number of Sites per Borough',
           xaxis = axis1,
           yaxis = axis2,
           showlegend = TRUE)
  
  #plot 2
  x1<-data.frame(data %>%filter((Type=="film")|(Type=="landmarks")|(Type=="library")|(Type=="restaurant"))%>%group_by(Type) %>% summarise(n()))
  y1<-data.frame(data %>% filter((Borough=="Brooklyn")|(Borough=="Manhattan")|(Borough=="Queens")|(Borough=="The Bronx"))%>%group_by(Type,Borough) %>% summarise(n()))
  scatter_new<-data.frame("X"=c(rep(x1[1,2],4),rep(x1[2,2],4),rep(x1[3,2],3),rep(x1[4,2],3)),"Y"=y1)
  scatter1<-data.frame(scatter,"percent"=round(scatter$Y.n../scatter$X,digit=2))
  p2 <- ggplot() + 
    geom_bar(aes(y = percent, x = Y.Type, fill = Y.Borough), 
             data = scatter1, stat = "identity")+ labs(x = "entertainment type", title = "The distribution of Sites per Borough")+
    theme(plot.title = element_text(hjust = 0.5))
  p3 <- ggplotly(p2)
  #output$plot2<-renderPlot(p3)
  
  #
  output$Plot1<-renderPlotly({
    p
  })
  
  output$Plot2<-renderPlotly({
    p3
  })
  
  
  #heatmap map
  gg <- ggplot()
  gg <- gg + geom_map(data=nyc_districts_map, map=nyc_districts_map,
                      aes(x=long, y=lat, map_id=id),
                      color="#2b2b2b", size=0.15, fill=NA)
  gg <- gg + geom_map(data=choro, map=nyc_districts_map,
                      aes(fill=fill, map_id=district),
                      color="#2b2b2b", size=0.15)
  
  gg <- gg + geom_text(data=mids1, aes(x=x, y=y, label=rating), size=2)
  gg <- gg + geom_text(data=mids1, aes(x=x+0.005, y=y+0.005, label=price), size=2)+labs(title = "Restaurants Information In Mahattan")
  gg<-gg+scale_fill_identity(guide = FALSE) 
  #gg <- gg + scale_fill_identity()
  gg <- gg + coord_map()+theme(plot.title = element_text(hjust = 0.5))
  gg <- gg + ggthemes::theme_map()
  
  output$Plot3 <- renderPlot({
    gg
  })
  
  #pie chart
  output$Plot4 <- renderPlotly({
    plot_ly(good_restaurant, labels = ~borough, values = ~n.., type = 'pie') %>%
      layout(title = 'Good Restaurants Distribution in New York City',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  
  
  #################### Directory ####################
  output$film_dir <- renderDataTable({ 
    data %>% filter(Type=="film")%>%
      select(Name, Year, Director, Address, Borough)
  })
  
  output$land_dir <- renderDataTable({ 
    data %>% filter(Type=="landmarks")%>%
      select(Name, Year, Address, Borough, Number_of_Complaints,Style,Material,Use)
  })
  
  output$lib_dir <- renderDataTable({ 
    data %>% filter(Type=="library")%>%
      select(Name, Address, Borough,System)
  })
  
  output$museum_dir <- renderDataTable({ 
    data %>% filter(Type=="Museum")%>%
      select(Name, Address, Tel,Url)
  })
  
  output$res_dir <- renderDataTable({ 
    data %>% filter(Type=="restaurant")%>%
      select(Name, Address, Borough,Categories,Phone,Rating,Price,Zip_code)
  })
}