library(shiny)
library(shinyWidgets)
library(leaflet)
library(shiny) 
library(tigris)
library(dplyr)
library(plotly)
library(ggplot2)


#setwd('fall2019-proj2--sec1-grp4/app')

data<-read.csv('../output/FINAL.csv',header = TRUE)
restaurant<-data[which(data$Type=="restaurant"),][sample(1:3165,400),]
df<-rbind(data[which((data$Type=="film")|(data$Type=="landmarks")|(data$Type=="library")),],restaurant)


ui <- 
  navbarPage("A Guidance for NYC Travelers",    
             
             tabPanel(
               strong("Stat"),
               fluidPage(
                 sidebarLayout(
                   absolutePanel(NULL, id = "controls", class = "panel panel-default", fixed = TRUE,draggable = TRUE, left = "auto", right = 20,
                                 top = 90, bottom = "auto", width = 250, height = "auto", cursor = "move",
                                 uiOutput("uni_reset", inline = TRUE),
                                 fluidRow(
                                   column(12, align = "center", offset = 0,
                                          actionButton("reset_input2", "Reset"),
                                          tags$style(type = "text/css", "#reset_input2 {width:100%}")
                                   )
                                 )
                   ),
                   mainPanel(
                     width = 10,
                     tabsetPanel(type = "tabs",
                                 tabPanel(strong("Basic Infomation"), radioButtons("basic_info", NULL,
                                                                                   choices = c("plot1",
                                                                                               "plot2"
                                                                                   ), inline = TRUE),
                                          
                                          plotlyOutput("Plot1")),
                                 tabPanel(strong("Details"), radioButtons("details", NULL,
                                                                          choices = c("Films",
                                                                                      "Landmarks",
                                                                                      "Restaurants",
                                                                                      "Museums",
                                                                                      "Libraries"), inline = TRUE),
                                          
                                          #plotlyOutput("satactPlot"),
                                          dataTableOutput("datatable2")))
                     
                     
                   ),
                   position = "right"
                 )
               )
             ),
             
             
             tabPanel("About us")
             
  )



server <- function(input, output) {
  
  #output$Plot1 <- renderPlotly({
  
  #})
  
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
  scatter1<-data.frame(scatter_new,"percent"=round(scatter$Y.n../scatter$X,digit=2))
  p2 <- ggplot() + 
    geom_bar(aes(y = percent, x = Y.Type, fill = Y.Borough), 
             data = scatter1, stat = "identity")+ labs(x = "entertainment type", title = "The distribution of Sites per Borough")+
    theme(plot.title = element_text(hjust = 0.5))
  p3<-ggplotly(p2)
  #output$plot2<-renderPlot(p3)
  
  
  output$Plot1<-renderPlotly({
    if (input$basic_info == "plot1"){
      p
    }
    else if  (input$basic_info == "plot2"){
      p3
    }
  })
  
  
  
  
  
  #table
  output$datatable2 <- renderDataTable(options = list(pageLength = 10, autowidth = TRUE),
                                       {
                                         if (input$details == "Films"){
                                           data %>% filter(Type=="film")%>%
                                             select(Name, Year, Director, Address, Borough)
                                         }
                                         else if(input$details == "Landmarks"){
                                           data %>% filter(Type=="landmarks")%>%
                                             select(Name, Year, Address, Borough, Number_of_Complaints,Style,Material,Use)
                                         }
                                         else if(input$details == "Museums"){
                                           data %>% filter(Type=="Museum")%>%
                                             select(Name, Address, Tel,Url)
                                         }
                                         else if(input$details == "Libraries"){
                                           data %>% filter(Type=="library")%>%
                                             select(Name, Address, Borough,System)
                                         }
                                         else if(input$details == "Restaurants"){
                                           data %>% filter(Type=="restaurant")%>%
                                             select(Name, Address, Borough,Categories,Phone,Rating,Price,Zip_code)
                                         }
                                       }
  )
  
  
}

# Run the application
shinyApp(ui = ui, server = server)