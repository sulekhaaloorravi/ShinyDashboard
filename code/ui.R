require(d3heatmap)
require(dygraphs)
require(xts)  
require(ggplot2)
require(plotly)
require(shiny)
require(data.table)
require(forecast)
require(d3scatter)
require(crosstalk)
require(rgl)
require(shinythemes)


fb <- data.frame(fread("data/FoodBalanceSheet.csv",
                       stringsAsFactors = FALSE))
fb$Year <- as.character(fb$Year)
fb$Year <- paste(fb$Year,"12-31",sep="-")
#str(fb)
fb$Year <- as.Date(fb$Year)

fcy <- c("2014-12-31","2015-12-31","2016-12-31","2017-12-31","2018-12-31","2019-12-31",
         "2020-12-31","2021-12-31","2022-12-31","2023-12-31")

fcy <-as.Date(fcy)

fb2<-xts(fb,fb$Year)

Area <- unique(fb$Area)
Date <- as.character(unique(fb$Year))

shinyUI(
navbarPage("Sulekha Aloorravi - Food & Agriculture Org",
           theme = shinytheme("flatly"),
         #  shinythemes::themeSelector(),
           tabPanel("Heatmap",
  sidebarPanel(width=2,
     selectInput("area", label = "Select Country", 
               choices = c(Area), 
               selected = "India"),
    
    selectInput("startdate", label = "Start Date", 
                choices = c(Date), 
                selected = "2000-12-31"),
    
    selectInput("enddate", label = "End Date", 
                choices = c(Date), 
                selected = "2010-12-31"),
       helpText("Problem Statement: Forecast population, food production, wastage, forest area and agricultural area until 2023 for different countries in the world"),
       helpText("This data is acquired from FAO.org."),
    helpText("Charts will show data according to selection."),
    helpText("Chart 1 is an interactive Heatmap"),
    helpText("Chart 2 is an interactive Time series graph")
    
  ),
 mainPanel(
    d3heatmapOutput("heat",width="100%",height = "400px"),
    dygraphOutput("parallel",width="100%",height = "400px")
  )
),
tabPanel("Crosstalk board",
         sidebarPanel(width=2,
           selectInput("ct1", label = "Select 3 Countries", 
                       choices = c(Area), 
                       selected = "India"),
           
           selectInput("ct2", label = "", 
                       choices = c(Area), 
                       selected = "China"),
           
           selectInput("ct3", label = "", 
                       choices = c(Area), 
                       selected = "Argentina"),
           helpText("Select any 3 countries to display their data on charts 1 and 2"),
           helpText("These charts cross talk with each other. Select data in either chart 1 or 2. Notice that all other charts will change."),
           helpText("Chart 1 shows Agricultural Area vs Food Availability"),
           helpText("Chart 2 shows Food Production vs Wastage")
           
           
         ),
         mainPanel(
           fluidRow(
             column(6,
           d3scatterOutput("d3sc1")),
           column(6,
           d3scatterOutput("d3sc2"))
           ),
           fluidRow(
             column(6,
           plotOutput("cross1")),
           column(6,
                  plotOutput("cross2"))
           )
         )
),
tabPanel("Three-Dimensional View",
         sidebarPanel(width=2,
                      selectInput("ca1", label = "Select 3 Countries", 
                                  choices = c(Area), 
                                  selected = "India"),
                      
                      selectInput("ca2", label = "", 
                                  choices = c(Area), 
                                  selected = "China"),
                      
                      selectInput("ca3", label = "", 
                                  choices = c(Area), 
                                  selected = "Argentina"),
                      
                      helpText("This is an interactive 3D Cube showing data for 3 countries as selected"),
                      helpText("Production <=> Food Supply <=> Food Wastage across the countries are displayed"),
                      helpText("Zoom and rotate this cube to view in 3 dimensions")
                      
         ),
         mainPanel(
                    rglwidgetOutput("rgl",width="100%",height="600px")
                    
         )
         ),
         tabPanel("Forecast",
                  sidebarPanel(width=2,
                               selectInput("areafc", label = "Select Country", 
                                           choices = c(Area), 
                                           selected = "India"),
                               
                               selectInput("selectionfc", label = "Select Category", 
                                           choices = c("Population","Food Availability","Agricultural Area", "Forest Area", "Inland Water Area", "Organic Farming", "Planted Forest", "Production", "Waste", "Food Supply per Capita"), 
                                           selected = "Agricultural Area"),
                               helpText("Select each country and each category to see its forecasted data until 2023"),
                               helpText("Data is forecasted based on Arima (Auto Regressive Moving Average) model"),
                               helpText("Chart 1 is an interactive stacked time series graph"),
                               helpText("Chart 2 is an interactive candle chart")
                               
                               
                               
                  ),
                  mainPanel(width=10,
                            fluidRow(
                              
                            dygraphOutput("fcdy",width = "100%", height = "300px"),
                            dygraphOutput("fcdycnd",width="100%",height="300px")
                  )
                  )
         )
)




)

