require(shiny)
require(shinydashboard)
require(dplyr)
require(ggplot2)
require(plotly)
require(leaflet)
require(rgdal)
require(htmltools)
require(sf)




taxiData <- read.csv('data/final_taxi_data_V2.csv')

taxiTempDF <- taxiData[, c("pickup_hour", "pickup_day", "pickup_month",
                           "pickup_boro", "dropoff_boro", "tip_amount")]

mainMapDF <- taxiData[, c("pickup_hour", "pickup_day", "pickup_month",
                          "pickup_boro", "dropoff_boro", "tip_amount",
                          "trip_distance", "trip_time_in_secs", "total_amount")]

groupDF1 <- mainMapDF %>% group_by(pickup_boro) %>% summarise(avgTip = mean(tip_amount))
groupDF2 <- mainMapDF %>% group_by(pickup_boro) %>% summarise(avgDist = mean(trip_distance))
groupDF3 <- mainMapDF %>% group_by(pickup_boro) %>% summarise(avgTime = mean(trip_time_in_secs))
groupDF4 <- mainMapDF %>% group_by(pickup_boro) %>% summarise(avgAmt = mean(total_amount))

boroStats <- merge(groupDF1, groupDF2, by = "pickup_boro")
boroStats <- merge(boroStats, groupDF3, by = "pickup_boro")
boroStats <- merge(boroStats, groupDF4, by = "pickup_boro")

nyc = readOGR("data/geo_export_bcadd0d2-8e3b-48bd-a37e-54efd6d35dd3.shp",
              layer="geo_export_bcadd0d2-8e3b-48bd-a37e-54efd6d35dd3")


nycStats <- merge(nyc, boroStats %>% mutate_at(c('avgTip', 'avgDist', 'avgTime', 'avgAmt'), round, 2),
                  by.x = "boro_name", by.y = "pickup_boro")


ui <- dashboardPage(
  
  
  dashboardHeader(title = "NYC Yellow Taxi"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Fun Facts!", tabName = "coverpage", icon = icon("dashboard")),
      menuItem("Explore...", tabName = "mainpage", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML('
    
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 20px;
      }
      
      .content-wrapper, .right-side {
                                background-color: #7da2d1;
                                }
    '))),
    
    
    
    tabItems(
      
      tabItem(tabName = "mainpage",
              
              fluidRow(
                
                box(plotlyOutput("tipBarChart", height = 400)),
                box(plotlyOutput("tipHistogram", height = 400))
                
              ),
              
              fluidRow(
                
                box(
                  
                  title = "How would you like to view?",
                  radioButtons("filter", "Select a filter: ", c("Day of Week", "Month", "Time of Day")),
                  
                  conditionalPanel(condition = "input.filter == 'Day of Week'",
                                   
                                   selectInput("week", "Select Day of Week",
                                               
                                               c("Monday" = "Monday", "Tuesday" = "Tuesday", "Wednesday" = "Wednesday",
                                                 "Thursday" = "Thursday", "Friday" = "Friday", "Saturday" = "Saturday",
                                                 "Sunday" = "Sunday"))),
                  
                  
                  conditionalPanel(condition = "input.filter == 'Month'",
                                   
                                   selectInput("month", "Select Month",
                                               
                                               c("January" = 1, "February" = 2, "March" = 3, "April" = 4,
                                                 "May" = 5, "June" = 6, "July" = 7, "August" = 8, "Spetember" = 9,
                                                 "October" = 10, "November" = 11, "December" = 12))),
                  
                  conditionalPanel(condition = "input.filter == 'Time of Day'",
                                   
                                   selectInput("hour", "Select the Time of Day",
                                               c("0" = 0,  "1" = 1,  "2" = 2,  "3" = 3,  "4" = 4,  "5" = 5,  "6" = 6,
                                                 "7" = 7,  "8" = 8,  "9" = 9,  "10" = 10,  "11" = 11,  "12" = 12,
                                                 "13" = 13,  "14" = 14,  "15" = 15,  "16" = 16,  "17" = 17,  "18" = 18,
                                                 "19" = 19,  "20" = 20,  "21" = 21,  "22" = 22,  "23" = 23,  "24" = 24)))
                ),
                
                
                box(leafletOutput("theMap", height = 500))
                
              )),
      
      tabItem(tabName = "coverpage",
              
              fluidRow(
                
                box(
                  
                  h1("New York City",
                     style = "text-align:center;font-family:verdana;"),
                  
                  p("NYC is a very densly populated city. It is in fact the most populous city in the US. City's population is higher than 40 of the 50 states. Manhattan has a population density of 27,000 people per square kilometer.
                     That's a lot of people and a LOT of taxis.
                     
		    Another thing that is quintessentially New York is the Yello Taxi. You dont have to have visited New York to recognise that taxi thanks to the 100s if not 1000s of movies and tv shows set in New York City. There are more than 13,000 of these in NYC and they service more than 600,000 people every day in New York. It means more than 236 million people in a year.",
                    
                    style = "text-align:center;font-family:verdana;"),
                  
                  
                  img(src = "nyctaxi.jpg", width = 400, style="display: block; margin-left: auto; margin-right: auto;"),
                   
                  
                  
                  h3("Data",
                     style = "text-align:center;font-family:verdana;"),
                  
                  p("Data is collected from these taxis throughout the year. Each taxi ride generates a record containing information like the pick up and drop off cordinates, trip time, distance etc. With millions of cab rides every year that is a lot of data that we can play with",
                    style = "text-align:center;font-family:verdana;"),
                  
                  h3("Tipping", 
                     style = "text-align:center;font-family:verdana;"),
                  
                  
                  p("To tip or not to tip is a debate that has been going on for decades and will continue to do so for a few more decades. But what we know for sure is that tipping is a deeply ingrained concept in the culture and taxi's are no exception. Using the data it would be interesting to know if there are any interesting patterns in the amount of tip a cab rides gives. ",
                    style = "text-align:center;font-family:verdana;"),
                  
                  
                  
                  p("In the next page...We can visualize the data and find out how New Yorkers tip per cab ride.
                    We can answer questions like what day of the week New Yorkers are most generous and a few others. Check it out!!",
                    style = "text-align:center;font-family:verdana;"),
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  ),
                
                
                
                box(leafletOutput("mainMap", height = 800))
                
            )  
              
              
      ))))


server <- function(input, output) {
  
  forHistogram <- reactive({
    
    if (input$filter == "Day of Week") {taxiTempDF[taxiTempDF$pickup_day == input$week,]}
    else if (input$filter == "Month") {taxiTempDF[taxiTempDF$pickup_month == input$month,]}
    else if (input$filter == "Time of Day") {taxiTempDF[taxiTempDF$pickup_hour == input$hour,]}
    
  })
  
  
  forBar <- reactive({
    
    if (input$filter == "Day of Week") {taxiTempDF[taxiTempDF$pickup_day == input$week,] %>% 
        group_by(pickup_boro) %>% summarise(avgTip = mean(tip_amount))}
    
    else if (input$filter == "Month") {taxiTempDF[taxiTempDF$pickup_month == input$month,] %>%
        group_by(pickup_boro) %>% summarise(avgTip = mean(tip_amount))}
    
    else if (input$filter == "Time of Day") {taxiTempDF[taxiTempDF$pickup_hour == input$hour,] %>%
        group_by(pickup_boro) %>% summarise(avgTip = mean(tip_amount))}
    
  })
  
  
  forMap <- reactive({
    
    if (input$filter == "Day of Week") {merge(nyc, taxiTempDF[taxiTempDF$pickup_day == input$week,] %>% 
                                                    group_by(pickup_boro) %>% summarise(avgTip = mean(tip_amount)) %>% 
                                                    mutate_at('avgTip', round, 2),
                                              
                                              by.x = "boro_name",
                                              by.y = "pickup_boro")}
    
    else if (input$filter == "Month") {merge(nyc, taxiTempDF[taxiTempDF$pickup_month == input$month,] %>%
                                                    group_by(pickup_boro) %>% summarise(avgTip = mean(tip_amount)) %>% 
                                                    mutate_at('avgTip', round, 2),
                                             
                                              by.x = "boro_name",
                                              by.y = "pickup_boro")}
    
    else if (input$filter == "Time of Day") {merge(nyc, taxiTempDF[taxiTempDF$pickup_hour == input$hour,] %>%
                                                    group_by(pickup_boro) %>% summarise(avgTip = mean(tip_amount)) %>%
                                                    mutate_at('avgTip', round, 2),
                                                   
                                              by.x = "boro_name",
                                              by.y = "pickup_boro")}
    })
  
  
  
  output$tipHistogram <- renderPlotly({ggplotly(ggplot(forHistogram(), aes(tip_amount)) + geom_histogram(binwidth = 1)  +
      
                                      ylim(0, 40000) +
                                      theme_minimal(base_size = 15) +
                                      ggtitle('Distribution of tips') +
                                      xlab('Tips from 0$ to 5$') +
                                      ylab('Frequency')
                                    
                                    )})

  
  
  
  output$tipBarChart <- renderPlotly({ggplotly(ggplot(forBar(), aes(pickup_boro, avgTip)) + geom_bar(stat = 'identity') +
      
                                      ylim(0, 3) +
                                      theme_minimal(base_size = 15) +
                                      ggtitle('Average Tips by Borough') +
                                      xlab('NYC Borough') +
                                      ylab('Average Tip')
                                  )})
  
  
  output$theMap <- renderLeaflet({leaflet() %>% setView(lng = -74.070978, lat = 40.721276, zoom = 10) %>% 
                                   
                                               addTiles() %>% 
                                               addPolygons(data = forMap(), color = 'black', fillOpacity = 0.6, weight = 2,
                                                           
                                                           label = ~paste0("<b>",boro_name, "</b>", "<br>",
                                                                           "Average Tip: ", "<b>",
                                                                           forMap()$avgTip, "</b>", "<br>") %>% lapply(htmltools::HTML),
                                                           
                                                           labelOptions = labelOptions(textsize = "15px"),
                                               
                                               highlightOptions = highlightOptions(color = 'black',
                                                                                   weight = 3,
                                                                                   bringToFront = TRUE,
                                                                                   opacity = 1,)) %>%
                                              addProviderTiles('Esri.NatGeoWorldMap')})
  
  
  output$mainMap <- renderLeaflet({leaflet() %>% setView(lng = -74.070978, lat = 40.721276, zoom = 10) %>% 
      
                                                 addTiles() %>%
                                                 addPolygons(data = nycStats, color = 'black', fillOpacity = 0.6, weight = 2,
                                                             label = ~paste0("<b>",nycStats$boro_name,"</b>", "<br>", "<br>",
                                                                             "Average Tip: ", "<b>",nycStats$avgTip,"</b>", "<br>",
                                                                             "Average Distance: ","<b>", nycStats$avgDist,"</b>", "<br>",
                                                                             "Average Trip Time: ","<b>", nycStats$avgTime,"</b>", "<br>",
                                                                             "Average Cab Fare: ","<b>", nycStats$avgAmt,"</b>", "<br>") %>%
                                                                             lapply(htmltools::HTML),
                                                             
                                                             labelOptions = labelOptions(textsize = "15px"),
                                                             highlightOptions = highlightOptions(color='black',
                                                                                                 weight=4,
                                                                                                 bringToFront = TRUE,
                                                                                                 opacity = 1,)) %>%
      
                                                 addProviderTiles('Esri.NatGeoWorldMap')})
  
  
  
  
  
}

shinyApp(ui, server)