##load packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(maps)

##header
header <- dashboardHeader(title = "US Geography Game")

##sidebar
sidebar <- dashboardSidebar()

##body 
body <- dashboardBody( 
 fluidRow( 
   h3("Guess the Latitude and Longitude of:"),
   h3(textOutput('rand.states1')),
  actionButton("NewState", "New State?")
  ),
 
 fluidPage(leafletOutput('map1')),
 
  h5(numericInput("Lat", label = h5("Latitude"), value = 1)),
  h5(numericInput("Long", label = h5("Longitude"), value = 1))
  )

server <- function(input, output) { 
  
  states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
  output$rand.states1 <- renderText({
    input$NewState 
    sample(states, 1)
    })
  
    mapStates = map("state", fill = TRUE, plot = FALSE)
    mapWorld = map("world", fill = FALSE, plot = FALSE)

    output$map1 <- renderLeaflet({
      leaflet(data = mapStates) %>% addTiles() %>%
        addPolygons(fillColor = topo.colors(3, alpha = NULL), stroke = FALSE)%>%
        addMarkers(lng=input$Long, lat=input$Lat,popup = "You are here")
    })
  
}

##create the dashboard
ui<-dashboardPage(header, sidebar, body)

shinyApp(ui, server)