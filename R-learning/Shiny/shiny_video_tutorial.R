library(shiny)
library(shinydashboard)
library(leaflet)
library(maps)

ui <- fluidPage("hello world",
                sliderInput(inputId="num",
                  label = "choose a number",
                  value = 25, min=1, max=100),
                plotOutput("hist"),
                apartment
                )

server <- function(input,output){
  output$hist <- renderPlot({
    title <- (input$num)
    hist(rnorm(input$num), main= title)
    })
 output$apartment <- renderLeaflet({mapStates = map("state", fill = TRUE, plot = FALSE)
  apartment = leaflet(data = mapStates) %>% addTiles() %>%
    addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)%>%
    addMarkers(lng=-77.049266, lat=38.902792,popup = "apartment")})
  
}

shinyApp(ui=ui,server=server)


