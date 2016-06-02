##load packages
library(shiny)
library(shinydashboard)

##header
header <- dashboardHeader(title = "My Dashboard")

##sidebar
sidebar <- dashboardSidebar()

##body
body <- dashboardBody()

##create the dashboard
ui<-dashboardPage(header, sidebar, body)

server <- function(input, output) { }

shinyApp(ui, server)