# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
####################################################################
#install.packages(c("shiny","shinydashboard","ggplot2","data.table"))
##install.packages("ggplot2")
##install.packages("data.table")
##install.packages("maps")
##install.packages("mapproj")
##install.packages("plotly")
##install.packages("sna")
##install.packages("Hmisc")
##install.packages("lubridate")
##install.packages("threejs")
 library(shiny)
 library(shinydashboard)
 library(ggplot2)
 library(data.table)
####################################################################
dbHeader <- dashboardHeader(title = "Customer 1 Dashboard")
dbHeader$children[[2]]$children <-  tags$a(href='http://www.transvoyant.com', tags$img(src='./TransVoyant-small.png',height='20',width='195'))
#tags$style(type="text/css", "#recalculating { opacity: 1.0; }")

shinyUI(dashboardPage(skin = "black",
  dbHeader,
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global Events", tabName = "globe", icon = icon("globe")),
      menuItem("World Map", tabName = "worldmap", icon = icon("map")),
      menuItem("Trend - PoL Anomalies", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Feeds - Anomalies", tabName = "scharts", icon = icon("line-chart")),
      #menuItem("Charts", tabName = "charts", icon = icon("line-chart")),
      menuItem("Data", tabName = "table", icon = icon("columns"))
      
    )
  ),
   dashboardBody(
     tabItems(
       #1st tab - World Globe with spikes
       tabItem(tabName = "globe",
               #             fluidRow(
               #               box(
               #                 title = "Controls",
               #                 shiny::column(12, offset = 0,
               #                               sliderInput("slider2", "Day",
               #                                           min = min(data$Date, na.rm = TRUE), max = max(data$Date, na.rm = TRUE),
               #                                           value = min(data$Date), animate = animationOptions(interval=1000, loop=FALSE), width = '100%') #animate = TRUE
               #                               
               #                               ### COULD ALSO TRY DATE SLIDER INPUT
               #                               #dateRangeInput(inputId, label, start = NULL, end = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", 
               #                               #               startview = "month", weekstart = 0, language = "en", separator = " to ", width = NULL)
               #                               
               #                               # Animation with custom interval (in ms) to control speed, plus looping
               #                               #sliderInput("animation", "Looping Animation:", 1, 2000, 1, step = 10, 
               #                               #           animate=animationOptions(interval=300, loop=T))
               #                 )
               #                 
               #               )
               #             ),
               
               fluidRow(
                 box(globeOutput("plot13", width = "100%", height = "600px"), width = 12
                     #tags$style(type="text/css", "#plot13.recalculating { opacity: 1.0; }")
                    ) 
                   ),
               fluidRow(
                 box(plotly::plotlyOutput("plot1", width = "auto", height = "300px"), width = 12,
                     tags$style(type="text/css", "#plot1.recalculating { opacity: 1.0; }"))
               )
               
       ), ### End of tabItem 
       
       #5th tab content
       tabItem(tabName = "worldmap",
#                fluidRow(
#                  box(
#                    title = "Controls",
#                    shiny::column(12, offset = 0,
#                                  sliderInput("slider2", "Day",
#                                              min = min(data$Date, na.rm = TRUE), max = max(data$Date, na.rm = TRUE),
#                                              value = min(data$Date), animate = animationOptions(interval=1000, loop=FALSE),
#                                              width = '100%', timeFormat = "%F")
#                                  
#                                  ### COULD ALSO TRY DATE SLIDER INPUT
#                                  #dateRangeInput(inputId, label, start = NULL, end = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", 
#                                  #               startview = "month", weekstart = 0, language = "en", separator = " to ", width = NULL)
#                                  
#                                  # Animation with custom interval (in ms) to control speed, plus looping
#                                  #sliderInput("animation", "Looping Animation:", 1, 2000, 1, step = 10, 
#                                  #           animate=animationOptions(interval=300, loop=T))
#                    )
#                    
#                  )
#                ),
               
               fluidRow(
                 box(plotly::plotlyOutput("plot12", width = "100%", height = "600px"), width = 12,
                     tags$style(type="text/css", "#plot12.recalculating { opacity: 1.0; }")) 
               ),
               
              fluidRow(
                 box(plotOutput("plotVar", width = "auto", height = "300px"), width = 12,
                     tags$style(type="text/css", "#plotVar.recalculating { opacity: 1.0; }"))
              )

       ), ### End of tabItem
      
      # 1st tab content
      tabItem(tabName = "dashboard",
#               fluidRow(
#                 box(
#                   title = "Controls",
#                     shiny::column(12, offset = 0,
#                                   sliderInput("slider", "Month",
#                                               min = min(data$month, na.rm = TRUE), max = max(data$month, na.rm = TRUE),
#                                               step = 1, value = min(data$month), animate = animationOptions(interval=1500, loop=FALSE),
#                                               width = '100%')
#                                   
#                     ### COULD ALSO TRY DATE SLIDER INPUT
#                     #dateRangeInput(inputId, label, start = NULL, end = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", 
#                     #               startview = "month", weekstart = 0, language = "en", separator = " to ", width = NULL)
#                     )
#                   
#                 )
#               ),
              
              fluidRow(
                box(plotly::plotlyOutput("anomalyMap", width = "auto", height = "600px"), width = 12,
                    tags$style(type="text/css", "#anomalyMap.recalculating { opacity: 1.0; }"))
              ),
              
              fluidRow(
                box(plotOutput("anomalyTrend", width = "auto", height = "300px"), width = 12,
                    tags$style(type="text/css", "#anomalyTrend.recalculating { opacity: 1.0; }"))
              )#,
              
#               fluidRow(
#                 box(plotly::plotlyOutput("plot2", width = "100%", height = "300px"),
#                     tags$style(type="text/css", "#plot2.recalculating { opacity: 1.0; }")),
#                 box(plotOutput("plot3", width = "100%", height = "300px"),
#                     tags$style(type="text/css", "#plot3.recalculating { opacity: 1.0; }"))
#               )
      ),
      
      # 2nd tab content
      tabItem(tabName = "scharts",
              
              fluidRow(
                box(plotOutput("anomalyTrend5", width = "auto", height = "200px"), width = 6, 
                    tags$style(type="text/css", "#anomalyTrend5.recalculating { opacity: 1.0; }")), #note width = 6 divides into halves
                box(plotOutput("splot5", width = "auto", height = "200px"), width = 6,
                    tags$style(type="text/css", "#splot5.recalculating { opacity: 1.0; }"))
              ),
              fluidRow(
                box(plotOutput("anomalyTrend6", width = "auto", height = "200px"), width = 6, 
                    tags$style(type="text/css", "#anomalyTrend5.recalculating { opacity: 1.0; }")),
                box(plotOutput("splot6", width = "auto", height = "200px"), width = 6,
                    tags$style(type="text/css", "#splot6.recalculating { opacity: 1.0; }"))
              ),
              fluidRow(
                box(plotOutput("anomalyTrend11", width = "auto", height = "200px"), width = 6,
                    tags$style(type="text/css", "#anomalyTrend11.recalculating { opacity: 1.0; }")),
                box(plotOutput("splot11", width = "auto", height = "200px"), width = 6,
                    tags$style(type="text/css", "#splot11.recalculating { opacity: 1.0; }"))
              ),
              fluidRow(
                box(plotOutput("anomalyTrend10", width = "auto", height = "200px"), width = 6,
                    tags$style(type="text/css", "#anomalyTrend10.recalculating { opacity: 1.0; }")),
                box(plotOutput("splot10", width = "auto", height = "200px"), width = 6,
                    tags$style(type="text/css", "#splot10.recalculating { opacity: 1.0; }"))
              ),
              fluidRow(
                box(plotOutput("anomalyTrend7", width = "auto", height = "200px"), width = 6,
                    tags$style(type="text/css", "#anomalyTrend7.recalculating { opacity: 1.0; }")),
                box(plotOutput("splot7", width = "auto", height = "200px"), width = 6,
                    tags$style(type="text/css", "#splot7.recalculating { opacity: 1.0; }"))
              ),
              fluidRow(
                box(plotOutput("anomalyTrend8", width = "auto", height = "200px"), width = 6,
                    tags$style(type="text/css", "#anomalyTrend8.recalculating { opacity: 1.0; }")),
                box(plotOutput("splot8", width = "auto", height = "200px"), width = 6,
                    tags$style(type="text/css", "#splot8.recalculating { opacity: 1.0; }"))
              ),
              fluidRow(
                box(plotOutput("anomalyTrend9", width = "auto", height = "200px"), width = 6,
                    tags$style(type="text/css", "#anomalyTrend9.recalculating { opacity: 1.0; }")),
                box(plotOutput("splot9", width = "auto", height = "200px"), width = 6,
                    tags$style(type="text/css", "#splot9.recalculating { opacity: 1.0; }"))
              )
      ),
      
#     # 3rd tab content
#     tabItem(tabName = "charts",
#             fluidRow(
#               box(
#                 title = "Controls",
#                 shiny::column(12, offset = 0,
#                               sliderInput("slider1", "Month",
#                                           min = min(data$yearmon, na.rm = TRUE), max = max(data$yearmon, na.rm = TRUE),
#                                           value = min(data$yearmon), animate = animationOptions(interval=500, loop=FALSE), 
#                                           width = '100%', timeFormat = "%F")
#                               
#                               ### COULD ALSO TRY DATE SLIDER INPUT
#                               #dateRangeInput("slider1", "Day", start = "2014-12-31", end = "2016-01-01", min = "2014-12-31", max = "2016-01-01", format = "yyyy-mm-dd", 
#                               #               startview = "month", weekstart = 0, language = "en", separator = " to ", width = '100%')
#                               
#                               # Animation with custom interval (in ms) to control speed, plus looping
#                               #sliderInput("animation", "Looping Animation:", 1, 2000, 1, step = 10, 
#                                #           animate=animationOptions(interval=300, loop=T))
#                 )
#               )
#             ),
#             
#             fluidRow(
#               box(plotly::plotlyOutput("plot4", width = "auto", height = "200px"), width = 6, 
#                   tags$style(type="text/css", "#plot4.recalculating { opacity: 1.0; }")), #note width = 6 divides into halves
#               box(plotly::plotlyOutput("plot5", width = "auto", height = "200px"), width = 6,
#                   tags$style(type="text/css", "#plot5.recalculating { opacity: 1.0; }"))
#             ),
#             fluidRow(
#               box(plotly::plotlyOutput("plot6", width = "auto", height = "200px"), width = 6, 
#                   tags$style(type="text/css", "#plot6.recalculating { opacity: 1.0; }")),
#               box(plotly::plotlyOutput("plot7", width = "auto", height = "200px"), width = 6,
#                   tags$style(type="text/css", "#plot7.recalculating { opacity: 1.0; }"))
#             ),
#             fluidRow(
#               box(plotly::plotlyOutput("plot8", width = "auto", height = "200px"), width = 6,
#                   tags$style(type="text/css", "#plot8.recalculating { opacity: 1.0; }")),
#               box(plotly::plotlyOutput("plot9", width = "auto", height = "200px"), width = 6,
#                   tags$style(type="text/css", "#plot9.recalculating { opacity: 1.0; }"))
#             )
#             
#     ),
    
    # 4th tab content
    tabItem(tabName = "table",
            pageWithSidebar(
              headerPanel('Data From All Streams'),
              sidebarPanel(
                checkboxGroupInput('show_vars', 'Columns in Data to show:', names(as.data.frame(data[,c(12,3,6,4,5), with=FALSE])),
                                   selected = names(as.data.frame(data[,c(12,3,6,4,5), with=FALSE]))), #data[,c(12,3,6,7,4,5), with=FALSE]
                helpText('For the data, we can select variables to show in the table;
                         fields available are: Feed, Datetime, Categories, Title, longitude, latitude')
                ),
              mainPanel(
                tabsetPanel(
                  tabPanel('Data', 
                           dataTableOutput("mytable1"))#,
                  #tabPanel('mtcars',
                           #dataTableOutput("mytable2")),
                  #tabPanel('iris',
                           #dataTableOutput("mytable3"))
                )
               )
              )
    )#, End of tabItem

#     #6th tab content
#     tabItem(tabName = "maps1",
#             fluidRow(
#               box(
#                 title = "Controls",
#                 shiny::column(12, offset = 0,
#                               sliderInput("slider2", "Day",
#                                           min = min(data$Date, na.rm = TRUE), max = max(data$Date, na.rm = TRUE),
#                                           value = min(data$Date), animate = animationOptions(interval=1000, loop=FALSE), width = '100%') #animate = TRUE
#                               
#                               ### COULD ALSO TRY DATE SLIDER INPUT
#                               #dateRangeInput(inputId, label, start = NULL, end = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", 
#                               #               startview = "month", weekstart = 0, language = "en", separator = " to ", width = NULL)
#                               
#                               # Animation with custom interval (in ms) to control speed, plus looping
#                               #sliderInput("animation", "Looping Animation:", 1, 2000, 1, step = 10, 
#                               #           animate=animationOptions(interval=300, loop=T))
#                 )
#                 
#               )
#             ),
#             
#             fluidRow(
#               box(plotOutput("plot11", width = "100%", height = "600px"), width = 12,
#                   tags$style(type="text/css", "#plot10.recalculating { opacity: 1.0; }")) 
#             )
#             
#     ) ### End of tabItem
   ) ### End of dashboardBody
  ) ### End of dashboardPage
 )
) ### End of shinyUI

