# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
# install.packages("data.table")
# library(shiny)
# library(shinydashboard)
# library(ggplot2)
# library(data.table)

shinyServer(function(input, output) {
  
#####################################################################################
### World Globe With Height Bars at Events Locations...
#####################################################################################
  output$plot13 <- renderGlobe({
    ### Plot a globe with events
    # First set up colors
#     colores<-rep("black",length(nearbyLatLong$lat))
#     colores[which((nearbyLatLong$pop-1)*(49/291) + 1 > 5)]<-"red"
#     # get background image from NASA for pretty aesthetics
#     earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"
#     jpeg(earth,width=2048,height=1024,quality=100,bg="white",antialias="default")
    
    #     globejs(img=earth, bg="white", emissive="#aaaacc", 
    #             lat  = nearbyLatLong[-150<=lon & lon<=-50 & 20<=lat & lat<=60,.(lat)]$lat, 
    #             long = nearbyLatLong[-150<=lon & lon<=-50 & 20<=lat & lat<=60,.(lon)]$lon,
    #             color = "red",
    #             #value = nearbyLatLong[-150<=lon & lon<=-50 & 20<=lat & lat<=60,.(pop)]$pop)
    #             value = (nearbyLatLong[-150<=lon & lon<=-50 & 20<=lat & lat<=60,.(pop)]$pop-1)*(19/130) + 1)
    
    # same as above but doing for all spatial points
    globejs(img=earth, bg="white", emissive="#aaaacc", 
            lat  = nearbyLatLong$lat, 
            long = nearbyLatLong$lon,
            color = colores,
            value = (nearbyLatLong$pop-1)*(49/291) + 1)
  })
  
#######################################################################################################################################
## PLOTS
#######################################################################################################################################
    output$plot1 <- renderPlotly({
    ### All feeds aggregated count - Line trend
#     df<-data[order(month), j=list(count = .N), by = list(month, feed)]
#     df <- df[complete.cases(df),]
#       
#     p<-ggplot(df, aes(x=month,y=count, group=feed)) +
#       xlab("") + ylab("Monthly Total Events") + 
#       scale_x_continuous(breaks = c(seq(1,12)), limits = c(min(df$month, na.rm = T), max(df$month, na.rm = T)), # use max/min(..., na.rm=T) if issues
#                          labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
#       ylim(min(df$count), max(df$count)) +
#       geom_line(data = df[month<=input$slider,], aes(color=feed), size=0.75)
# 
#     ggplotly(p)
    
    ### REMEMBER: THIS IS BY MONTH!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    p<-ggplot(df1, aes(x=month,y=count, group=feed)) +
      xlab("") + ylab("Monthly Total Events") + 
      scale_x_discrete(breaks = seq(1,12), limits = c(min(df1$month, na.rm = T), max(df1$month, na.rm = T)), # use max/min(..., na.rm=T) if issues
                       labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
      ylim(min(df1$count), max(df1$count)) +
      geom_line(aes(color=feed), size=0.75) + 
      ggtitle("Total Events By Month for 2015 (All Feeds)") + xlab("Events") + ylab("Month")
    ggplotly(p)
  })
    
    output$plot12 <- renderPlotly({
      mp <- NULL
      mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
      mp <- ggplot() + mapWorld
      #Now Layer the cities on top (x=long, y=lat)
      mp <- mp + 
        geom_point(data = nearbyLatLong, aes(x=lon, y=lat, size=pop), colour = "red")
      ggplotly(mp)
    })
    
    output$plotVar <- renderPlot({
      
      eb <- aes(ymax = meanCnt + sdCnt, ymin = meanCnt - sdCnt)
      h<-ggplot(data = mergeAll, aes(x = Date, y = meanCnt)) + 
        geom_line(size = 2) + 
        geom_ribbon(eb, alpha = 0.5) +
        # code above works without this additional line below...trying to layer scatter plot of points for all data
        #ggplot(data[, j=list(count = .N), by = Date], aes(Date, count)) +
        #geom_point(data=data[, j=list(meanCnt = .N), by = Date], colour="blue") + #TO GET A SCATTER PLOT OF ALL COUNTS FOR DAY
        geom_point(data=dfVar1, colour="blue") + #TO GET A SCATTER PLOT OF ALL COUNTS FOR DAY
        geom_point(data=dfVar2, colour="red") + # TO GET SCATTER PLOTS OF ONLY THOSE POINTS OUTSIDE 3 SDs
        xlab("") + ylab("Average Daily Events")
      print(h)
    })
    
#   output$plot2 <- renderPlotly({
#     ### All feeds aggregated count - Histogram
#     h<-ggplot(data, aes(x = month)) +
#       scale_x_continuous(breaks = c(seq(1,12)), limits = c(min(data$month, na.rm = T), max(data$month, na.rm = T)+0.5),
#                          labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
#       ylim(0, max(table(data$month))) +
#       geom_histogram(data = data[month<=input$slider,], binwidth = 1/2, fill = "grey50") + 
#       xlab("Month") + ylab("Total Events")
#     
#     #print(h,newpage=F)
#     ggplotly(h)
# 
#     #ggplot(data, aes(month, colour = "#000099")) + xlab("") + ylab("Events") + 
#      # scale_x_continuous(breaks = c(seq(1,12)), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
#      # geom_freqpoly(binwidth = 1)
#   })
  
#   output$plot3 <- renderPlot({
#     ### All feeds aggregated count - Pie chart
#     pie <- ggplot(data, aes(x = factor(1), fill = data$feed)) + 
#            geom_bar(width = 1) + 
#            scale_fill_discrete(name="Feed") + 
#            #coord_polar(theta = "y") + 
#            xlab("") + ylab("") + labs(title = "Distribution of Event Source")
#     
#     print(pie,newpage=F)
#     #ggplotly(pie)
# 
#   })
  
  ### World Map of Anomalies ONLY
  output$anomalyMap <- renderPlotly({
    #Using GGPLOT to get World Map
    mp <- NULL
    mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
    mp <- ggplot() + mapWorld
    #Now Layer the cities on top (x=long, y=lat)
    mp <- mp + 
      geom_point(data = df2, 
                 aes(x=lon, y=lat), colour = "black") +
      geom_point(data = df3, 
                 aes(x=lon, y=lat), colour = "white") +
      geom_point(data = ANnearbyLatLong, aes(x=lon, y=lat, size=pop), colour = "red")
    #plot(mp)
    ggplotly(mp)
  })
  
  # Trend of Anomalies ONLY (i.e., those dates outside 3 sds)
  output$anomalyTrend <- renderPlot({
    p<-ggplot(data[data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) | 
                     data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
                   j=list(count = .N), by = Date], aes(Date, count)) +
      geom_point(colour="red", pch=17, size=2) + 
      geom_point(data = data[!data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) |
                               !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
                             j=list(count = .N), by = Date], colour="grey", pch=20) + 
      xlab("") + ylab("Average Daily Events")
    plot(p)

  })
  
#   #############################################################################################################################################
#   ### PLOTS FOR INDIVIDUAL STATIC CHARTS ######################################################################################################
#   #############################################################################################################################################
#   
#   output$splot4 <- renderPlot({
#     ### Terrorism Feed Only
#     ## STATIC PLOT - DO NOT ERASE - USE FOR REF
#     ggplot(data[data$feed=="Terrorism", j=list(count = .N), by = Date], aes(Date, count)) +
#       #geom_line(aes(group=1), colour="blue") + 
#       geom_line(colour="blue") + 
#       xlab("") + ylab("Terrorism Events")
#   })
#   
#   output$splot5 <- renderPlot({
#     ### Gang Feed Only
#     ## STATIC PLOT - DO NOT ERASE - USE FOR REF
#     ggplot(data[data$feed=="Gangs", j=list(count = .N), by = Date], aes(Date, count)) +
#       #geom_line(aes(group=1), colour="blue") + 
#       geom_line(colour="blue") + 
#       xlab("") + ylab("Daily Gang Events")
#   })
#   
#   output$splot6 <- renderPlot({
#     ### Health Feed Only
#     ## STATIC PLOT - DO NOT ERASE - USE FOR REF
#     ggplot(data[data$feed=="Global Health Hazards", j=list(count = .N), by = Date], aes(Date, count)) +
#       #geom_line(aes(group=1), colour="blue") + 
#       geom_line(colour="blue") + 
#       xlab("") + ylab("Health Hazards Events")
#   })
#   
#   output$splot7 <- renderPlot({
#     ### Hazmat
#     ## STATIC PLOT - DO NOT ERASE - USE FOR REF
#     ggplot(data[data$feed=="US Hazmat Incidents", j=list(count = .N), by = Date], aes(Date, count)) +
#       geom_line(colour="orange") + 
#       xlab("") + ylab("Daily Hazmat Events")
#   })
#   
#   output$splot8 <- renderPlot({
#     ### Global Travel Warnings
#     ## STATIC PLOT - DO NOT ERASE - USE FOR REF
#      ggplot(data[data$feed=="Global Travel Warnings", j=list(count = .N), by = Date], aes(Date, count)) +
#       geom_line(colour="green") + 
#       xlab("") + ylab("Daily Global Travel Warnings")
#   })
#   
#   output$splot9 <- renderPlot({
#     ### US State Dept Alerts & Warnings
#     ## STATIC PLOT - DO NOT ERASE - USE FOR REF
#      ggplot(data[feed=="US State Dept Travel Alerts" | feed=="US State Dept Travel Warnings", j=list(count = .N), by = Date], aes(Date, count)) +
#       geom_line(colour="green") + 
#       xlab("") + ylab("Daily US State Dept Alerts & Warnings")
#   })
  
  #############################################################################################################################################
  ### SUMMIT Static Charts #####################################################################################################
  #############################################################################################################################################
  
#   ### Total Events...see how I can use or Just do not use
#   output$splot4 <- renderPlot({
#     #All feeds
#     ggplot(data[, j=list(count = .N), by = Date], aes(Date, count)) +
#       #geom_line(aes(group=1), colour="blue") + 
#       #geom_line(colour="blue") + #TO GET A LINE/TREND GRAPH
#       geom_point(colour="blue") + #TO GET A SCATTER PLOT OF THE POINTS
#       xlab("") + ylab("Daily Total Events")
#     # 2016-02-19 10:44 - just had the idea that I can use "data[, j=list(count = .N), by = list(Date,feed)]"      
#   })
  
  ### Terrorism
  # Trend
  output$anomalyTrend5 <- renderPlot({
    p5<-ggplot(df5, aes(Date, count)) +
      geom_point(colour="red", pch=17, size=2) + 
      geom_point(data = df5.1, colour="grey", pch=20) + 
      xlab("") + ylab("Anomalous Terrorism Events")
    plot(p5)
  })
  # Map
  output$splot5 <- renderPlot({
    # Terrorism:
    #Using GGPLOT to get World Map
    mp5 <- NULL
    mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
    mp5 <- ggplot() + mapWorld
    mp5 <- mp5 + 
      geom_point(data = ANnearbyLatLong, aes(x=lon, y=lat, size=pop), colour = "red") +
      geom_point(data = df5.2, 
                 aes(x=lon, y=lat), colour = "red") +
      geom_point(data = df5.3, 
                 aes(x=lon, y=lat), colour = "blue") + ggtitle("Anomalous Terrorism Events")
    #ggplotly(mp5)
    plot(mp5)
  })
  
  ### Gangs
  # Trend
  output$anomalyTrend6 <- renderPlot({
    p6<-ggplot(df6, aes(Date, count)) +
      geom_point(colour="red", pch=17, size=2) + 
      geom_point(data = df6.1, colour="grey", pch=20) + 
      xlab("") + ylab("Anomalous Gangs Events")
    plot(p6)
  })
  
  #Map
  output$splot6 <- renderPlot({
    # Gangs:
    #Using GGPLOT to get World Map
    mp6 <- NULL
    mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
    mp6 <- ggplot() + mapWorld
    mp6 <- mp6 + 
      geom_point(data = ANnearbyLatLong, aes(x=lon, y=lat, size=pop), colour = "red") +
      geom_point(data = df6.2, 
                 aes(x=lon, y=lat), colour = "red") +
      geom_point(data = df6.3, 
                 aes(x=lon, y=lat), colour = "blue") + ggtitle("Anomalous Gang Events")
    #ggplotly(mp6)
    plot(mp6)
  })
  
  ### Human Trafficking
  #Trend
  output$anomalyTrend11 <- renderPlot({
    p11<-ggplot(df11, aes(Date, count)) +
      geom_point(colour="red", pch=17, size=2) + 
      geom_point(data = df11.1, colour="grey", pch=20) + 
      xlab("") + ylab("Anomalous Human Trafficking")
    plot(p11)
  })
  
  #Map
  output$splot11 <- renderPlot({
    #Using GGPLOT to get World Map
    mp11 <- NULL
    mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
    mp11 <- ggplot() + mapWorld
    mp11 <- mp11 + 
      geom_point(data = ANnearbyLatLong.1, aes(x=lon, y=lat, size=pop), colour = "red") +
      geom_point(data = df11.2, 
                 aes(x=lon, y=lat), colour = "red") +
      geom_point(data = df11.3, 
                 aes(x=lon, y=lat), colour = "blue") + ggtitle("Anomalous Human Trafficking")
    #ggplotly(mp5)
    plot(mp11)
    
  })
  
  ### Food/Water Shortages
  #Trend
  output$anomalyTrend10 <- renderPlot({
    p10<-ggplot(df10, aes(Date, count)) +
      geom_point(colour="red", pch=17, size=2) + 
      geom_point(data = df10.1, colour="grey", pch=20) + 
      xlab("") + ylab("Anomalous Food/Water Shortages")
    plot(p10)
  })
  
  #Map
  output$splot10 <- renderPlot({
    # Global Travel Warnings
    #Using GGPLOT to get World Map
    mp10 <- NULL
    mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
    mp10 <- ggplot() + mapWorld
    mp10 <- mp10 + 
      geom_point(data = ANnearbyLatLong.1, aes(x=lon, y=lat, size=pop), colour = "red") +
      geom_point(data = df10.2, 
                 aes(x=lon, y=lat), colour = "red") +
      geom_point(data = df10.3, 
                 aes(x=lon, y=lat), colour = "blue") + ggtitle("Anomalous Food/Water Shortages")
    #ggplotly(mp5)
    plot(mp10)
  })
  
  ### Health Hazards
  #Trend
  output$anomalyTrend7 <- renderPlot({
    p7<-ggplot(df7, aes(Date, count)) +
      geom_point(colour="red", pch=17, size=2) + 
      geom_point(data = df7.1, colour="grey", pch=20) + 
      xlab("") + ylab("Anomalous Health Hazards")
    plot(p7)
  })
  
  #Map
  output$splot7 <- renderPlot({
    # Health Hazards
    #Using GGPLOT to get World Map
    mp7 <- NULL
    mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
    mp7 <- ggplot() + mapWorld
    mp7 <- mp7 + 
      geom_point(data = ANnearbyLatLong, aes(x=lon, y=lat, size=pop), colour = "red") +
      geom_point(data = df7.2, 
                 aes(x=lon, y=lat), colour = "red") +
      geom_point(data = df7.3, 
                 aes(x=lon, y=lat), colour = "blue") + ggtitle("Anomalous Health Hazards")
    #ggplotly(mp7)
    plot(mp7)
    
  })
  
  ### Hazmat
  #Trend
  output$anomalyTrend8 <- renderPlot({
    p8<-ggplot(df8, aes(Date, count)) +
      geom_point(colour="red", pch=17, size=2) + 
      geom_point(data = df8.1, colour="grey", pch=20) + 
      xlab("") + ylab("Anomalous Hazmat Incidents")
    plot(p8)
  })
  
  #Map
  output$splot8 <- renderPlot({
    # US Hazmat
    mp8 <- NULL
    mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
    mp8 <- ggplot() + mapWorld
    mp8 <- mp8 + 
      geom_point(data = ANnearbyLatLong, aes(x=lon, y=lat, size=pop), colour = "red") +
      geom_point(data = df8.2, 
                 aes(x=lon, y=lat), colour = "red") +
      geom_point(data = df8.3, 
                 aes(x=lon, y=lat), colour = "blue") + ggtitle("Anomalous Hazmat Incidents")
    #ggplotly(mp8)
    plot(mp8)
    
  })
  
  ### Global Travel Warnings
  #Trend
  output$anomalyTrend9 <- renderPlot({
    p9<-ggplot(df9, aes(Date, count)) +
      geom_point(colour="red", pch=17, size=2) + 
      geom_point(data = df9.1, colour="grey", pch=20) + 
      xlab("") + ylab("Anomalous Travel Warnings")
    plot(p9)
  })
  
  #Map
  output$splot9 <- renderPlot({
    # Global Travel Warnings
    mp9 <- NULL
    mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
    mp9 <- ggplot() + mapWorld
    mp9 <- mp9 + 
      geom_point(data = ANnearbyLatLong, aes(x=lon, y=lat, size=pop), colour = "red") +
      geom_point(data = df9.2, 
                 aes(x=lon, y=lat), colour = "red") +
      geom_point(data = df9.3, 
                 aes(x=lon, y=lat), colour = "blue") + ggtitle("Anomalous Travel Warnings")
    #ggplotly(mp9)
    plot(mp9)

  })
  
#   output$splot10 <- renderPlot({
#     # US State Dept Travel Alerts
# 
#   })
#   
#   output$splot11 <- renderPlot({
#     # US State Dept Travel Warnings
# 
#   })
  
  #############################################################################################################################################
  ### PLOTS FOR INDIVIDUAL DYNAMIC CHARTS #####################################################################################################
  #############################################################################################################################################

#   output$plot4 <- renderPlot({
#     ### Terrorism Feed Only
#     df4<-data[data$feed=="Terrorism" & data$Date<=input$slider1, j=list(count = .N), by = Date]
#     
#     ggplot(df4, aes(x=df4$Date, y=df4$count)) +
#       geom_line(colour="red") + 
#       xlab("") + ylab("Daily Terrorism Events")
# 
#   })
  
#   output$plot4 <- renderPlotly({
#     ### Terrorism Feed Only
#     
#     ## STATIC PLOT - DO NOT ERASE - USE FOR REF
#     #ggplot(data[data$feed=="Terrorism", j=list(count = .N), by = Date], aes(Date, count)) +
#     #  #geom_line(aes(group=1), colour="blue") + 
#     #  geom_line(colour="blue") + 
#     #  xlab("") + ylab("Daily Gang Events")
#     
#     # Set up the temp datasets...
#     df<-data[data$feed=="Terrorism", j=list(count = .N), by = yearmon]
#     df2<-df[df$yearmon<=input$slider1,]
#     # Use conditional logic for when there is no data
#     #if (cond) expr1 else expr2
#     if (input$slider1 < min(df$yearmon)) {
#       g<-ggplot(df, aes(x=yearmon, y=count)) + 
#         geom_line(data = df, aes(x=df$yearmon, y=df$count*0), colour="red") + 
#         xlab("") + ylab("Daily Terrorism Events") +
#         #scale_x_date(limits = c(min(df$Date),max(df$Date))) +
#         ylim(min(df$count), max(df$count))
#       ggplotly(g)
#     } else {
#       g<-ggplot(df, aes(x=yearmon, y=count)) + 
#         xlab("") + ylab("Daily Terrorism Events") +
#         #scale_x_date(limits = c(min(df$Date),max(df$Date))) +
#         ylim(min(df$count), max(df$count)) +
#        geom_line(data = df2, aes(x=df2$yearmon, y=df2$count), colour="red")
#       
#       ggplotly(g)
#     }
# 
#   })
#   
# #   output$plot5 <- renderPlot({
# #     ### Gang Feed Only
# #     df5<-data[data$feed=="Gangs", j=list(count = .N), by = Date]
# #     
# #     ggplot(df5[Date<=input$slider1,], aes(Date, count)) +
# #       #geom_line(aes(group=1), colour="blue") + 
# #       geom_line(colour="blue") + 
# #       xlab("") + ylab("Daily Gang Events")
# #   
# #   })
#   
#   output$plot5 <- renderPlotly({
#     ### Gang Feed Only
#     ## STATIC PLOT - DO NOT ERASE - USE FOR REF
#     #ggplot(data[data$feed=="Gangs", j=list(count = .N), by = Date], aes(Date, count)) +
#     #  #geom_line(aes(group=1), colour="blue") + 
#     #  geom_line(colour="blue") + 
#     #  xlab("") + ylab("Daily Gang Events")
#     #######################################
#     
#     # Set up the temp datasets...
#     df<-data[data$feed=="Gangs", j=list(count = .N), by = yearmon]
#     df2<-df[df$yearmon<=input$slider1,]
#     
#     # Use conditional logic for when there is no data
#     if (input$slider1 < min(df$yearmon)) {
#       g<-ggplot(df, aes(x=yearmon, y=count)) + 
#         geom_line(data = df, aes(x=df$yearmon, y=df$count*0), colour="blue") + 
#         xlab("") + ylab("Daily Gang Events") +
#         #scale_x_yearmon(limits = c(min(df$yearmon),max(df$yearmon))) +
#         ylim(min(df$count), max(df$count))
#       
#       ggplotly(g)
#     } else {
#       g<-ggplot(df, aes(x=yearmon, y=count)) + 
#         xlab("") + ylab("Daily Gang Events") +
#         #scale_x_yearmon(limits = c(min(df$yearmon),max(df$yearmon))) +
#         ylim(min(df$count), max(df$count)) +
#         geom_line(data = df2, aes(x=df2$yearmon, y=df2$count), colour="blue")
#       
#       ggplotly(g)
#     }
#     
#     
#   })
#   
#   output$plot6 <- renderPlotly({
#     ### Health Feed Only
# 
#     #######################################
#     
#     # Set up the temp datasets...
#     df<-data[data$feed=="Global Health Hazards", j=list(count = .N), by = yearmon]
#     df2<-df[df$yearmon<=input$slider1,]
#     
#     # Use conditional logic for when there is no data
# #     if (input$slider1 < min(df$yearmon)) {
# #       g<-ggplot(df, aes(x=yearmon, y=count)) + 
# #         geom_line(data = df, aes(x=df$yearmon, y=df$count*0), colour="yellow") + 
# #         xlab("") + ylab("Daily Global Health Hazards") +
# #         scale_x_yearmon(limits = c(min(df$yearmon),max(df$yearmon))) +
# #         ylim(min(df$count), max(df$count))
# #       
# #       ggplotly(g)
# #     } else {
#       g<-ggplot(df, aes(x=yearmon, y=count)) + 
#         xlab("") + ylab("Daily Global Health Hazards") +
#         #scale_x_date(limits = c(min(df$yearmon),max(df$yearmon))) +
#         ylim(min(df$count), max(df$count)) +
#         geom_line(data = df2, aes(x=df2$yearmon, y=df2$count), colour="yellow")
#       
#       ggplotly(g)
# #    }
#     
#   })
#   
# #   output$plot7 <- renderPlot({
# #     ### Hazmat
# #     df7<-data[data$feed=="US Hazmat Incidents", j=list(count = .N), by = Date]
# #     
# #     ggplot(df7[Date<=input$slider1,], aes(Date, count)) +
# #       geom_line(colour="orange") + 
# #       xlab("") + ylab("Daily Hazmat Events")
# #     
# #   })
#   
#   output$plot7 <- renderPlotly({
#     ### Hazmat
#     ## STATIC PLOT - DO NOT ERASE - USE FOR REF
#     #ggplot(data[data$feed=="US Hazmat Incidents", j=list(count = .N), by = Date], aes(Date, count)) +
#     #  geom_line(colour="orange") + 
#     #  xlab("") + ylab("Daily Hazmat Events")
# 
#     #######################################
#     
#     # Set up the temp datasets...
#     df<-data[data$feed=="US Hazmat Incidents", j=list(count = .N), by = yearmon]
#     df2<-df[df$yearmon<=input$slider1,]
#     
#     # Use conditional logic for when there is no data
#     if (input$slider1 < min(df$yearmon)) {
#       g<-ggplot(df, aes(x=yearmon, y=count)) + 
#         geom_line(data = df, aes(x=df$yearmon, y=df$count*0), colour="orange") + 
#         xlab("") + ylab("Daily Hazmat Events") +
#         #scale_x_yearmon(limits = c(min(df$yearmon),max(df$yearmon))) +
#         ylim(min(df$count), max(df$count))
#       
#       ggplotly(g)
#     } else {
#       g<-ggplot(df, aes(x=yearmon, y=count)) + 
#         xlab("") + ylab("Daily Hazmat Events") +
#         #scale_x_yearmon(limits = c(min(df$yearmon),max(df$yearmon))) +
#         ylim(min(df$count), max(df$count)) +
#         geom_line(data = df2, aes(x=df2$yearmon, y=df2$count), colour="orange")
#       
#       ggplotly(g)
#     }
#     
#   })
#   
# #   output$plot8 <- renderPlot({
# #     ### Global Travel Warnings
# #     df8<-data[data$feed=="Global Travel Warnings", j=list(count = .N), by = Date]
# #     
# #     ggplot(df8[Date<=input$slider1,], aes(Date, count)) +
# #       geom_line(colour="green") + 
# #       xlab("") + ylab("Daily Global Travel Warnings")
# #     
# #   })
#   
#   output$plot8 <- renderPlotly({
#     ### Global Travel Warnings
#     ## STATIC PLOT - DO NOT ERASE - USE FOR REF
#     # ggplot(data[data$feed=="Global Travel Warnings", j=list(count = .N), by = Date], aes(Date, count)) +
#     #  geom_line(colour="green") + 
#     #  xlab("") + ylab("Daily Global Travel Warnings")
# 
#     #######################################
#     
#     # Set up the temp datasets...
#     df<-data[data$feed=="Global Travel Warnings", j=list(count = .N), by = yearmon]
#     df2<-df[df$yearmon<=input$slider1,]
#     
#     # Use conditional logic for when there is no data
#     if (input$slider1 < min(df$yearmon)) {
#       g<-ggplot(df, aes(x=yearmon, y=count)) + 
#         geom_line(data = df, aes(x=df$yearmon, y=df$count*0), colour="green") + 
#         xlab("") + ylab("Daily Global Travel Warnings") +
#         #scale_x_yearmon(limits = c(min(df$yearmon),max(df$yearmon))) +
#         ylim(min(df$count), max(df$count))
#       
#       ggplotly(g)
#     } else {
#       g<-ggplot(df, aes(x=yearmon, y=count)) + 
#         xlab("") + ylab("Daily Global Travel Warnings") +
#         #scale_x_yearmon(limits = c(min(df$yearmon),max(df$yearmon))) +
#         ylim(min(df$count), max(df$count)) +
#         geom_line(data = df2, aes(x=df2$yearmon, y=df2$count), colour="green")
#       
#       ggplotly(g)
#     }
#     
#   })
#   
# #   output$plot9 <- renderPlot({
# #     ### US State Dept Alerts & Warnings
# #     df9<-data[feed=="US State Dept Travel Alerts" | feed=="US State Dept Travel Warnings", j=list(count = .N), by = Date]
# #     
# #     ggplot(df9[Date<=input$slider1,], aes(Date, count)) +
# #       geom_line(colour="green") + 
# #       xlab("") + ylab("Daily US State Dept Alerts & Warnings")
# #     
# #   })
#   
#   output$plot9 <- renderPlotly({
#     ### US State Dept Alerts & Warnings
#     ## STATIC PLOT - DO NOT ERASE - USE FOR REF
#     # ggplot(data[feed=="US State Dept Travel Alerts" | feed=="US State Dept Travel Warnings", j=list(count = .N), by = Date], aes(Date, count)) +
#     #  geom_line(colour="green") + 
#     #  xlab("") + ylab("Daily US State Dept Alerts & Warnings")
# 
#     #######################################
#     
#     # Set up the temp datasets...
#     df<-data[data$feed=="US State Dept Travel Alerts" | data$feed=="US State Dept Travel Warnings", j=list(count = .N), by = yearmon]
#     df2<-df[df$yearmon<=input$slider1,]
#     
#     # Use conditional logic for when there is no data
#     if (input$slider1 < min(df$yearmon)) {
#       g<-ggplot(df, aes(x=yearmon, y=count)) + 
#         geom_line(data = df, aes(x=df$yearmon, y=df$count*0), colour="green") +
#         xlab("") + ylab("Daily US State Dept Alerts & Warnings") +
#         #scale_x_yearmon(limits = c(min(df$yearmon),max(df$yearmon))) +
#         ylim(min(df$count), max(df$count))
#       
#       ggplotly(g)
#     } else {
#       g<-ggplot(df, aes(x=yearmon, y=count)) + 
#         xlab("") + ylab("Daily Global Travel Warnings") +
#         #scale_x_yearmon(limits = c(min(df$yearmon),max(df$yearmon))) +
#         ylim(min(df$count), max(df$count)) +
#         geom_line(data = df2, aes(x=df2$yearmon, y=df2$count), colour="green")
#       
#       ggplotly(g)
#     }
#     
#   })
#   
#   output$plot12 <- renderPlotly({
#     ### Plot all Global Travel Warnings - static for now USING "map" package...but see after next two lines to do same in ggplot...
# #     map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
# #     pts<-points(data[Date<=input$slider2][[4]],data[Date<=input$slider2][[5]], col = "red", pch=8, cex=.25)
#     
#     ### OR ###
#     
#     #Using GGPLOT to get World Map
# #     mp <- NULL
# #     mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
# #     mp <- ggplot() + mapWorld
# #     #Now Layer the cities on top (x=long, y=lat)
# #     mp <- mp + 
# #     ### WITH SLIDER
# # #       geom_point(data = data[data$Date<=input$slider2,], 
# # #                  aes(x=data[data$Date<=input$slider2,]$lon, 
# # #                      y=data[data$Date<=input$slider2,]$lat), 
# # #       colour = "red") #+ scale_size(range = c(2, 7))
# #     ### WITHOUT SLIDER
# #       geom_point(data = data, aes(x=lon, y=lat), colour = "red")
# #     #print(mp,newpage=F)
# #     ggplotly(mp)
#     
#     ### OR with proportional bubbles
#     #Using GGPLOT to get World Map
#     mp <- NULL
#     mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
#     mp <- ggplot() + mapWorld
#     #Now Layer the cities on top (x=long, y=lat)
#     mp <- mp + 
#       geom_point(data = nearbyLatLong, aes(x=lon, y=lat, size=pop), colour = "red")
#     ggplotly(mp)
#   })

  
#   ### Map with interactions...
#   output$plot11 <- renderPlot({
#     #####################################################################################
#     ### MAP WITH INTERACTIONS...
#     #####################################################################################
#     
#     #to avoid conflicts between plyr and Hmisc. If anyone knows a better way of doing this, please let me know!
#     #tryCatch(detach("package:Hmisc"),error=function(e) NULL)
#     
#     df2 <- ddply(gdelt,.(Actor1GeoLat,Actor1GeoLong,Actor2GeoLat,Actor2GeoLong,ActionGeoLat,ActionGeoLong),summarize, count=sum(count))
#     df2 <- df2[complete.cases(df2),]
#     
#     ### place points and edges in separate data frames
#     pointsDf <- df2[,5:7]
#     colnames(pointsDf)[3] <- "count2"
#     edgesDf <- df2[,c(1:4,7)]
#     
#     a <- paste0(edgesDf[,1],edgesDf[,2])#remove points were start and end are the same
#     b <- paste0(edgesDf[,3],edgesDf[,4])
#     edgesDf <- edgesDf[!a==b,]
# 
#     edgeMaker <- function(whichRow, len = 1, curved = TRUE){
#       fromC <- c(edgesDf[whichRow,2],edgesDf[whichRow,1])  # Origin
#       toC <- c(edgesDf[whichRow,4],edgesDf[whichRow,3]) # Terminus
#       weight <- edgesDf[whichRow, 5]  # Terminus
#       
#       ### Add curve:
#       graphCenter <- c(0,0)#colMeans(edgesDf[,1:2])  # Center of the overall graph
#       bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
#       distance1 <- sum((graphCenter - bezierMid)^2)
#       if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
#         bezierMid <- c(toC[1], fromC[2])
#       }  # To select the best Bezier midpoint
#       bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
#       if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
#       
#       edge <- data.frame(Hmisc::bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
#                                        c(fromC[2], bezierMid[2], toC[2]),  # X & y
#                                        evaluation = len))  # Bezier path coordinates
#       edge$Sequence <- 1:len  # For size and colour weighting in plot
#       edge$weight <- weight
#       edge$Group <- whichRow
#       return(edge)
#     }
#     allEdges <- lapply(1:nrow(edgesDf), edgeMaker, len = 100, curved = TRUE)
#     allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^
#     
#     #     col <- rep(gray(8/10),length(labels))
#     #     col2 <- rep(gray(1/10),length(labels))
#     #     col[labels==term] <- "red3"
#     #     col2[labels==term] <- "red4"
#     
#     new_theme_empty <- theme_bw()
#     new_theme_empty$line <- element_blank()
#     new_theme_empty$rect <- element_blank()
#     new_theme_empty$strip.text <- element_blank()
#     new_theme_empty$axis.text <- element_blank()
#     new_theme_empty$axis.title <- element_blank()
#     new_theme_empty$legend <- element_blank()
#     new_theme_empty$plot.margin <- structure(c(0, 0, 0, -1), unit = "lines",
#                                              valid.unit = 3L, class = "unit")
#     ### Plot world map with ggplot...
#     mp <- NULL
#     mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
#     mp <- ggplot() +   mapWorld
#     
#     mp <- mp + geom_path(data=allEdges, aes(x = x, y = y,group = Group,  # Edges with gradient
#                                             size=(weight-1),colour=Sequence),alpha=.6)  # and taper
#     mp <- mp + geom_point(data = data.frame(pointsDf),  # Add points
#                           aes(x = ActionGeoLong, y = ActionGeoLat,size=log(count2)), pch = 21,
#                           colour = "black", fill = "red2")  # Customize gradient 
#     mp <- mp + scale_colour_gradient(low = "red3", high = "white", guide = "none")
#     mp <- mp + scale_size(range = c(.6, 5), guide = "none")  # Customize taper
#     mp <- mp + new_theme_empty  # Clean up plot
#     #mp <- mp + xlim(min(allEdges$x-1),max(allEdges$x+1))
#     #mp <- mp + ylim(20,65)
#     
#     print(mp)
#     #ggplotly(mp)
#   })
  
########################################################################################################################################
### Tables
########################################################################################################################################  
  
  # a large table, reactive to input$show_vars & sorted columns are colored now because CSS are attached to them
  output$mytable1 = renderDataTable({
    #as.data.frame(data[,c(12,3,6,7,4,5), with=FALSE])[, input$show_vars, drop = FALSE]
    #as.data.frame(data[,c(12,3,6,7,4,5), with=FALSE])
    data[,.(feed,Datetime,categories,lon,lat)]
  },  options = list(orderClasses = TRUE)
  )
  # sorted columns are colored now because CSS are attached to them
#   output$mytable2 = renderDataTable({
#     mtcars
#   }, options = list(orderClasses = TRUE)
#   )
  
  # customize the length drop-down menu; display 5 rows per page by default
#   output$mytable3 = renderDataTable({
#     iris
#   }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
#   )
})

