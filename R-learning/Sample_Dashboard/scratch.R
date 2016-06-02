### Quick scratch script to aggregate data for individual charts by month and use "yyyy-mm" format for dates

# quick check to see if I can get "yyyy-mm" date variable from data$Date with built-in function
# I am going to try a hack since as.Date will insert day 1 when only given month and year
library(zoo)
as.character(lubridate::month(head(data$Date)))
as.character(lubridate::year(head(data$Date)))
# the function below will map any date to the first of the month (might be best to use for aggregating by "month")
as.Date(paste(as.character(lubridate::year(head(data$Date))), as.character(lubridate::month(head(data$Date))), "01", sep = "-"))

# let me add a field to data that is each data$Date mapped to the first of the month...
data$yearmon<-as.Date(paste(as.character(lubridate::year(data$Date)), as.character(lubridate::month(data$Date)), "01", sep = "-"))

# now let me try and aggregate based on this field (yearmon) instead of Date...
test<-data[data$feed=="Terrorism", j=list(count = .N), by = Date]
test1<-data[data$feed=="Terrorism", j=list(count = .N), by = yearmon]

# now to check how plot would look like for monthly aggregated data
ggplot(test1, aes(yearmon, count)) +
  geom_line(colour="blue") + 
  xlab("") + ylab("Terrorism Events") 



############################################################################################################
### 2016-01-28
### Code for whatever I need for writing up the Cust 1 report
############################################################################################################

# Code to reproduce and polish static plots for each data source...

  ### Terrorism Feed Only
  ## STATIC PLOT - DO NOT ERASE - USE FOR REF
  ggplot(data[data$feed=="Terrorism", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="cyan4") + 
    geom_line(y=filter(data[data$feed=="Terrorism", j=list(count = .N), by = Date]$count, rep(1/7,7), sides=2), colour="black") + 
    geom_line(y=28, colour="blue") + 
    geom_line(y=47, colour="red") + 
    xlab("") + ylab("Terrorism Events") + ggtitle("Trend of Terrorism Events for 2015 Q4")


  ### Gang Feed Only
  ## STATIC PLOT - DO NOT ERASE - USE FOR REF
  ggplot(data[data$feed=="Gangs" & data$Date>=as.Date("2015-10-01"), j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="red") + 
    xlab("") + ylab("Gang Events") + ggtitle("Trend of Gang Events for 2015 Q4")


  ### Health Feed Only
  ## STATIC PLOT - DO NOT ERASE - USE FOR REF
  ggplot(data[data$feed=="Global Health Hazards" & data$Date>=as.Date("2015-10-01"), j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="pink") + 
    xlab("") + ylab("Health Hazards") + ggtitle("Trend of Health Hazards for 2015 Q4")


  ### Hazmat
  ## STATIC PLOT - DO NOT ERASE - USE FOR REF
  ggplot(data[data$feed=="US Hazmat Incidents", j=list(count = .N), by = Date], aes(Date, count)) +
    geom_line(colour="orange") + 
    xlab("") + ylab("Hazmat Incidents") + ggtitle("Trend of Hazmat Incidents for 2015 Q4")

#   ### Global Travel Warnings
#   ## STATIC PLOT - DO NOT ERASE - USE FOR REF
#   ggplot(data[data$feed=="Global Travel Warnings", j=list(count = .N), by = Date], aes(Date, count)) +
#     geom_line(colour="green") + 
#     xlab("") + ylab("Global Travel Warnings") + ggtitle("Trend of Global Travel Warnings for 2015 Q4")
# 
#   ### US State Dept Alerts & Warnings
#   ## STATIC PLOT - DO NOT ERASE - USE FOR REF
#   ggplot(data[feed=="US State Dept Travel Alerts" | feed=="US State Dept Travel Warnings", j=list(count = .N), by = Date], aes(Date, count)) +
#     geom_line(colour="green") + 
#     xlab("") + ylab("US State Dept Alerts & Warnings") + ggtitle("Trend of US State for 2015 Q4")
  
  ### Quick look to see how Global Travel Warnings and US State Dept Alerts & Warnings turns out
  ggplot(data[data$feed %in% c("Global Travel Warnings", "US State Dept Travel Alerts") & data$Date>=as.Date("2015-10-01"), j=list(count = .N), by = Date], aes(Date, count)) +
    geom_line(colour="green") + 
    xlab("") + ylab("Travel Warnings/Alerts") + ggtitle("Trend of Travel Warnings (3 feeds) for 2015 Q4")

# Quick look into the geospatial location of where the events took place for the spike in Terrorist events near Oct 7th spike
  
  # quick aggregation of events by day so that I can see on what day exactly the hightest count is associated with...
  data[data$feed=="Terrorism", j=list(count = .N), by = Date][order(Date),]
  
  # temp dataset only looking at all data with the most events (2015-10-06 & 07), least events (2015-10-24, 11-07, 12-05, 12-15), and San Bernardino (2015-12-02)
  temp<-data[data$Date%in%c(as.Date("2015-10-06"),as.Date("2015-10-07"),as.Date("2015-10-24"),as.Date("2015-11-07"),as.Date("2015-12-02"),as.Date("2015-12-05"),as.Date("2015-12-15")),]
  
  # geo on world map
  map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
  pts<-points(temp$lon,temp$lat, col = "red", pch=8, cex=.25)
  
  # geo on usa map
  map("usa", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
  pts<-points(temp$lon,temp$lat, col = temp$feed, pch=8, cex=.25)
  
  # focus on specific date
  map("usa", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
  pts<-points(temp[temp$Date==as.Date("2015-10-06"),]$lon,temp[temp$Date==as.Date("2015-10-06"),]$lat, col = temp$feed, pch=8, cex=.25)
  
  map("usa", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
  pts<-points(temp[temp$Date==as.Date("2015-10-07"),]$lon,temp[temp$Date==as.Date("2015-10-07"),]$lat, col = temp$feed, pch=8, cex=.25)
  
  map("usa", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
  pts<-points(temp[temp$Date==as.Date("2015-12-02"),]$lon,temp[temp$Date==as.Date("2015-12-02"),]$lat, col = temp$feed, pch=8, cex=.25)
  
  # for both 10-6 and 10-7
  map("usa", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
  pts<-points(temp[temp$Date%in%c(as.Date("2015-10-06"),as.Date("2015-10-06")),]$lon,temp[temp$Date%in%c(as.Date("2015-10-06"),as.Date("2015-10-06")),]$lat,
              col = "red", pch=8, cex=.75)
  #same as above but focused only on East coast cluster
  map("state", regions = c("delaware","new jersey","new york","connecticut",
                           "rhode island", "massachusetts","new hampshire","vermont","maine"), 
      boundary=TRUE, col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
  pts<-points(temp[temp$Date%in%c(as.Date("2015-10-06"),as.Date("2015-10-06")),]$lon,temp[temp$Date%in%c(as.Date("2015-10-06"),as.Date("2015-10-06")),]$lat,
              col = "red", pch=8, cex=.75)
  
  # to get addresses for geo codes in 2015-10-06 to see what locations they actually are...
  library(ggmap)
  revgeocode(c(temp[temp$Date%in%c(as.Date("2015-10-06"),as.Date("2015-10-06")),]$lon[34], temp[temp$Date%in%c(as.Date("2015-10-06"),as.Date("2015-10-06")),]$lat[34]))
  for(i in 1:97)
  {
    addr<-revgeocode(c(temp[temp$Date%in%c(as.Date("2015-10-06"),as.Date("2015-10-06")),]$lon[i], temp[temp$Date%in%c(as.Date("2015-10-06"),as.Date("2015-10-06")),]$lat[i]))
    print(addr)
  }
  #print(i)

  # to see how many unique geo coordinates we have for 2015-10-06
  dim(temp[temp$Date==as.Date("2015-10-06"),])
  # sanity check for lon and lat
  length(temp[temp$Date==as.Date("2015-10-06"),]$lon)
  length(temp[temp$Date==as.Date("2015-10-06"),]$lat)
  # unique count check for each individual coordinate
  length(unique(temp[temp$Date==as.Date("2015-10-06"),]$lon))
  length(unique(temp[temp$Date==as.Date("2015-10-06"),]$lat))
  # total unique count for both coordinates combined
  dim(unique(temp[temp$Date==as.Date("2015-10-06"),c(4,5), with=F]))
  dim(unique(temp[temp$Date==as.Date("2015-10-07"),c(4,5), with=F]))
  dim(unique(temp[temp$Date==as.Date("2015-12-02"),c(4,5), with=F]))
  
  #tabulate counts for 2015-10-06 by feed (data source)
  table(temp[temp$Date==as.Date("2015-10-06"),]$feed)
  table(temp[temp$Date==as.Date("2015-10-07"),]$feed)
  table(temp[temp$Date==as.Date("2015-12-02"),]$feed)
  
  ### Function to get the region or states for the US...
  library(sp)
  library(maps)
  library(maptools)
  
  # The single argument to this function, pointsDF, is a data.frame in which:
  #   - column 1 contains the longitude in degrees (negative in the US)
  #   - column 2 contains the latitude in degrees
  
  latlong2state <- function(pointsDF) {
    # Prepare SpatialPolygons object with one SpatialPolygon
    # per state (plus DC, minus HI & AK)
    states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
    IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
    states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=wgs84"))
    
    # Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                              proj4string=CRS("+proj=longlat +datum=wgs84"))
    
    # Use 'over' to get _indices_ of the Polygons object containing each point 
    indices <- over(pointsSP, states_sp)
    
    # Return the state names of the Polygons object containing each point
    stateNames <- sapply(states_sp@polygons, function(x) x@ID)
    stateNames[indices]
  }
  
  # Test the function using points some points
  testPoints <- data.frame(x = temp[temp$Date%in%c(as.Date("2015-10-06"),as.Date("2015-10-06")),]$lon[c(6,11,28,34)],
                           y = temp[temp$Date%in%c(as.Date("2015-10-06"),as.Date("2015-10-06")),]$lat[c(6,11,28,34)])
  latlong2state(testPoints)
  #tabulate the results from above...
  table(latlong2state(testPoints))
  
  # Test the function with all the points from these two dates...note...shouldn't work with out of US
  testPoints <- data.frame(x = temp[temp$Date%in%c(as.Date("2015-10-06"),as.Date("2015-10-06")),]$lon,
                           y = temp[temp$Date%in%c(as.Date("2015-10-06"),as.Date("2015-10-06")),]$lat)
  latlong2state(testPoints)
  #tabulate the results from above...
  table(latlong2state(testPoints))
  
  # On all the longs and lats for the temp data which still only contains those dates of interest thus far...
  testPoints <- data.frame(x = temp$lon, y = temp$lat)
  #tabulate the results from above...
  table(latlong2state(testPoints))
  
  # On all the longs and lats for the the entire data
  testPoints <- data.frame(x = data$lon, y = data$lat)
  #tabulate the results from above...
  table(latlong2state(testPoints))
  
# NOTE: AT THIS POINT FORWARD, I MIGHT TO USE THIS DATASET TO ADD ADDITIONAL FIELDS, PERHAPS FROM GDELT, ETC.
# TO FURTHER ADD DETAILS TO THE WRITE-UP. 
  ### Create subset of data dataset that contains only relevant fields and 2015Q4 in order to apply latlong2state fcn
  qfour<-data[data$Date>=as.Date("2015-10-01"),c("lon","lat","categories","title","month","Date","feed"),with=F]
  # some sanity checks
  min(qfour$Date)
  max(qfour$Date)
  
  #need to add states to data in qfour...
  qfour$state<-latlong2state(data.frame(x=qfour$lon,y=qfour$lat))
  
  #tabulate by state each feed
  table(qfour$state,qfour$feed)
  
  #write out to make into nicely formatted table in Excel
  write.csv(table(qfour$state,qfour$feed), "/Users/tomas/Documents/IQT/state_feeds_2015Q4.csv")
  
  #to get tabulations for BOS area only
  dim(qfour[qfour$lat>=41.863056 & qfour$lat<=42.863056 & qfour$lon>=-71.506389 & qfour$lon<=-70.506389,])
  bos<-qfour[qfour$lat>=41.863056 & qfour$lat<=42.863056 & qfour$lon>=-71.506389 & qfour$lon<=-70.506389,]
  #tabulate as above but for BOS only and by month
  table(bos$month,bos$feed)
  
  
  ############################################################################################################
  ### 2016-02-17
  ### Code for getting ribbon plots done correctly on the various feed plots
  ############################################################################################################
  library(ggplot2)
  q <- data.frame(
    x   = seq(1, 100, 1),
    ts1 = sample(1:100),
    ts2 = sample(1:100))
  
  q$mean <- apply(q, 1, function(row) mean(row[-1]))
  q$sd   <- apply(q, 1, function(row) sd(row[-1]))
  
  eb <- aes(ymax = mean + sd, ymin = mean - sd)
  ggplot(data = q, aes(x = x, y = mean)) + 
    geom_line(size = 2) + 
    geom_ribbon(eb, alpha = 0.5)
  
  ### NOTE: the code above makes sense for plotting the mean of several fields across the a single time variable, and then SD as ribbon
  
  # Now, let me start looking at the code for the various plots and see how to modify for ribbon/anomaly detection
  ggplot(data[data$feed=="Terrorism", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily Terrorism Events")
  
  # same code as above but want to see plot if we do NOT filter for feed 
  ggplot(data[, j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily Total Events: Terrorism, Gangs, Health Hazards, Hazmat, Global Travel Warnings, US State Dept Alerts & Warnings")
  
## RESULT: there is a huge spike in activity in early Q1, then a drop to "normal" and then another huge spike in late Q1...need to dig into each feed
# Terrorism:
  ggplot(data[data$feed=="Terrorism", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily Terrorism Events")

# Gangs:
  ggplot(data[data$feed=="Gangs", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily Gang Events")
  
# Health Hazards
  ggplot(data[data$feed=="Global Health Hazards", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily Global Health Hazards")
  
# US Hazmat
  ggplot(data[data$feed=="US Hazmat Incidents", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily US Hazmat Incidents")
  
# Global Travel Warnings
  ggplot(data[data$feed=="Global Travel Warnings", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily Global Travel Warnings")
  
# US State Dept Travel Alerts
  ggplot(data[data$feed=="US State Dept Travel Alerts", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily US State Dept Travel Alerts")
  
# US State Dept Travel Warnings
  ggplot(data[data$feed=="US State Dept Travel Warnings", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily US State Dept Travel Warnings")
  
### RESULT: The early Q1 spike is due to Global Travel Warnings feed. The latter Q1 spike is due to Gang & US Hazmat feeds...
# going to create temp datasets of each so that I can dig into each
  # Starting with Global Travel Warnings since seems to be most egregious offender
### global travel warnings data only
  gtwarns<-data[data$feed=="Global Travel Warnings",]
  # let check unique links, timestamps, etc.
  length(unique(gtwanrs$link))
  #83k rows should not be too much of a problem for excel...so writing out to csv and quick review in Excel
  write.csv(gtwarns,"/Users/tomas/Desktop/gtwarns2015.csv")
  ### RESULT: by looking at the count of unique links and quick glance on Excel, we can see that the dedups are present...
  ### need to simply sort by timestamp and choose the most recent timestamp by link
  
### gang data only
  gangs<-data[data$feed=="Gangs",]
  #unique count of links seems to match up...so this might just turn out to be a true signal
  length(unique(gangs$link))
  table(gangs$month)
  # writing to csv to quick analyze in Excel for record linkage
  write.csv(gangs,"/Users/tomas/Desktop/gangs2015.csv")
  # quick tabulation of categories for month=2 in gangs
  table(gangs)
  
### US hazmat data only
  hazmat<-data[data$feed=="US Hazmat Incidents",]
  #unique count of links seems to match up...so this might just turn out to be a true signal
  dim(hazmat)
  length(unique(hazmat$link))
  table(hazmat$month)
  # writing to csv to quick analyze in Excel for record linkage
  write.csv(hazmat,"/Users/tomas/Desktop/hazmat2015.csv")
  ### RESULT: dups also exits...reason likely that I used datetime and it is unique based on datetime
  
  #quick test to see if my R code will select the most recent date
  # I know that in Hazmat the following link has 9 dups because the timestamp changes every hour
  testLink<-hazmat[hazmat$link %in% c("http://hazmat.globalincidentmap.com/eventdetail.php?ID=13284",
                                      "http://hazmat.globalincidentmap.com/eventdetail.php?ID=13285",
                                      "http://hazmat.globalincidentmap.com/eventdetail.php?ID=13289"),]
  ### SUCCESS...THE CODE BELOW DOES THE DEDUP IN R CORRECTLY
  testLink[order(timestamp), .SD[.N], by="link"]
  
### Let me start again as above (starting on new section for today 2016.02.17) but with de-dupped data...
  #de-dup the 'data' dataframe
  length(unique(data$link))
  data<-data[order(timestamp), .SD[.N], by="link"]
  
# Re-plotting again by total and feeds to see if there is still an artifact spike
  
  # All feeds
  ggplot(data[, j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    #geom_line(colour="blue") + #TO GET A LINE/TREND GRAPH
    geom_point(colour="blue") + #TO GET A SCATTER PLOT OF THE POINTS
    xlab("") + ylab("Daily Total Events: Terrorism, Gangs, Health Hazards, Hazmat, Global Travel Warnings, US State Dept Alerts & Warnings")
  
  ## RESULT: there is a huge spike in activity in early Q1, then a drop to "normal" and then another huge spike in late Q1...need to dig into each feed
  
  # Terrorism:
  ggplot(data[data$feed=="Terrorism", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    #geom_line(colour="blue") + 
    geom_point(colour="blue") + #TO GET A SCATTER PLOT OF THE POINTS
    xlab("") + ylab("Daily Terrorism Events")
  
  # Gangs:
  ggplot(data[data$feed=="Gangs", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily Gang Events")
  
  # Health Hazards
  ggplot(data[data$feed=="Global Health Hazards", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily Global Health Hazards")
  
  # US Hazmat
  ggplot(data[data$feed=="US Hazmat Incidents", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily US Hazmat Incidents")
  
  # Global Travel Warnings
  ggplot(data[data$feed=="Global Travel Warnings", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily Global Travel Warnings")
  
  # US State Dept Travel Alerts
  ggplot(data[data$feed=="US State Dept Travel Alerts", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily US State Dept Travel Alerts")
  
  # US State Dept Travel Warnings
  ggplot(data[data$feed=="US State Dept Travel Warnings", j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_line(colour="blue") + 
    xlab("") + ylab("Daily US State Dept Travel Warnings")
  
### RESULT: Just noticed that the spikes pertain to all the timestamps being identical on 2.23.15

### 2016.02.18 09:31...currently moving forward with data the way it is and checking back later to resolve issues
### moving forward with ribbon plots.
  
  # code to get dataset ideal for calculating the mean and sd across feeds for each day
  length(unique(data$Date))
  min(data$Date)
  max(data$Date)
  #first try to get the counts for each date for Terrorism
  data[data$feed=="Terrorism", j=list(count = .N), by = Date]
  dim(data[data$feed=="Terrorism", j=list(count = .N), by = Date]) # 342 x 2
  dim(data[data$feed=="Gangs", j=list(count = .N), by = Date]) # 42 x 2
  dim(data[data$feed=="Global Health Hazards", j=list(count = .N), by = Date]) # 344 x 2
  dim(data[data$feed=="US Hazmat Incidents", j=list(count = .N), by = Date]) # 316 x 2
  dim(data[data$feed=="Global Travel Warnings", j=list(count = .N), by = Date]) # 344 x 2
  dim(data[data$feed=="US State Dept Travel Alerts", j=list(count = .N), by = Date]) # 19 x 2
  dim(data[data$feed=="US State Dept Travel Warnings", j=list(count = .N), by = Date]) # 47 x 2
  
  # Create temp datasets for each count done by Date (MIGHT ALSO WANT TO DO BY MONTH)
  a <- data[data$feed=="Terrorism", j=list(terrorCnt = .N), by = Date]
  b <- data[data$feed=="Gangs", j=list(gangCnt = .N), by = Date]
  c <- data[data$feed=="Global Health Hazards", j=list(healthCnt = .N), by = Date]
  d <- data[data$feed=="US Hazmat Incidents", j=list(hazCnt = .N), by = Date]
  e <- data[data$feed=="Global Travel Warnings", j=list(gtWarnsCnt = .N), by = Date]
  f <- data[data$feed=="US State Dept Travel Alerts", j=list(stateAlerts = .N), by = Date]
  g <- data[data$feed=="US State Dept Travel Warnings", j=list(stateWarns = .N), by = Date]
  
  # begin to merge two at a time (may need to do this better--probably possible to do SQL-like join with data.table to do all at once)
  merge1<-merge(a, b, by="Date", all.x=T, all.y=T)
  merge2<-merge(merge1, c, by="Date", all.x=T, all.y=T)
  merge3<-merge(merge2, d, by="Date", all.x=T, all.y=T)
  merge4<-merge(merge3, e, by="Date", all.x=T, all.y=T)
  merge5<-merge(merge4, f, by="Date", all.x=T, all.y=T)
  mergeAll<-merge(merge5, g, by="Date", all.x=T, all.y=T)
  
  # quick check to make sure that where missing the math will work
  # change all NAs to 0s in data.table
  for (j in seq_len(ncol(mergeAll))) set(mergeAll, which(is.na(mergeAll[[j]])), j, 0)
  # Calcuate the median across feeds by Date
  mergeAll$medianCnt <- apply(mergeAll[, 2:8, with=FALSE], 1, median)
  # Calcuate the mean across feeds by Date
  mergeAll$meanCnt <- apply(mergeAll[, 2:8, with=FALSE], 1, mean)
  # Calculate the sd across feeds by Date
  mergeAll$sdCnt <- apply(mergeAll[, 2:8, with=FALSE], 1, sd)
  
  # Plot mean and sd as ribbon 
  eb <- aes(ymax = meanCnt + sdCnt, ymin = meanCnt - sdCnt)
  ggplot(data = mergeAll, aes(x = Date, y = meanCnt)) + 
    geom_line(size = 2) + 
    geom_ribbon(eb, alpha = 0.5) +
    # code above works without this additional line below...trying to layer scatter plot of points for all data
    #ggplot(data[, j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_point(data=data[, j=list(meanCnt = .N), by = Date], colour="blue") + #TO GET A SCATTER PLOT OF ALL COUNTS FOR DAY
    geom_point(data=mergeAll[,.(Date,meanCnt)], colour="blue") + #TO GET A SCATTER PLOT OF ALL COUNTS FOR DAY
    geom_point(data=mergeAll[meanCnt>=mean(mergeAll$meanCnt)+3*sd(mergeAll$meanCnt), .(Date, meanCnt)], colour="red") + # TO GET SCATTER PLOTS OF ONLY THOSE POINTS OUTSIDE 3 SDs
    xlab("") + ylab("Daily Average Events")
  
  # Trying to figure out how to scatter plots only those points outside 3 sds 
  mergeAll[meanCnt>=mean(mergeAll$meanCnt)+3*sd(mergeAll$meanCnt), .(Date, meanCnt)]
  
  # So we can see that we want to dig in to the following time neighborhood, namely: 2015-02-02 & 2015-02-23
  
  ### Continue to plot for all data and each individual feed based on 14 day (+/-7) window
  # All data but with data causing anomalies identified:
  ggplot(data[data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) | 
              data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
              j=list(count = .N), by = Date], aes(Date, count)) +
    geom_point(colour="red", pch=17, size=2) + 
    geom_point(data = data[!data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) |
                           !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
                           j=list(count = .N), by = Date], colour="grey", pch=20) + 
    xlab("") + ylab("Daily Average Events")
  
  # Terrorism:
  ggplot(data[data$feed=="Terrorism" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
              j=list(count = .N), by = Date], aes(Date, count)) +
    geom_point(colour="red", pch=17, size=2) + 
    geom_point(data = data[data$feed=="Terrorism" & !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
                           j=list(count = .N), by = Date], colour="grey", pch=20) + 
    xlab("") + ylab("Daily Terrorism Events")
  
  # Gangs:
  ggplot(data[data$feed=="Gangs" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
              j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_point(colour="blue", pch=17, size=2) + 
    geom_point(data = data[data$feed=="Gangs" & !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
                           j=list(count = .N), by = Date], colour="grey", pch=20) + 
    xlab("") + ylab("Daily Gang Events")
  
  # Health Hazards
  ggplot(data[data$feed=="Global Health Hazards" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
              j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_point(colour="blue", pch=17, size=2) + 
    geom_point(data = data[data$feed=="Global Health Hazards" & !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
                           j=list(count = .N), by = Date], colour="grey", pch=20) + 
    xlab("") + ylab("Daily Global Health Hazards")
  
  # US Hazmat
  ggplot(data[data$feed=="US Hazmat Incidents" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
              j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_point(colour="blue", pch=17, size=2) + 
    geom_point(data = data[data$feed=="US Hazmat Incidents" & !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
                           j=list(count = .N), by = Date], colour="grey", pch=20) + 
    xlab("") + ylab("Daily US Hazmat Incidents")
  
  # Global Travel Warnings
  ggplot(data[data$feed=="Global Travel Warnings" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
              j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_point(colour="blue", pch=17, size=2) + 
    geom_point(data = data[data$feed=="Global Travel Warnings" & !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
                           j=list(count = .N), by = Date], colour="grey", pch=20) + 
    xlab("") + ylab("Daily Global Travel Warnings")
  
  # US State Dept Travel Alerts
  ggplot(data[data$feed=="US State Dept Travel Alerts" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
              j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_point(colour="blue", pch=17, size=2) + 
    geom_point(data = data[data$feed=="US State Dept Travel Alerts" & !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
                           j=list(count = .N), by = Date], colour="grey", pch=20) + 
    xlab("") + ylab("Daily US State Dept Travel Alerts")
  
  # US State Dept Travel Warnings
  ggplot(data[data$feed=="US State Dept Travel Warnings" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
              j=list(count = .N), by = Date], aes(Date, count)) +
    #geom_line(aes(group=1), colour="blue") + 
    geom_point(colour="blue", pch=17, size=2) + 
    geom_point(data = data[data$feed=="US State Dept Travel Warnings" & !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
                           j=list(count = .N), by = Date], colour="grey", pch=20) + 
    xlab("") + ylab("Daily US State Dept Travel Warnings")
  
### To take another look at the combined feeds (first plot) from current dashboard
  df<-data[order(month), j=list(count = .N), by = list(month, feed)]
  df <- df[complete.cases(df),]
  ### REMEMBER: THIS IS BY MONTH!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  p<-ggplot(df, aes(x=month,y=count, group=feed)) +
    xlab("") + ylab("Monthly Total Events") + 
    scale_x_discrete(breaks = seq(1,12), limits = c(min(df$month, na.rm = T), max(df$month, na.rm = T)), # use max/min(..., na.rm=T) if issues
                       labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
    ylim(min(df$count), max(df$count)) +
    geom_line(aes(color=feed), size=0.75)
  ggplotly(p)
  
  ### For above plots, need to decide towards end for what order to provide them in...
  ## for now moving on to using code for spatial plotting...
  
  # Plot all Global Travel Warnings - static for now USING "map" package...but see after next two lines to do same in ggplot...
       map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
       pts<-points(data[data$feed=="Terrorism" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)][[4]], 
                   data[data$feed=="Terrorism" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)][[5]], 
                   col = "red", pch=8, cex=.25)

  ### OR ###
  
  #Using GGPLOT to get World Map
  mp <- NULL
  mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
  mp <- ggplot() + mapWorld
  #Now Layer the cities on top (x=long, y=lat)
  mp <- mp + 
    geom_point(data = data[data$feed=="Terrorism" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)], 
               aes(x=lon, y=lat), colour = "red") +
    geom_point(data = data[data$feed=="Terrorism" & data$Date==as.Date("2015-02-02")], 
               aes(x=lon, y=lat), colour = "blue")
  ggplotly(mp)
  
### RESULT: THE CODE BELOW WORKS REALLY WELL FOR PLOTTING THE COORDINATES WITH SIZED DOTS
### AND IT GOOD CANDIDATE FOR SLIDER BASED ON "POP" AND INDEX INDICATOR
  
  ### SAME AS ABOVE, BUT USING ROUNDING OF LAT/LONG AND TABULATION TO PROPORTINALLY SIZE UP DOTS
  # need to create a dataset that has the lat and long rounded and then count for each
  roundLatLong<-data[,c("lon","lat"):=list(round(lon,1), round(lat,1)), with=F]
  # now to aggregate for a pop field
  nearbyLatLong<-roundLatLong[,j=list(pop=.N), by = list(lon,lat)]
  #Using GGPLOT to get World Map
  mp <- NULL
  mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
  mp <- ggplot() + mapWorld
  #Now Layer the cities on top (x=long, y=lat)
  mp <- mp + 
    geom_point(data = nearbyLatLong, aes(x=lon, y=lat, size=pop), colour = "red")
  ggplotly(mp)

  
  ### looking into plotting world globe
  library("threejs")
  earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"
  globejs(img=earth, bg="white", emissive="#aaaacc", 
          lat  = nearbyLatLong[-150<=lon & lon<=-50 & 20<=lat & lat<=60,.(lat)]$lat, 
          long = nearbyLatLong[-150<=lon & lon<=-50 & 20<=lat & lat<=60,.(lon)]$lon,
          color = "red",
          #value = nearbyLatLong[-150<=lon & lon<=-50 & 20<=lat & lat<=60,.(pop)]$pop)
          value = (nearbyLatLong[-150<=lon & lon<=-50 & 20<=lat & lat<=60,.(pop)]$pop-1)*(19/130) + 1)
          #value = 20)

# same as above but doing for all spatial points
  globejs(img=earth, bg="white", emissive="#aaaacc", 
          lat  = nearbyLatLong$lat, 
          long = nearbyLatLong$lon,
          color = colores,
          value = (nearbyLatLong$pop-1)*(49/291) + 1)
  
  # quick test to see how I can design a vector the length of lat to have blue when <=5, red > 5
  head((nearbyLatLong$pop-1)*(49/291) + 1 > 5)
  colores<-rep("black",length(nearbyLatLong$lat))
  colores[which((nearbyLatLong$pop-1)*(49/291) + 1 > 5)]<-"red"
  
### Note on 2016.02.19 01:39 - I am thinking of putting the plot below and a Map of only the 
### "anomalous" events for the "Static charts" tab instead of each of the individual feeds
  
### NOTE THAT FOR THE MAP BELOW...MIGHT ALSO WANT TO HAVE THE MAP THAT DOESN'T DO PROP DOTS
  ### BUT DOES DO SEPARATE COLORS AND DO SIDE BY SIDE
  
  # All data but with data causing anomalies identified:
  p<-ggplot(data[data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) | 
                data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
              j=list(count = .N), by = Date], aes(Date, count)) +
    geom_point(colour="red", pch=17, size=2) + 
    geom_point(data = data[!data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) |
                             !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
                           j=list(count = .N), by = Date], colour="grey", pch=20) + 
    xlab("") + ylab("Daily Average Events")
  plot(p, newpage = T)
  
  # world map with prop dots but only for those anomalous dates
  # need to create a dataset that has the lat and long rounded and then count for each
  
  ANroundLatLong<-data[data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) | 
                     data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),]
  # now to aggregate for a pop field
  ANnearbyLatLong<-ANroundLatLong[,j=list(pop=.N), by = list(lon,lat)]
  #Using GGPLOT to get World Map
  mp <- NULL
  mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
  mp <- ggplot() + mapWorld
  #Now Layer the cities on top (x=long, y=lat)
  mp <- mp + 
    geom_point(data = data[data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),], 
               aes(x=lon, y=lat), colour = "black") +
    geom_point(data = data[data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1),], 
               aes(x=lon, y=lat), colour = "white") +
    geom_point(data = ANnearbyLatLong, aes(x=lon, y=lat, size=pop), colour = "red")

  ggplotly(mp)
  ### NOTE LINES 639 TO 645  DO FOR ALL...WHILE THE LINES BELOW DO FOR TERRORISM ONLY FOR THE "ANOMALOUS" DATES
  mp <- mp + 
    geom_point(data = ANnearbyLatLong, aes(x=lon, y=lat, size=pop), colour = "red") +
    geom_point(data = data[data$feed=="Terrorism" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)], 
               aes(x=lon, y=lat), colour = "red") +
    geom_point(data = data[data$feed=="Terrorism" & data$Date==as.Date("2015-02-02")], 
               aes(x=lon, y=lat), colour = "blue")
  ggplotly(mp)
  
  ### LEFT OFF 2016.02.19 02:08 HERE...Need to make the top two charts as the main "Dashboard" panel, that is
  ### do not use the one with total count, histogram, and pie chart,...
  ### and possibly still do another slide below that where we plot each Feed individually...but that should be it
  
  ### To see if I can get Shelly's code to work better
  output$plot10 <- renderPlotly({
    
    # Parse the data with the date slider input
    #df <- data[data$feed=="Terrorism" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),]
    df <- data[data$Date<=as.Date("2015-06-01"),]
    
    # Hover data
    df$hover <- with(df, paste(Datetime, " | ", title, " |  Feed:", feed))
    
    # marker styling
    m <- list(
      size = 8, opacity = 0.6, symbol = "circle", color = "#b20000"
    )
    
    # geo styling
    g <- list(
      showland = TRUE,
      landcolor = "#d9d9d9",
      subunitcolor = "#919191",
      countrycolor = "#919191",
      countrywidth = 0.5,
      subunitwidth = 0.5,
      showcountries = TRUE
    )
    
    # plot it
    plot_ly(df, lat = lat, lon = lon, text = hover, 
            type = "scattergeo", mode = "markers",
            marker = m) %>%
      layout(title = "This is the Title", geo = g)
    
  })
  
### 2016-02-19 15:07 - MAKING SURE I GET THE CORRECT PLOTS NEEDED WITH FOOD/WATER AND HUMAN TRAFFICKING
### ONCE WORKING HERE NEED TO ADD TO GLOBAL, SERVER AND ADJUST UI
  
# RESULT...I DID PERFORM THE STEPS MENTION ABOVE BUT MIGRATED CODE TO APPROPRIATE LOCATIONS FOR DASHBOARD

  