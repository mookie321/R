#install.packages("data.table")
#install.packages("Cairo")
#install.packages("plotly")
#install.packages("htmlwidgets"))
library(shiny)
library(shinydashboard)
library(ggplot2)
library(maps)
library(mapproj)
library(data.table)
library(htmlwidgets)
library(plotly)
library(sna)
library(Hmisc)
library(lubridate)
library(plyr)
library("threejs")
#library(RgoogleMaps)
#library(ggmap)
#library(Cairo)
#options(shiny.usecairo=T)

### Read in Rds data
data <- readRDS("Dedup2015unionedData.rds")
gdelt <- readRDS("gdeltRED_USA_2011Q1.Rds")
nearbyLatLong<-readRDS("nearbyLatLong.rds")
mergeAll<-readRDS("mergeAll.rds")
FoodWaterAndHuman<-readRDS("FoodWaterHumTrafficData.rds")

### Global Vars
earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"
colores<-rep("black",length(nearbyLatLong$lat))
colores[which((nearbyLatLong$pop-1)*(49/291) + 1 > 5)]<-"red"
jpeg(earth,width=2048,height=1024,quality=100,bg="white",antialias="default")

### Global adhoc datasets
ANroundLatLong<-data[data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) | 
                       data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),]
ANnearbyLatLong<-ANroundLatLong[,j=list(pop=.N), by = list(lon,lat)]
ANroundLatLong.1<-FoodWaterAndHuman[FoodWaterAndHuman$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) | 
                       FoodWaterAndHuman$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),]
ANnearbyLatLong.1<-ANroundLatLong.1[,j=list(pop=.N), by = list(lon,lat)]
food2<-FoodWaterAndHuman[,.(link,eventSourceSid,Datetime,lon,lat,categories,title,timestamp,month,timestamp_s,Date,feed)]
data2 <- base::rbind(data,food2)
df1<-data2[order(month), j=list(count = .N), by = list(month, feed)]
df1 <- df1[complete.cases(df1),]
# df1<-data[order(month), j=list(count = .N), by = list(month, feed)]
# df1 <- df1[complete.cases(df1),]
df2<-data[data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),]
df3<-data[data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1),]
df5<-data[data$feed=="Terrorism" & (data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) | 
                                      data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)),
          j=list(count = .N), by = Date]
df5.1<-data[!data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) |
              !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
            j=list(count = .N), by = Date]
df5.2<-data[data$feed=="Terrorism" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),]
df5.3<-data[data$feed=="Terrorism" & data$Date==as.Date("2015-02-02"),]
df6<-data[data$feed=="Gangs" & (data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) | 
                                  data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)),
          j=list(count = .N), by = Date]
df6.1<-data[!data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) |
              !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
            j=list(count = .N), by = Date]
df6.2<-data[data$feed=="Gangs" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)]
df6.3<-data[data$feed=="Gangs" & (data$Date==as.Date("2015-02-02") | data$Date==as.Date("2015-02-23"))]
df7<-data[data$feed=="Global Health Hazards" & (data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) | 
                                                  data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)),
          j=list(count = .N), by = Date]
df7.1<-data[!data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) |
              !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
            j=list(count = .N), by = Date]
df7.2<-data[data$feed=="Global Health Hazards" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)]
df7.3<-data[data$feed=="Global Health Hazards" & data$Date==as.Date("2015-02-02")]
df8<-data[data$feed=="US Hazmat Incidents" & (data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) | 
                                                data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)),
          j=list(count = .N), by = Date]
df8.1<-data[!data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) |
              !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
            j=list(count = .N), by = Date]
df8.2<-data[data$feed=="US Hazmat Incidents" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)]
df8.3<-data[data$feed=="US Hazmat Incidents" & data$Date==as.Date("2015-02-02")]
df9<-data[data$feed=="Global Travel Warnings" & (data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) | 
                                                   data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)),
          j=list(count = .N), by = Date]
df9.1<-data[!data$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) |
              !data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
            j=list(count = .N), by = Date]
df9.2<-data[data$feed=="Global Travel Warnings" & data$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)]
df9.3<-data[data$feed=="Global Travel Warnings" & data$Date==as.Date("2015-02-02")]
df10<-FoodWaterAndHuman[FoodWaterAndHuman$feed=="Food/Water Shortages" & (FoodWaterAndHuman$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) | 
                                                                            FoodWaterAndHuman$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)),
                        j=list(count = .N), by = Date]
df10.1<-FoodWaterAndHuman[!FoodWaterAndHuman$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) |
                            !FoodWaterAndHuman$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
                          j=list(count = .N), by = Date]
df10.2<-FoodWaterAndHuman[FoodWaterAndHuman$feed=="Food/Water Shortages" & FoodWaterAndHuman$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),]
df10.3<-FoodWaterAndHuman[FoodWaterAndHuman$feed=="Food/Water Shortages" & FoodWaterAndHuman$Date==as.Date("2015-02-02"),]
df11<-FoodWaterAndHuman[FoodWaterAndHuman$feed=="Human Trafficking" & (FoodWaterAndHuman$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) | 
                                                                         FoodWaterAndHuman$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1)),
                        j=list(count = .N), by = Date]
df11.1<-FoodWaterAndHuman[!FoodWaterAndHuman$Date%in%seq.Date(as.Date("2015-02-23")-5,as.Date("2015-02-23")+5,by=1) |
                            !FoodWaterAndHuman$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),
                          j=list(count = .N), by = Date]
df11.2<-FoodWaterAndHuman[FoodWaterAndHuman$feed=="Human Trafficking" & FoodWaterAndHuman$Date%in%seq.Date(as.Date("2015-02-02")-5,as.Date("2015-02-02")+5,by=1),]
df11.3<-FoodWaterAndHuman[FoodWaterAndHuman$feed=="Human Trafficking" & FoodWaterAndHuman$Date==as.Date("2015-02-02"),]
dfVar1<-mergeAll[,.(Date,meanCnt)]
dfVar2<-mergeAll[meanCnt>=mean(mergeAll$meanCnt)+3*sd(mergeAll$meanCnt), .(Date, meanCnt)]



