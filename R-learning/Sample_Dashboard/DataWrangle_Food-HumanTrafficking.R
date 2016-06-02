#######################################################################################################################################
### 2016-02-16
### Code to get the rest of the 2015 data for those feeds missing it
### Namely, create a new unionedData.rds file that has all 2015 data for those feeds that initially I left out for size
#######################################################################################################################################

### LOOK AT unionedData.rds to get the first dates for feeds and find out which need more data:
# library(data.table)
# unionedData<-readRDS("unionedData.rds")
# # get the first date by feed
# unionedData[, min(unionedData$Date, na.rm=T), by="feed"]
# unionedData[order(feed,month), j=list(count = .N), by="feed,month"]

#######################################################################################################################################
### OVERALL: TO SEE COMBINE ALL DATA SOURCES INTO ONE FOR THE DASHBOARD SAMPLE REPORT - 2016.02.16 13:06
#######################################################################################################################################

#######################################################################################
#Set the system environment variables                                                 #
Sys.setenv(SPARK_HOME = "/Users/tomas/spark-1.5.2-bin-hadoop2.6")                     #
Sys.setenv("SPARKR_SUBMIT_ARGS" = "--conf spark.driver.memory=2g sparkr-shell")       #
#
#Set the library path                                                                 #
.libPaths((c(file.path(Sys.getenv("SPARK_HOME"),"R","lib"),.libPaths())))             #
#
#Load the SparkR library                                                              #
library(SparkR)                                                                       #
#library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))        #
### Note:                                                                             #
# objects masked from package:stats = filter, na.omit                                 #
# objects masked from package:base = intersect, rbind, sample, subset,                #      
# summary, table, transform                                                           #
#
#Create the spark context and SQL context                                             #
sc <- sparkR.init()                                                                   #
#sc <- sparkR.init(master = "local[*]", sparkEnvir = list(spark.driver.memory="2g"))  #     
sqlContext<-sparkRSQL.init(sc)                                                        #
#######################################################################################


# path to each sample data source to determine what we can do away with
pathToTerror<-"/Users/tomas/Documents/S3-Samples/1-TerrorismGeoRSS/year=2015"
pathToGang<-"/Users/tomas/Documents/S3-Samples/2-GangGeoRSS/year=2015_DEDUP"
pathToHealth<-"/Users/tomas/Documents/S3-Samples/3-GlobalHealthHazards"
pathToHazmat<-"/Users/tomas/Documents/S3-Samples/7-HazmatGeoRSS"
pathToGTWarns<-"/Users/tomas/Documents/S3-Samples/8-1-GlobalTravelWarns"
pathToUSSAlerts<-"/Users/tomas/Documents/S3-Samples/8-2-US_State_Alerts"
pathToUSSWarns<-"/Users/tomas/Documents/S3-Samples/8-3-US_State_Warns"

pathToFoodWater<-"/Users/tomas/Documents/S3-Samples/14-GlobalFoodShortages/year=2015"
pathToHumanTraffic<-"/Users/tomas/Documents/S3-Samples/13-HumanTrafficking/year=2015"

#Create DataFrame from gz files in json format
rawTerror<-jsonFile(sqlContext,pathToTerror)
rawGang<-jsonFile(sqlContext,pathToGang)
rawHealth<-jsonFile(sqlContext,pathToHealth)
rawHazmat<-jsonFile(sqlContext,pathToHazmat)
rawGTWarns<-jsonFile(sqlContext,pathToGTWarns)
rawUSSAlerts<-jsonFile(sqlContext,pathToUSSAlerts)
rawUSSWarns<-jsonFile(sqlContext,pathToUSSWarns)

rawFoodWater<-jsonFile(sqlContext,pathToFoodWater)
rawHumanTraffic<-jsonFile(sqlContext,pathToHumanTraffic)

registerTempTable(rawTerror,"tempTerror")
terrorDF<-sql(sqlContext,"SELECT eventSourceSid, geometry.coordinates[0] as lon, geometry.coordinates[1] as lat,
              properties.categories, properties.link, properties.title, timestamp, month FROM tempTerror")

registerTempTable(rawGang,"tempGang")
GangDF<-sql(sqlContext,"SELECT eventSourceSid, lon, lat, categories, link, title, timestamp, month FROM tempGang")

# Simple select and assignment to new DF to rearrange schema as other instead of alphabetical
#GangDF<-select(rawGang, "eventSourceSid", "lon", "lat", "categories", "link", "title", "timestamp", "timestamp_s", "month")

registerTempTable(rawHealth,"tempHealth")
HealthDF<-sql(sqlContext,"SELECT eventSourceSid, geometry.coordinates[0] as lon, geometry.coordinates[1] as lat,
              properties.categories, properties.link, properties.title, timestamp, month FROM tempHealth")

registerTempTable(rawHazmat,"tempHazmat")
HazmatDF<-sql(sqlContext,"SELECT eventSourceSid, geometry.coordinates[0] as lon, geometry.coordinates[1] as lat,
              properties.categories, properties.link, properties.title, timestamp, month FROM tempHazmat")

registerTempTable(rawGTWarns,"tempGTWarns")
GTWarnsDF<-sql(sqlContext,"SELECT eventSourceSid, geometry.coordinates[0] as lon, geometry.coordinates[1] as lat,
               properties.categories, properties.link, properties.title, timestamp, month FROM tempGTWarns")

registerTempTable(rawUSSAlerts,"tempUSSAlerts")
USSAlertsDF<-sql(sqlContext,"SELECT eventSourceSid, properties.categories, properties.link, properties.title, timestamp, month FROM tempUSSAlerts")

registerTempTable(rawUSSWarns,"tempUSSWarns")
USSWarnsDF<-sql(sqlContext,"SELECT eventSourceSid, properties.categories, properties.link, properties.title, timestamp, month FROM tempUSSWarns")

registerTempTable(rawFoodWater,"tempFoodWater")
FoodWaterDF<-sql(sqlContext,"SELECT eventSourceSid, geometry.coordinates[0] as lon, geometry.coordinates[1] as lat,
              properties.categories, properties.link, properties.title, timestamp, month FROM tempFoodWater")

registerTempTable(rawHumanTraffic,"tempHumanTraffic")
HumanTrafficDF<-sql(sqlContext,"SELECT eventSourceSid, geometry.coordinates[0] as lon, geometry.coordinates[1] as lat,
              properties.categories, properties.link, properties.title, timestamp, month FROM tempHumanTraffic")

printSchema(terrorDF)
printSchema(GangDF)
printSchema(HealthDF)
printSchema(HazmatDF)
printSchema(GTWarnsDF)
printSchema(USSAlertsDF)
printSchema(USSWarnsDF)

printSchema(FoodWaterDF)
printSchema(HumanTrafficDF)

# Need to add lon and lat columns to Alerts and Warns
USSAlertsDF<-withColumn(USSAlertsDF,"lon",USSAlertsDF$month*0.0)
USSAlertsDF<-withColumn(USSAlertsDF,"lat",USSAlertsDF$month*0.0)
USSWarnsDF<-withColumn(USSWarnsDF,"lon",USSWarnsDF$month*0.0)
USSWarnsDF<-withColumn(USSWarnsDF,"lat",USSWarnsDF$month*0.0)

# change the order of the columns to match others
USSAlertsDF<-select(USSAlertsDF,"eventSourceSid","lon","lat","categories","link","title","timestamp","month")
USSWarnsDF<-select(USSWarnsDF,"eventSourceSid","lon","lat","categories","link","title","timestamp","month")

# Union all two at a time (will need to do this different in Scala)
unioned0<-unionAll(terrorDF,GangDF)
unioned1<-unionAll(unioned0,HealthDF)
unioned2<-unionAll(unioned1,HazmatDF)
unioned3<-unionAll(unioned2,GTWarnsDF)
unioned4<-unionAll(unioned3,USSAlertsDF)
unioned<-unionAll(unioned4,USSWarnsDF)

### 2016-02-19...doing separate join bc at this moment I just want to create another separate DF for Food/Human Traffic
FoodWaterAndHuman<-unionAll(FoodWaterDF,HumanTrafficDF)

#sanity check
printSchema(unioned)

printSchema(FoodWaterAndHuman)

### Add String Timestamp for readability
unioned<-withColumn(unioned, "timestamp_s", from_unixtime(unioned$timestamp / 1000))

FoodWaterAndHuman<-withColumn(FoodWaterAndHuman, "timestamp_s", from_unixtime(FoodWaterAndHuman$timestamp / 1000))

#sanity check
printSchema(unioned)
sum(count(terrorDF),count(GangDF),count(HealthDF),count(HazmatDF),count(GTWarnsDF),count(USSAlertsDF),count(USSWarnsDF))
count(unioned)

### COLLECT TO LOCAL AND USE DATA.TABLE TO REMOVE DUPLICATES ###########################################################
# collect to local df
unionLocal<-collect(unioned, stringsAsFactors = FALSE)

unionLocal$Date<-as.Date(unionLocal$timestamp_s) #use as.POSIX to get datetime (if needing finer levels)
#unionLocal$Datetime<-as.POSIXct(unionLocal$timestamp_s) #use as.POSIX to get datetime (if needing finer levels)
#NOTE: trying below is line above for Datetime did not work for some reason
unionLocal$Datetime<-as.POSIXct(unionLocal$timestamp/1000, origin = "1970-01-01")

# Install Data.Table to better deal with manipulation
library(data.table)
# Using data.table to order by link and Datetime
unionLocal.dt <- as.data.table(unionLocal)           # convert to data.table
# [REMOVE DUPS] Let's keep the last for each group (link, Datetime), i.e., only unique records
dedupunion = unionLocal.dt[, .SD[.N], by="link,Datetime"]

# Let me map all arcane eventSourceSid to readeable version (that could be used as proxy categories) in the unioned data
#create data frame with mappings
essidMap<-data.frame(eventSourceSid=c("d159cc2b-c067-4cf8-95c1-e9047dd1b94b","dcce4b79-ce65-42bd-8ed2-f73c44f213fe","b1f27ef6-5e66-49d2-947a-29e28a38f16e",
                                      "8fbb9f18-7ed1-495e-ad2f-ea127a0c0b11", "eec735bf-4a08-4d5a-ba4d-e56243fc368b", "14e83863-73ab-4edb-847d-642216b0aa6e",
                                      "66b7a3f4-0b9a-4665-a69c-18b024b1b917"),
                     feed=c("Terrorism","Gangs","Global Health Hazards","US Hazmat Incidents", "Global Travel Warnings", "US State Dept Travel Alerts",
                            "US State Dept Travel Warnings"))

# USE merge for data frames to get the "feed" easy to read field from essidMap into new df
dedupunion1<-merge(dedupunion,essidMap,by="eventSourceSid")

# NOTE (MAY OR MAY NOT NEED): month var in this combined data is wrong for some unknown reason....going to replace here
#dedupunion1$month<-month(dedupunion1$Date)

# Quick save of dedupunion1 so that I can use it in Dashboard project to create all the plots...
# might also need to save individual aggregated levels of dedupunion1 (unless I just replace with one liner of aggregation)
saveRDS(dedupunion1, "All2015unionedData.rds")

####################################################################################################
### 2016-02-19...save as above starting at line 148, but for Food/Water and Human Trafficking only
####################################################################################################

unionLocal<-collect(FoodWaterAndHuman, stringsAsFactors = FALSE)

unionLocal$Date<-as.Date(unionLocal$timestamp_s) #use as.POSIX to get datetime (if needing finer levels)
unionLocal$Datetime<-as.POSIXct(unionLocal$timestamp/1000, origin = "1970-01-01")

unionLocal.dt <- as.data.table(unionLocal) # convert to data.table

dedupunion = unionLocal.dt[order(timestamp), .SD[.N], by="link"] # remove duplicates

# Let me map all arcane eventSourceSid to readeable version (that could be used as proxy categories) in the unioned data
#create data frame with mappings
essidMap<-data.frame(eventSourceSid=c("09f42a37-bb89-426e-845b-faaeb912f615","f65aacd5-140c-49b9-942f-ee94cf032848"),
                     feed=c("Food/Water Shortages","Human Trafficking"))

# USE merge for data frames to get the "feed" easy to read field from essidMap into new df
dedupunion1<-merge(dedupunion,essidMap,by="eventSourceSid")

# NOTE (MAY OR MAY NOT NEED): month var in this combined data is wrong for some unknown reason....going to replace here
#dedupunion1$month<-month(dedupunion1$Date)

# Quick save of dedupunion1 so that I can use it in Dashboard project to create all the plots...
# might also need to save individual aggregated levels of dedupunion1 (unless I just replace with one liner of aggregation)
saveRDS(dedupunion1, "FoodWaterHumTrafficData.rds")

