#### Code to get all of GDELT data from 2015Q4...
gdelt1001<-read.table(unzip("/Users/tomas/Documents/S3-Samples/11-GDELT/2015Q4/2015-OCT/20151001.export.CSV.zip"), sep = "\t", fill=TRUE, stringsAsFactors = F,
                      col.names = c("GLOBALEVENTID","SQLDATE","MonthYear","Year","FractionDate","Actor1Code","Actor1Name","Actor1CountryCode","Actor1KnownGroupCode","Actor1EthnicCode",
                                    "Actor1Religion1Code","Actor1Religion2Code","Actor1Type1Code","Actor1Type2Code","Actor1Type3Code","Actor2Code","Actor2Name","Actor2CountryCode",
                                    "Actor2KnownGroupCode","Actor2EthnicCode","Actor2Religion1Code","Actor2Religion2Code","Actor2Type1Code","Actor2Type2Code","Actor2Type3Code",
                                    "IsRootEvent","EventCode","EventBaseCode","EventRootCode","QuadClass","GoldsteinScale","NumMentions","NumSources","NumArticles","AvgTone","Actor1Geo_Type",
                                    "Actor1Geo_FullName","Actor1Geo_CountryCode","Actor1Geo_ADM1Code","Actor1Geo_Lat","Actor1Geo_Long","Actor1Geo_FeatureID","Actor2Geo_Type",
                                    "Actor2Geo_FullName","Actor2Geo_CountryCode","Actor2Geo_ADM1Code","Actor2Geo_Lat","Actor2Geo_Long","Actor2Geo_FeatureID","ActionGeo_Type",
                                    "ActionGeo_FullName","ActionGeo_CountryCode","ActionGeo_ADM1Code","ActionGeo_Lat","ActionGeo_Long","ActionGeo_FeatureID","DATEADDED","SOURCEURL"))

#code to read in the data for GDELT data
rep(20151001:20151031)
rep(20151101:20151130)
rep(20151201:20151231)

# code to read in all data in 2015Q4/2015-OCT
#Current working directory: "/Users/tomas/Documents/IQT/Sample_Dashboard"

setwd("/Users/tomas/Documents/S3-Samples/11-GDELT/2015Q4/2015-OCT/")
file_list <- list.files()

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("gdeltq4")){
    gdeltq4 <- read.table(unzip(file), header=FALSE, sep="\t", fill=TRUE, stringsAsFactors = F,
                          col.names = c("GLOBALEVENTID","SQLDATE","MonthYear","Year","FractionDate","Actor1Code","Actor1Name","Actor1CountryCode","Actor1KnownGroupCode","Actor1EthnicCode",
                                        "Actor1Religion1Code","Actor1Religion2Code","Actor1Type1Code","Actor1Type2Code","Actor1Type3Code","Actor2Code","Actor2Name","Actor2CountryCode",
                                        "Actor2KnownGroupCode","Actor2EthnicCode","Actor2Religion1Code","Actor2Religion2Code","Actor2Type1Code","Actor2Type2Code","Actor2Type3Code",
                                        "IsRootEvent","EventCode","EventBaseCode","EventRootCode","QuadClass","GoldsteinScale","NumMentions","NumSources","NumArticles","AvgTone","Actor1Geo_Type",
                                        "Actor1Geo_FullName","Actor1Geo_CountryCode","Actor1Geo_ADM1Code","Actor1Geo_Lat","Actor1Geo_Long","Actor1Geo_FeatureID","Actor2Geo_Type",
                                        "Actor2Geo_FullName","Actor2Geo_CountryCode","Actor2Geo_ADM1Code","Actor2Geo_Lat","Actor2Geo_Long","Actor2Geo_FeatureID","ActionGeo_Type",
                                        "ActionGeo_FullName","ActionGeo_CountryCode","ActionGeo_ADM1Code","ActionGeo_Lat","ActionGeo_Long","ActionGeo_FeatureID","DATEADDED","SOURCEURL"))
  }
  
  # if the merged dataset does exist, append to it
  if (exists("gdeltq4")){
    temp_dataset <-read.table(unzip(file), header=FALSE, sep="\t", fill=TRUE, stringsAsFactors = F,
                              col.names = c("GLOBALEVENTID","SQLDATE","MonthYear","Year","FractionDate","Actor1Code","Actor1Name","Actor1CountryCode","Actor1KnownGroupCode","Actor1EthnicCode",
                                            "Actor1Religion1Code","Actor1Religion2Code","Actor1Type1Code","Actor1Type2Code","Actor1Type3Code","Actor2Code","Actor2Name","Actor2CountryCode",
                                            "Actor2KnownGroupCode","Actor2EthnicCode","Actor2Religion1Code","Actor2Religion2Code","Actor2Type1Code","Actor2Type2Code","Actor2Type3Code",
                                            "IsRootEvent","EventCode","EventBaseCode","EventRootCode","QuadClass","GoldsteinScale","NumMentions","NumSources","NumArticles","AvgTone","Actor1Geo_Type",
                                            "Actor1Geo_FullName","Actor1Geo_CountryCode","Actor1Geo_ADM1Code","Actor1Geo_Lat","Actor1Geo_Long","Actor1Geo_FeatureID","Actor2Geo_Type",
                                            "Actor2Geo_FullName","Actor2Geo_CountryCode","Actor2Geo_ADM1Code","Actor2Geo_Lat","Actor2Geo_Long","Actor2Geo_FeatureID","ActionGeo_Type",
                                            "ActionGeo_FullName","ActionGeo_CountryCode","ActionGeo_ADM1Code","ActionGeo_Lat","ActionGeo_Long","ActionGeo_FeatureID","DATEADDED","SOURCEURL"))
    gdeltq4<-rbind(gdeltq4, temp_dataset)
    rm(temp_dataset)
  }
  
}
# revert back to working directory for project...