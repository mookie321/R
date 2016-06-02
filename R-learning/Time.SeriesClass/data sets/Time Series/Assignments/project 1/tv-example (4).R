source("tv.R")

#First, check if this tv series is included
OUR_TITLE <- "Community"

res.1 <- search_by_title(OUR_TITLE,type="series")
res.1
# Source: local data frame [10 x 5]
# 
# Title      Year    imdbID   Type
# (chr)     (chr)     (chr)  (chr)
# 1                             Community 2009–2015 tt1439629 series
# 2                     Community Channel     2006– tt1561809 series

#Check if it has ratings
find_by_title(OUR_TITLE, type="episode", season=1, episode=1)$imdbRating

#Run function
community <-getTv(OUR_TITLE,OUR_YEAR = 2009)
x <- community$x