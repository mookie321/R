##https://dataveld.wordpress.com/2016/02/15/getting-started-with-r-maps-in-microsoft-power-bi/
install.packages("maps")
install.packages("mapproj")
library(maps)
library(mapproj)

##plot map of the world
map()
##plot map of the US
map("state")
##plot map of the US and change the color
map("state", col="#01B8AA")
##plot map of the US change the color, and the background
map("state", col="#01B8AA", bg="#374649")
##plot map of the US change the color, and the background, and fill in the states
map("state", col="#01B8AA", bg="#374649", fill=TRUE)
##using the lat and long from mercator 
map("state", col="#01B8AA", bg="#374649", fill=TRUE, proj="mercator")

##what they use for this tutorial
map("state", col="#01B8AA", bg="#374649", proj="albers", param=c(39,45))
points(mapproject(dataset$Longitude, dataset$Latitude))


