##choose what tv show and name it OUR_TITLE
OUR_TITLE <-"Breaking Bad"

##check if the show is on imdb
res.1<-search_by_title(OUR_TITLE,type="series")
res.1

#Check if it has ratings
find_by_title(OUR_TITLE, type="episode", season=1, episode=1)$imdbRating

#Run function store ratings in variable named x
breaking_bad<-getTv(OUR_TITLE,OUR_YEAR = 2008)
y<-breaking_bad$x

ts.plot(y)


##code from part 1 used to find best model of tv show (breaking bad)
##plot acf of breaking bad
acf(x)
##plot pacf of breaking bad ratings.
pacf(x)


##fit all the 121 models of AR, MA, and ARMA processes, and store the AICs in a matrix

max.order <- 10
AIC.matrix1<- matrix(0,nrow = max.order+1,ncol= max.order+1)


for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima1 <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix1[i,j] <- AIC(currentArima1) 
    
  }
}
