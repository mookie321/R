##choose what tv show and name it OUR_TITLE
OUR_TITLE <-"entourage"

##check if the show is on imdb
res.1<-search_by_title(OUR_TITLE,type="series")
res.1

#Check if it has ratings
find_by_title(OUR_TITLE, type="episode", season=1, episode=1)$imdbRating

#Run function store ratings in variable named x
entourage<-getTv(OUR_TITLE,OUR_YEAR = 2004)
x<-entourage$x

##acf and pacf of entourage
acf(x)
pacf(x)
##look at the ts.plot of entourage
ts.plot(x)

##fit all the 121 models of AR, MA, and ARMA processes, and store the AICs in a matrix

max.order <- 10
AIC.matrix <- matrix(1000,nrow = max.order+1,ncol= max.order+1)


for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
    
  }
}
### and find the lowest AIC store as min.aic
min.aic<- which(AIC.matrix == min(AIC.matrix), arr.ind=TRUE)
## find the second lowest AIC store as min.aic2
min.aic2<-which(AIC.matrix == sort(AIC.matrix,partial=n-119)[n-119], arr.ind = TRUE)
## find the third lowest AIC store as min.aic3
min.aic3<-which(AIC.matrix == sort(AIC.matrix,partial=n-118)[n-118], arr.ind = TRUE)



n <- length(AIC.matrix)
sort(AIC.matrix,partial=n-119)[n-119]
sort(AIC.matrix,partial=n-118)[n-118]

##store the p and q for the best model
p=min.aic[,1]-1
q=min.aic[,2]-1
##store the p and q for the second best model
p2=min.aic2[,1]-1
q2=min.aic2[,2]-1
##store the p and q for the third best model
p3=min.aic3[,1]-1
q3=min.aic3[,2]-1


min(AIC.matrix)

##fit the best model based on lowest aic
best.model<-arima(x,order=c(p,0,q))
##fit the second best model based on second lowest aic
best.model2<-arima(x,order=c(p2,0,q2))
##fit the third best model based on third lowest aic
best.model3<-arima(x,order=c(p3,0,q3))

print(best.model, prmsd=TRUE)
print(best.model2, prmsd=TRUE)
print(best.model3, prmsd=TRUE)




##plot the residuals of the best model's acf and pacf.
acf(best.model$residuals)
pacf(best.model$residuals)
##plot the residuals of the second best model's acf and pacf.
acf(best.model2$residuals)
pacf(best.model2$residuals)
##plot the residuals of the third best model's acf and pacf.
acf(best.model3$residuals)
pacf(best.model3$residuals)
