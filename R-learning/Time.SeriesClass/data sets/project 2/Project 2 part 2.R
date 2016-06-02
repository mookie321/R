##install packages
install.packages("devtools")
install.packages("dplyr")
install.packages("pbapply")
install.packages("stringr")

devtools::install_github("hrbrmstr/omdbapi")

library(dplyr)
library(pbapply)
library(omdbapi)

getTv <- function(OUR_TITLE,OUR_YEAR=NA){
  x <- c()
  x.season <- c()
  x.episode <- c()
  
  #loop over seasons (Assuming maximum of 50 seasons)
  for(this.season in 1:50){
    #check if this season exists, otherwise break the for loop
    if(dim(find_by_title(OUR_TITLE,type="episode",
                         season=this.season,
                         episode=1,
                         year_of_release = OUR_YEAR))[1] == 0){
      break
    } else {
      #now go over the episodes
      
      #first wait for 2 seconds (this amount probably needs to be higher)
      #(we don't want to get blacklisted from the API)
      print("Waiting for 2 seconds...")
      Sys.sleep(2)
      
      #looping over episodes (maximum is 50)
      for(this.episode in 1:50){
        if(dim(find_by_title(OUR_TITLE, 
                             type="episode",
                             season=this.season,
                             episode=this.episode,
                             year_of_release = OUR_YEAR))[1] == 0){
          break
        } else {
          if(this.episode %% 9 ==0){
            print("Waiting for 2 seconds...")
            Sys.sleep(2)
          }
          this.rating <- find_by_title(OUR_TITLE, 
                                       type="episode",
                                       season=this.season,
                                       episode=this.episode,
                                       year_of_release = OUR_YEAR)$imdbRating
          x <- c(x,this.rating)
          x.season <- c(x.season,this.season)
          x.episode <- c(x.episode,this.episode) 
        }
        
      }
      
    }
  }
  
  return(data.frame(x=x,season=x.season,episode=x.episode))
}



##choose what tv show and name it OUR_TITLE
OUR_TITLE <-"The Wire"

##check if the show is on imdb
res.1<-search_by_title(OUR_TITLE,type="series")
res.1

#Check if it has ratings
find_by_title(OUR_TITLE, type="episode", season=1, episode=1)$imdbRating

Wire<-getTv(OUR_TITLE,OUR_YEAR = 2002)
x<-Wire$x


###FIT  a least squares model
lm1 <- lm(x~factor(season)-1,data=Wire)
##report the summary of the least squares model
summary(lm1)
## plot the residuals of the fitted model
x <- lm1$residuals

ts.plot(x)
acf(x)
pacf(x)


max.order <- 10
AIC.matrix <- list()


###Fit a series of ARIMA models with 0 ≤ p ≤ 10, 0 ≤ q ≤ 10, 1 ≤ d ≤ 3, 
##and store the AIC of each fit.
max.d <- 3
for(d in 0:max.d){
  AIC.temp.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)
  for(i in 1:(max.order+1)){
    for(j in 1:(max.order+1)){
      AIC.temp.matrix[i,j] <- tryCatch(
        {
          #try
          currentArima <- arima(x,order=c(i-1,d,j-1))
          AIC(currentArima)
        },
        error=function(cond){
          errMessage <- paste(i-1,d,j-1,sep=",")
          errMessage <- paste0("Error in fitting ARIMA(",errMessage,"), setting AIC to 10^6")
          message(errMessage)
          return(10^6)
        },
        warning=function(cond){
          errMessage <- paste(i-1,d,j-1,sep=",")
          errMessage <- paste0("Error in fitting ARIMA(",errMessage,"), setting AIC to 10^6")
          message(errMessage)
          return(10^6)
        })
    }
  }
  AIC.matrix[[d+1]] <- AIC.temp.matrix
}
      
      
      




###find the minimum aic of each AIC matrix, and store minimum aic of each matrix
d.0<-min(AIC.matrix[[1]]) ##ARIMA(0 ≤ p ≤ 10,0,0 ≤ q ≤ 10)
d.1<-min(AIC.matrix[[2]]) ##ARIMA(0 ≤ p ≤ 10,1,0 ≤ q ≤ 10)
d.2<-min(AIC.matrix[[3]]) ##ARIMA(0 ≤ p ≤ 10,2,0 ≤ q ≤ 10)
d.3<-min(AIC.matrix[[4]]) ##ARIMA(0 ≤ p ≤ 10,3,0 ≤ q ≤ 10)

###create list of AIC of the minimum of the AIC matrices, and find the 
###minimum AIC of ARIMA models of 0≤ d ≤3
d.list<-list(d.0,d.1,d.2,d.3)
d.best<-which.min(d.list)


###find the 2nd and 3rd lowest AIC from the aic.matrix(d.best) with the lowest aic.
n <- length(AIC.matrix[[d.best]])
min.aic2<-sort(AIC.matrix[[d.best]],partial=n-119)[n-119]
min.aic3<-sort(AIC.matrix[[d.best]],partial=n-118)[n-118]

###find the row and column of the lowest AIC.
arrayInd(which.min(AIC.matrix[[d.best]]), dim(AIC.matrix[[d.best]]))
aic.col=arrayInd(which.min(AIC.matrix[[d.best]]), dim(AIC.matrix[[d.best]]))[2]
aic.row=arrayInd(which.min(AIC.matrix[[d.best]]), dim(AIC.matrix[[d.best]]))[1]

## store p,d,q of the best model
p.bestmodel=aic.row-1
d.bestmodel=d.best-1
q.bestmodel=aic.col-1



####fit the 3 best models of lowest aic
best.model<-arima(x,order=c(p.bestmodel,d.bestmodel,q.bestmodel))
best.model2<-arima(x,order=c(7,0,1))
best.model3<-arima(x,order=c(4,0,8))



##plot the residuals of the best model acf and pacf.
acf(best.model$residuals)
pacf(best.model$residuals)

##install stargazer which helps me create a table of the arima results to compare the 3 best models.
install.packages("stargazer")
library(stargazer)
regression_results<-stargazer(lm1,gls1, type="text",title = "Regression Results")
stargazer(best.model,best.model2,best.model3,type="text")

