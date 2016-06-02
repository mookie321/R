install.packages("devtools")
install.packages("dplyr")
install.packages("pbapply")
install.packages("stringr")

devtools::install_github("hrbrmstr/omdbapi")

##loading the packages
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



OUR_TITLE <-"Breaking Bad"

res.1<-search_by_title(OUR_TITLE,type="series")
res.1

#Check if it has ratings
find_by_title(OUR_TITLE, type="episode", season=1, episode=1)$imdbRating

#Run function
breaking_bad<-getTv(OUR_TITLE,OUR_YEAR = 2008)
x<-breaking_bad$x

acf(x)
pacf(x)

max.order <- 10
AIC.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)

Best_I <- 0
Best_J<-0
LOWEST_AIC <- 10^10

for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
    temp.arima <- arima(x,order=c(j,0,i))
    if(AIC(currentArima) < LOWEST_AIC){
      LOWEST_AIC <- AIC(currentArima)
      Best_I <- i
      Best_J <- j
    }
  }
}

p=Best_I-1
q=Best_J-1

##fit a best model with a arima(p,0,q)
##plot acf and pacf of the best model
best.model<-arima(x,order=c(p,0,q))
acf(best.model$residuals)
pacf(best.model$residuals)
best.model

auto.arima(x)


bb.lm<-lm(x~factor(season)-1,data=breaking_bad)
summary(bb.lm)

