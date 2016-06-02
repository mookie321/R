MAaic <- function(MA.process,max.order=10){
  BEST_I <- 0
  LOWEST_AIC <- 10^10
  for(i in 0:max.order){
    temp.arima <- arima(MA.process,order=c(0,0,i))
    if(AIC(temp.arima) < LOWEST_AIC){
      LOWEST_AIC <- AIC(temp.arima)
      BEST_I <- i
    }
  }
  return(BEST_I)
}


x <- arima.sim(list(ma=c(0.8,0.6,-0.1,0.1)),n = 500)
MAaic(x)


### with the forecast package
library(forecast)
x <- arima.sim(list(ma=c(0.8,0.6,-0.1,0.1)),n = 500)
auto.arima(x,d=0,max.p = 0)
