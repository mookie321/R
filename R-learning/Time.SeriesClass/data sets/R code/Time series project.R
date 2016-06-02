##problem 1 for the project
##simulate x using arima.sim
x <- arima.sim(list(ar=c(0.7,-0.5,.3),
                    ma=c(0.7,0.5,0.2)),n = 500)

##acf and pcf of x
acf(x)
pacf(x)

##fit all the 121 models of AR, MA, and ARMA processes, and store the AICs in a matrix
## and find the lowest AIC
##and show the p and q of the lowest aic
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
best.model<-arima(x,order=c(p-1,0,q-1))
acf(best.model$residuals)
pacf(best.model$residuals)


#######################################long way
##simulate ar(p=0,...,10)
ar.x.0 <- arima(x,order = c(0,0,0))
ar.x.1 <- arima(x,order = c(1,0,0))
ar.x.2 <- arima(x,order = c(2,0,0))
ar.x.3 <- arima(x,order = c(3,0,0))
ar.x.4 <- arima(x,order = c(4,0,0))
ar.x.5 <- arima(x,order = c(5,0,0))
ar.x.6 <- arima(x,order = c(6,0,0))
ar.x.7 <- arima(x,order = c(7,0,0))
ar.x.8 <- arima(x,order = c(8,0,0))
ar.x.9 <- arima(x,order = c(9,0,0))
ar.x.10 <- arima(x,order = c(10,0,0))

##store aic of ar(p=0,...,10)
ar.aic.0<-ar.x.0$aic
ar.aic.1<-ar.x.1$aic
ar.aic.2<-ar.x.2$aic
ar.aic.3<-ar.x.3$aic
ar.aic.4<-ar.x.4$aic
ar.aic.5<-ar.x.5$aic
ar.aic.6<-ar.x.6$aic
ar.aic.7<-ar.x.7$aic
ar.aic.8<-ar.x.8$aic
ar.aic.9<-ar.x.9$aic
ar.aic.10<-ar.x.10$aic

##simulate ma(q=1,...,10)
ma.x.1 <- arima(x,order = c(0,0,1))
ma.x.2 <- arima(x,order = c(0,0,2))
ma.x.3 <- arima(x,order = c(0,0,3))
ma.x.4 <- arima(x,order = c(0,0,4))
ma.x.5 <- arima(x,order = c(0,0,5))
ma.x.6 <- arima(x,order = c(0,0,6))
ma.x.7 <- arima(x,order = c(0,0,7))
ma.x.8 <- arima(x,order = c(0,0,8))
ma.x.9 <- arima(x,order = c(0,0,9))
ma.x.10 <- arima(x,order = c(0,0,10))

##store aic ma(q=1,....,10)
ma.aic.1<-ma.x.1$aic
ma.aic.2<-ma.x.2$aic
ma.aic.3<-ma.x.3$aic
ma.aic.4<-ma.x.4$aic
ma.aic.5<-ma.x.5$aic
ma.aic.6<-ma.x.6$aic
ma.aic.7<-ma.x.7$aic
ma.aic.8<-ma.x.8$aic
ma.aic.9<-ma.x.9$aic
ma.aic.10<-ma.x.10$aic


arma.1<-arima(x,order = c(1,0,1))
########################

