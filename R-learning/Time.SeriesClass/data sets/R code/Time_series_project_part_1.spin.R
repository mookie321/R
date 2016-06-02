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
best.model<-arima(x,order=c(p,0,q))
acf(best.model$residuals)
pacf(best.model$residuals)


