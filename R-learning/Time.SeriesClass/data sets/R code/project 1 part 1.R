##simulate x using arima.sim
x <- arima.sim(list(ar=c(0.7,-0.5,.3),
                    ma=c(0.7,0.5,0.2)),n = 500)

##print acf and pacf plots of x
acf(x)
pacf(x)

##fit all the 121 models of AR, MA, and ARMA processes, and store the AICs in a matrix

max.order <- 10
AIC.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)


for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
   
  }
}

### and find the lowest AIC
##and show the p and q of the lowest aic
min.aic<- which(AIC.matrix == min(AIC.matrix), arr.ind=TRUE)


p=min.aic[,1]-1
q=min.aic[,2]-1

##fit the best model based on lowest aic
best.model<-arima(x,order=c(p,0,q))

##plot the residuals of the best models acf and pacf.
acf(best.model$residuals)
pacf(best.model$residuals)


