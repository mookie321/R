x <- arima.sim(list(ar=c(0.7,-0.5),
                    ma=c(0.8,0.7)),
               n=100)

plot(x)
acf(x)

max.order <- 5
AIC.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)

for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
  }
}

best.model <- arima(x,order=c(2,0,2))
acf(residuals(best.model))
