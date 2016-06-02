x <- arima.sim(list(ar=c(0.7,-0.5,.3),
                    ma=c(0.7,0.5,0.2)),n = 500)

max.order <- 10

AIC.matrix.0 <- matrix(10000,nrow = max.order+1,ncol= max.order+1)

for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix.0[i,j] <- AIC(currentArima) 
  }
}

AIC.matrix.1 <- matrix(10000,nrow = max.order+1,ncol= max.order+1)

for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,1,j-1))
    AIC.matrix.1[i,j] <- AIC(currentArima) 
  }
}

AIC.matrix.2 <- matrix(10000,nrow = max.order+1,ncol= max.order+1)

for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,2,j-1))
    AIC.matrix.2[i,j] <- AIC(currentArima) 
  }
}

AIC.matrix.3 <- matrix(10000,nrow = max.order+1,ncol= max.order+1)

for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,3,j-1))
    AIC.matrix.3[i,j] <- AIC(currentArima) 
  }
}

min(AIC.matrix.0)
min(AIC.matrix.1)
min(AIC.matrix.2)
min(AIC.matrix.3)
