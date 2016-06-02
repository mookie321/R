www <- "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/cbe.dat"
CBE <- read.table(www,header = TRUE)
Elec.ts <- ts(CBE[,3],start=1958,freq=12)
Elec.ts.diff <- diff(Elec.ts)

Elec.ts.log <- log(Elec.ts)
plot(Elec.ts.log)
Elec.ts.log.diff <- diff(Elec.ts.log)
plot(Elec.ts.log.diff)

acf(Elec.ts.log.diff)
pacf(Elec.ts.log.diff)

x <- Elec.ts.log.diff

max.order <- 4
AIC.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)

for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
  }
}
library(lattice)
arima(Elec.ts.log.diff,order=c(2,0,2))
arima(Elec.ts.log,order=c(2,1,2))

library(forecast)
auto.arima(Elec.ts.log)
arima(Elec.ts.log,order = c(0,1,1),seasonal = list(order=c(0,1,2),frequency=12))
