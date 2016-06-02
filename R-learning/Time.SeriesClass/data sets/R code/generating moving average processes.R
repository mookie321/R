#### GENERATING MOVING AVERAGE PROCESSES ####
## MA(q=1)

BETA <- c(0.7,0.5,0.3,-0.6,0.9)
BIG_T <- 10000
SIGMA_W <- 1

w_t <- rnorm(BIG_T,mean=0,sd=SIGMA_W)
x_t <- w_t

for(t in (length(BETA)+1):BIG_T){
  allSum <- 0
  for(s in 1:(length(BETA))){
    allSum <- allSum+BETA[s]*w_t[t-s]
  }
  x_t[t] <- w_t[t] + allSum
  
}

plot(x_t)
ts.plot(x_t[1:100])
acf(x_t)

#alternatively



x_t <- arima.sim(list(ma=BETA),n = BIG_T)

x <- arima.sim(list(ma=c(0.7,-0.5,0.2)),n = 500)
ma.x.1 <- arima(x,order = c(0,0,1))
ma.x.2 <- arima(x,order = c(0,0,2))
ma.x.3 <- arima(x,order = c(0,0,3))
ma.x.4 <- arima(x,order = c(0,0,4))
ma.x.5 <- arima(x,order = c(0,0,5))

x <- arima.sim(list(ma=c(0.7,0.5,0.2)),n = 500)
acf(x,lag.max = 40)


x <- arima.sim(list(ar=c(0.8,0.1)),n = 500)
acf(x,lag.max = 40)
