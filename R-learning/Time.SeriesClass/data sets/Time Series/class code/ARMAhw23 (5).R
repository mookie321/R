lm1 <- lm(x~factor(season)-1,data=HoC)
summary(lm1)

x <- lm1$residuals
ts.plot(x)
acf(x)

max.order <- 10
AIC.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)

for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
  }
}

best.model <- arima(x,order=c(1,0,3))
install.packages("nlme")
library(nlme)
gls1 <- gls(x~factor(season)-1,
            data=HoC,
            correlation = corARMA(value = coef(best.model)[1:4],p=1,q=3))
summary(lm1)
