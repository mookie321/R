lm1 <- lm(x~factor(season)-1,data=Wire)
summary(lm1)

x <- lm1$residuals
ts.plot(x)
acf(x)

max.order <- 10
AIC.matrix <- matrix(1000,nrow = max.order+1,ncol= max.order+1)

for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
  }
}

best.model <- arima(x,order=c(1,0,3))
acf(residuals(best.model))


install.packages("nlme")

COEFS<-coef(best.model)[1:6]
COEFS<-.5*COEFS  
coef(best.model)[1:9]
COEFS

library(nlme)
gls1 <- gls(x~factor(season)-1,
            data=Wire,
            correlation = corARMA(value = coef(best.model)[1:4],p=1,q=3))
ts.plot(Wire$x)

gls2<-gls(x~factor(season)-1,data=Wire)

Wire
gls(...,correlation=corARMA(COEFS,p=P,q=Q))

gls1<-update(gls2,
       corr=corARMA(COEFS,p=2, q=4) )
summary(lm1)
