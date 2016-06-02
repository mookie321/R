install.packages("gtrendsR")
library(gtrendsR)

user <- "stat4181@gmail.com"
psw <- "winteriscoming"
gconnect(user,psw)

aa <- gtrends(c("flu"),res="7d")
plot(aa)

str(aa)
head(aa$trend)
flu <- ts(aa$trend[,2],start=c(1,1),frequency = 24)
plot(flu)

flu.HW <- HoltWinters(flu)
plot(flu.HW)

length(flu)
flu.train <- window(flu,start=c(1,1),end=c(6,24))
flu.test <- window(flu,start=c(7,1),end=c(7,24))

plot(flu.train,xlim=c(1,8))
lines(flu.test,col=2,lwd=4)

flu.HW1 <- HoltWinters(flu.train)
HW1.pred <- predict(flu.HW1,n.ahead = 24)
lines(HW1.pred,col=4,lwd=3)

HW1.res <- flu.test-c(HW1.pred)
plot(HW1.res)
sum(HW1.res^2) #490.0992

## AR models ##

flu.train.cycle <- cycle(flu.train)
lm1 <- lm(flu.train~as.factor(flu.train.cycle)-1)
plot(lm1$coefficients)

plot(lm1$residuals,type='l')
acf(lm1$residuals)
ar(lm1$residuals)

library(nlme)
install.packages("forecast")
library(forecast)

ar.1.auto <- auto.arima(lm1$residuals,d=0)
gls1 <- gls(flu.train~as.factor(flu.train.cycle)-1,
            correlation = corARMA(value = c(0.95,-0.134),p=1,q=1))
plot(gls1$coefficients)
points(lm1$coefficients,col=2)

plot(flu.train,xlim=c(1,8))
lines(flu.test,col=2,lwd=4)
lines(HW1.pred,col=4,lwd=3)

auto.arima(gls1$residuals,d=0)

gls.pred <- gls1$coefficients
gls.res <- flu.test-gls.pred

plot(gls.res)
lines(HW1.res,col=2)

sum(HW1.res^2) #490.0992
sum(gls.res^2) #710.44

auto.arima(gls1$residuals,d=0)
# Series: gls1$residuals
# ARIMA(2,0,1) with zero mean
# 
# Coefficients:
#   ar1     ar2     ma1
# 0.1466  0.7644  0.6824
# s.e.  0.1395  0.1294  0.1634
# 
# sigma^2 estimated as 2.967:  log likelihood=-282.26
# AIC=572.51   AICc=572.8   BIC=584.39

gls1.arima <- arima(gls1$residuals,order = c(2,0,1))
gls.pred.time <- predict(gls1.arima,n.ahead = 24)$pred


gls.pred.proper <- gls.pred.time+c(gls1$coefficients)
gls.proper.res <- flu.test-gls.pred.proper

plot(gls.res)
lines(HW1.res,col=2)
lines(gls.proper.res,col=3)

sum(HW1.res^2) #490.0992
sum(gls.res^2) #710.44
sum(gls.proper.res^2) #641.443
