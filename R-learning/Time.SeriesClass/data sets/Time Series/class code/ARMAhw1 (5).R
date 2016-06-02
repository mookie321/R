ggplot(HoC2,aes(y=x,color=factor(season),x=episodeT,group=factor(season)))+
  geom_line(size=2)+geom_point(size=2)+geom_smooth(alpha=.3,method="lm")

ggplot(HoC2,aes(y=x,x=episodeT))+
  geom_line(size=2)+geom_point(size=2)+geom_smooth(alpha=.3,method="lm")


max.order <- 10
AIC.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)

for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
  }
}


best.model <- arima(x,order=c(0,0,0))
best.model2 <- arima(x,order=c(0,0,1)) #ma(1)
best.model3 <- arima(x,order=c(1,0,0)) #ar(1)
tinfoil.model <- arima(x,order=c(2,0,6)) #arma(2,6) (arima(2,0,6))
#acf(residuals(best.model))

HoCA <- data.frame(x=predict(tinfoil.model,n.ahead = 13)$pred,
           season=5,
           episode=1:13,
           episodeT=53:65)

HoCA2 <- data.frame(x=(predict(tinfoil.model,n.ahead = 26)$pred)[14:26],
                   season=6,
                   episode=1:13,
                   episodeT=66:78)


HoC2 <- rbind(HoC,HoCA)

HoC3 <- rbind(HoC2,HoCA2)

ggplot(HoC3,aes(y=x,color=factor(season),x=episodeT,group=factor(season)))+
  geom_line(size=2)+geom_point(size=2)+geom_smooth(alpha=.3,method="lm")
