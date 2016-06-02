breakingBad <-getTv("Breaking Bad",OUR_YEAR = 2008)
x <- breakingBad$x
 
lm1 <- lm(x~factor(season)-1,data=breakingBad)
summary(lm1)
 
x <- lm1$residuals
ts.plot(x)
acf(x)

max.order <- 10
AIC.matrix <- list()

max.d <- 3
for(d in 0:max.d){
  AIC.temp.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)
  for(i in 1:(max.order+1)){
    for(j in 1:(max.order+1)){
      AIC.temp.matrix[i,j] <- tryCatch(
        {
          #try
          currentArima <- arima(x,order=c(i-1,d,j-1))
          AIC(currentArima)
        },
        error=function(cond){
          errMessage <- paste(i-1,d,j-1,sep=",")
          errMessage <- paste0("Error in fitting ARIMA(",errMessage,"), setting AIC to 10^6")
          message(errMessage)
          return(10^6)
        },
        warning=function(cond){
          errMessage <- paste(i-1,d,j-1,sep=",")
          errMessage <- paste0("Error in fitting ARIMA(",errMessage,"), setting AIC to 10^6")
          message(errMessage)
          return(10^6)
        })
    }
  }
  AIC.matrix[[d+1]] <- AIC.temp.matrix
}

#The ARIMA results (d=1,2,3) are stored in AIC.matrix[[2]], AIC.matrix[[3]] and AIC.matrix[[4]]

best.model <- arima(x,order=c(1,0,3))
install.packages("nlme")
library(nlme)
gls1 <- gls(x~factor(season)-1,
            data=breakingBad,
            correlation = corARMA(value = coef(best.model)[1:4],p=1,q=3))
summary(lm1)
