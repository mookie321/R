x <- arima.sim(list(ar=c(0.7,-0.5,.3),
                    ma=c(0.7,0.5,0.2)),n = 500)

####plot the acf and pacf of x
acf(x)
pacf(x)

max.order <- 10
AIC.matrix <- list()


###Fit a series of ARIMA models with 0 ≤ p ≤ 10, 0 ≤ q ≤ 10, 1 ≤ d ≤ 3, 
##and store the AIC of each fit.
max.d <- 3
for(d in 0:max.d){
  AIC.temp.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)
  for(i in 1:(max.order+1)){
    for(j in 1:(max.order+1)){
      currentArima <- arima(x,order=c(i-1,d,j-1))
      AIC.temp.matrix[i,j] <- AIC(currentArima)
    }
  }
  AIC.matrix[[d+1]] <- AIC.temp.matrix
}



###find the minimum aic of each AIC matrix, and store minimum aic of each matrix
d.0<-min(AIC.matrix[[1]]) ##ARIMA(0 ≤ p ≤ 10,0,0 ≤ q ≤ 10)
d.1<-min(AIC.matrix[[2]]) ##ARIMA(0 ≤ p ≤ 10,1,0 ≤ q ≤ 10)
d.2<-min(AIC.matrix[[3]]) ##ARIMA(0 ≤ p ≤ 10,2,0 ≤ q ≤ 10)
d.3<-min(AIC.matrix[[4]]) ##ARIMA(0 ≤ p ≤ 10,3,0 ≤ q ≤ 10)

###create list of AIC of the minimum of the AIC matrices, and find the 
###minimum AIC of ARIMA models of 0≤ d ≤3
d.list<-list(d.0,d.1,d.2,d.3)
d.best<-which.min(d.list)

###find the row and column of the lowest AIC.
arrayInd(which.min(AIC.matrix[[d.best]]), dim(AIC.matrix[[d.best]]))
aic.col=arrayInd(which.min(AIC.matrix[[d.best]]), dim(AIC.matrix[[d.best]]))[2]
aic.row=arrayInd(which.min(AIC.matrix[[d.best]]), dim(AIC.matrix[[d.best]]))[1]

## store p,d,q of the best model
p.bestmodel=aic.row-1
d.bestmodel=d.best-1
q.bestmodel=aic.col-1



####fit the best model
best.model<-arima(x,order=c(p.bestmodel,d.bestmodel,q.bestmodel))

##plot the residuals of the best models acf and pacf.
acf(best.model$residuals)
pacf(best.model$residuals)




