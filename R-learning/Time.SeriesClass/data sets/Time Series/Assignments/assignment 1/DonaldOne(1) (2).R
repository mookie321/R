library(forecast)

#Following function simulates the next 60 days of the election campaign for one district (Alford)
DonaldOne <- function(ad.money.effect=0.001,
                      tv.min.effect=0.05,
                      time.series=TRUE,
                      n.days=60){
  interest.effect <- 6
  
  #generate ad sequence
  x.ad <- round(runif(n.days,min = 0, max=5/ad.money.effect))
  
  #generate tv apperance sequence
  x.tv.arima <- arima.sim(list(order=c(1,1,0), ar=0.7),n=n.days)
  x.tv.arima <- tail(x.tv.arima,n.days)/8
  x.tv <- round(5*(exp(-x.tv.arima)/(1+exp(-x.tv.arima)))/tv.min.effect)
  
  #generate unknown interest
  x.interest <- arima.sim(list(order=c(3,0,3), ar=c(0.7,0.2,0.05), ma=c(0.8,0.8,0.8)),n=n.days)
  
  Alford.poll <- 50 +
    interest.effect*x.interest*time.series +
    ad.money.effect*x.ad + 
    tv.min.effect*x.tv
  
  Alford.poll <- pmin(Alford.poll,100)
  Alford.poll <- pmax(Alford.poll,0)
  Alford.poll <- round(Alford.poll,2)
  
  return(
    data.frame(
      ad.money=x.ad,
      tv.min=x.tv,
      poll.A=Alford.poll,
      row.names = 1:n.days
    )
  )
  
}

AD.MONEY.EFFECT.FIXED <- 0.001 # Fixed effect of Ad Money on polls
simulation.size <- 5000 # Number of simulations to run
simulation.coverage <- rep(FALSE,simulation.size) #Vector to store simulation (and performance) results

for(i in 1:simulation.size){
  
  #Randomly generate a dataset
  temp <- DonaldOne(ad.money.effect = AD.MONEY.EFFECT.FIXED,
                    time.series=TRUE)
  
  #Estimate effect of ads on polls
  temp.estimate <- (temp$poll.A-mean(temp$poll.A))/
    (temp$ad.money-mean(temp$ad.money))
  
  #Provide confidence intervals
  temp.lowestimate <- mean(temp.estimate) - 1.96 * sd(temp.estimate)
  temp.upestimate<- mean(temp.estimate) + 1.96 * sd(temp.estimate)
  
  #Check if the interval contains the true (fixed) value
  if((AD.MONEY.EFFECT.FIXED > temp.lowestimate) 
     & (AD.MONEY.EFFECT.FIXED < temp.upestimate)) {
    simulation.coverage[i] <- TRUE
  }
}

#Print out results
temp.string <- paste0("This method has ",round(mean(simulation.coverage*100),2),"% coverage")
print(temp.string)
