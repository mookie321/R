library(forecast)

#Following function simulates the next 60 days of the election campaign for all districts
DonaldAll <- function(ad.money.A.effect=0.001,
                      ad.money.B.effect=0.002,
                      ad.money.C.effect=0.003,
                      tv.min.A.effect=0.02,
                      tv.min.B.effect=0.05,
                      tv.min.C.effect=0.05,
                      time.series=TRUE,
                      n.days=60){
  interest.effect <- 6
  
  #generate ad sequences
  x.A.ad <- round(runif(n.days,min = 0, max=5/ad.money.A.effect))
  x.B.ad <- round(runif(n.days,min = 0, max=5/ad.money.B.effect))
  x.C.ad <- round(runif(n.days,min = 0, max=5/ad.money.C.effect))
  
  #generate tv apperance sequence
  x.tv.arima <- arima.sim(list(order=c(1,1,0), ar=0.7),n=n.days)
  x.tv.arima <- tail(x.tv.arima,n.days)/8
  
  tv.min.effect <- max(tv.min.A.effect,tv.min.B.effect,tv.min.C.effect)
  x.tv <- round(5*(exp(-x.tv.arima)/(1+exp(-x.tv.arima)))/tv.min.effect)
  
  #generate unknown interest
  x.interest <- arima.sim(list(order=c(3,0,3), ar=c(0.7,0.2,0.05), ma=c(0.8,0.8,0.8)),n=n.days)
  
  Alford.poll <- 50 +
    interest.effect*x.interest*time.series +
    ad.money.A.effect*x.A.ad + 
    tv.min.A.effect*x.tv
  
  Burford.poll <- 50 +
    interest.effect*x.interest*time.series +
    ad.money.B.effect*x.B.ad + 
    tv.min.B.effect*x.tv
  
  Castleford.poll <- 50 +
    interest.effect*x.interest*time.series +
    ad.money.C.effect*x.C.ad + 
    tv.min.C.effect*x.tv
  
  Alford.poll <- pmin(Alford.poll,100)
  Alford.poll <- pmax(Alford.poll,0)
  Alford.poll <- round(Alford.poll,2)
  
  Burford.poll <- pmin(Burford.poll,100)
  Burford.poll <- pmax(Burford.poll,0)
  Burford.poll <- round(Burford.poll,2)
  
  Castleford.poll <- pmin(Castleford.poll,100)
  Castleford.poll <- pmax(Castleford.poll,0)
  Castleford.poll <- round(Castleford.poll,2)
  
  return(
    data.frame(
      ad.money.A=x.A.ad,
      ad.money.B=x.B.ad,
      ad.money.C=x.C.ad,
      tv.min=x.tv,
      poll.A=Alford.poll,
      poll.B=Burford.poll,
      poll.C=Castleford.poll,
      row.names = 1:n.days
    )
  )
  
}

# Fixed effect of Ad Money on polls for each district (AD.MONEY.EFFECT.FIXED.A for Alford, AD.MONEY.EFFECT.FIXED.B for Burford etc.)
AD.MONEY.EFFECT.FIXED.A <- 0.001
AD.MONEY.EFFECT.FIXED.B <- 0.002
AD.MONEY.EFFECT.FIXED.C <- 0.003

simulation.size <- 5000 # Number of simulations to run
simulation.coverage <- matrix(FALSE,nrow=simulation.size,ncol=3) #Matrix to store simulation (and performance) results

for(i in 1:simulation.size){
  
  #Randomly generate a dataset
  temp <- DonaldAll(ad.money.A.effect = AD.MONEY.EFFECT.FIXED.A,
                    ad.money.B.effect = AD.MONEY.EFFECT.FIXED.B,
                    ad.money.C.effect = AD.MONEY.EFFECT.FIXED.C,
                    time.series = TRUE)
  
  #Use linear regression to estimate the effect of ads on polls
  lm.A <- lm(poll.A~ad.money.A,data=temp)
  lm.B <- lm(poll.B~ad.money.B,data=temp)
  lm.C <- lm(poll.C~ad.money.C,data=temp)
  
  #Construct confidence interval for "ad.money.A.effect"
  temp.confint.A <- confint(lm.A,"ad.money.A",level=0.95)
  temp.lowestimate.A <- temp.confint.A[1]
  temp.upestimate.A<- temp.confint.A[2]
  
  #Construct confidence interval for "ad.money.B.effect"
  temp.confint.B <- confint(lm.B,"ad.money.B",level=0.95)
  temp.lowestimate.B <- temp.confint.B[1]
  temp.upestimate.B<- temp.confint.B[2]
  
  #Construct confidence interval for "ad.money.C.effect"
  temp.confint.C <- confint(lm.C,"ad.money.C",level=0.95)
  temp.lowestimate.C <- temp.confint.C[1]
  temp.upestimate.C<- temp.confint.C[2]
  
  #Check if the intervals contain their true values for each district
  if((AD.MONEY.EFFECT.FIXED.A > temp.lowestimate.A) 
     & (AD.MONEY.EFFECT.FIXED.A < temp.upestimate.A)) {
    simulation.coverage[i,1] <- TRUE
  }
  
  if((AD.MONEY.EFFECT.FIXED.B > temp.lowestimate.B) 
     & (AD.MONEY.EFFECT.FIXED.B < temp.upestimate.B)) {
    simulation.coverage[i,2] <- TRUE
  }
  
  if((AD.MONEY.EFFECT.FIXED.C > temp.lowestimate.C) 
     & (AD.MONEY.EFFECT.FIXED.C < temp.upestimate.C)) {
    simulation.coverage[i,3] <- TRUE
  }
}

#Print Results
temp.string <- paste0("This method has ",
                       round(mean(simulation.coverage[,1]*100),2),"% coverage for district A, ",
                       round(mean(simulation.coverage[,2]*100),2),"% coverage for district B, and ", 
                       round(mean(simulation.coverage[,3]*100),2),"% coverage for district C. ",
                       "It has uniform coverage (for all districts at the same time) ", round(mean(apply(simulation.coverage,1,sum)==3)*100,2),"% of the time. (95% required)")

print(temp.string)
