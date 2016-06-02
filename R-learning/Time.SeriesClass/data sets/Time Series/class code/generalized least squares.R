#### Alice, weight loss ####

INTERCEPT <- 160 #Initial weight
SLOPE <- -0.1 #Diet's weight loss per day 
SAMPLE_SIZE <- 100 #Number of days

SIGMA_NOISE <- 2 #Standard deviation of the white noise

#Generate AR with alpha=0.7, sigma_w=SIGMA_NOISE
w <- arima.sim(model=list(ar=c(.7)),
               n = SAMPLE_SIZE,
               sd=SIGMA_NOISE)

#Time covariates
tt <- 1:SAMPLE_SIZE

#Alice's Weight
x <- INTERCEPT + SLOPE*tt + w

#Plot change in time
ts.plot(x)

#Add main trend to the plot
abline(a = INTERCEPT, b=SLOPE, col=2, lwd=3)

#Fit LM
lm.temp <- lm(x~tt)

#Fit GLS with AR alpha=0.7
library(nlme) #load the package first
gls.temp <- gls(x~tt,correlation = corAR1(0.7)) #fit gls
summary(gls.temp) #display summary

coef(gls.temp)
confint(gls.temp,parm = "tt") #display conf interval

#### Everyone ####

# repeat the previous experiment with 20 different people
# each time, fit lm and gls
# store the fitted coefficients and the intervals
# display intervals and fits using lm and gls separately

lmIntervals <- matrix(NA,nrow=20,ncol=3)
glsIntervals <- matrix(NA,nrow=20,ncol=3)

for(temp.counter in 1:20){
  
  #generate current dataset
  random.intercept <- runif(1,min=120,160)
  w <- arima.sim(model=list(ar=c(.7)),
                 n = SAMPLE_SIZE,
                 sd=SIGMA_NOISE)
  tt <- 1:SAMPLE_SIZE
  x <- random.intercept + SLOPE*tt + w
  
  #fit lm and gls to the current dataset
  lm.temp <- lm(x~tt)
  gls.temp <- gls(x~tt,correlation = corAR1(0.7)) #fit gls
  
  #Store lm coefficients and the intervals
  lmIntervals[temp.counter,1] <- coef(lm.temp)[2]
  temp.conf <- confint(lm.temp,parm = "tt")
  lmIntervals[temp.counter,2:3] <- temp.conf
  
  #Store gls coefficients and the intervals
  glsIntervals[temp.counter,1] <- coef(gls.temp)[2]
  temp.conf <- confint(gls.temp,parm = "tt")
  glsIntervals[temp.counter,2:3] <- temp.conf
  
}

## Plot LM intervals on left, and GLS intervals on right

par(mfrow=c(1,2)) #change plotting settings

plot(0,0,type='n',xlim=c(-0.2,0),ylim=c(0,21),main="LM Intervals") #prep plot
abline(v=SLOPE,col=2,lwd=3)

for(temp.counter in 1:20){
  
  points(lmIntervals[temp.counter,1],temp.counter,lwd=3)
  lines(lmIntervals[temp.counter,2:3],c(temp.counter,temp.counter))
  
}

plot(0,0,type='n',xlim=c(-0.2,0),ylim=c(0,21),main="GLS Intervals") #prep plot
abline(v=SLOPE,col=2,lwd=3)

for(temp.counter in 1:20){
  
  points(glsIntervals[temp.counter,1],temp.counter,lwd=3)
  lines(glsIntervals[temp.counter,2:3],c(temp.counter,temp.counter))
  
}