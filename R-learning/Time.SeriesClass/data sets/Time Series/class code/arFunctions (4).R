##Create AR(4) with 
#alpha1=0.7
#alpha2=-0.4
#alpha3=-0.1
#alpha4=0.1

alpha <- c(.7, -.4, -.1, .1)

#variance of the error
sigma2 <- 1

SAMPLE.SIZE <- 1000

#Ar's order (or p)
alpha.length <- length(alpha)

#Create first p observations
X <- rnorm(alpha.length, sd=sqrt(sigma2))

#Loop over of the AR equation
for(t in (alpha.length+1):SAMPLE.SIZE){
  X[t] <- alpha[1]*X[t-1]+
    alpha[2]*X[t-2]+
    alpha[3]*X[t-3]+
    alpha[4]*X[t-4]+
    rnorm(1,sd=sqrt(sigma2))
  # sidenote: In the book examples,
  # w[t] is used here (instead of getting one rnorm(1) at each t).
  # Since each element of w[t] is independent of each other, 
  # there is no difference between the two methods
}

#Fit AR with the Yule-Walker equations
ar.X.yw <- ar(X,method="yule-walker")

#Fit AR with MLE equations
ar.X.mle <- ar(X,method="mle")

#Fit AR with OLS (Regression)
ar.X.ols <- ar(X,method="ols")

#Plot the acf of the residuals
acf(ar.X.yw$resid[-(1:alpha.length)])

#Obtain 95% Confidence interval for the first AR coefficient
c(ar.X.yw$ar[1]-1.96*sqrt(ar.X.yw$asy.var.coef[1,1]),
  ar.X.yw$ar[1]+1.96*sqrt(ar.X.yw$asy.var.coef[1,1]))

#Function that builds confidence intervals, the first input should be an AR variable (obtained from the ar() function)
#(issues: this won't run with the ols, needs a simple fix)
arConf <- function(tempAr,orderAr=1,
                   conf.level=.95){
  tempAlpha <- (1-conf.level)/2
  tempZ <- -qnorm(tempAlpha)
  tempCI <- c(tempAr$ar[orderAr]-tempZ*sqrt(tempAr$asy.var.coef[orderAr,orderAr]),
              tempAr$ar[orderAr]+tempZ*sqrt(tempAr$asy.var.coef[orderAr,orderAr]))
  return(tempCI)
}

#Print the 95% confidence interval for the second AR coefficient
arConf(tempAr=ar.X.yw,orderAr=2,conf.level=.95)

arConf(tempAr=ar.X.yw,orderAr=2)
#Same thing since default is conf.level=.95

#Print the 99.5% confidence interval for the fourth AR coefficient
arConf(tempAr=ar.X.yw,orderAr=4,conf.level=.995)
#And the 99.6% confidence interval...
arConf(tempAr=ar.X.yw,orderAr=4,conf.level=.996)

#Predict ten steps ahead from the latest point
ar.X.n10.predictions <- predict(ar.X.yw,n.ahead = 10)$pred