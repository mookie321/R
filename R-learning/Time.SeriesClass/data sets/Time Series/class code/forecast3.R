#Following code fits HW at each stage and then evaluates the 6-step ahead forecast accuracy on a rolling horizon

#install.packages("forecast")
library(forecast)

milk.new.hw <- HoltWinters(milk.new)
xhat <- (forecast(milk.new.hw,6)$mean)[6]
allResiduals <- c()
allAlphas <- c()

for(t in 133:162){
  allResiduals[t-132] <- milk[t+6]-xhat
  allAlphas[t-132] <- milk.new.hw$alpha
  
  milk.new <- milk[1:t]
  milk.new.hw <- HoltWinters(milk.new)
  xhat <- (forecast(milk.new.hw,6)$mean)[6]
}

print(mean(allResiduals^2))
print(mean(abs(allResiduals)))