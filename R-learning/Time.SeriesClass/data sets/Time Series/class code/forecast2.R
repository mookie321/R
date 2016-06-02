#Following code fits HW at each stage and then evaluates the forecast accuracy on a rolling horizon

#install.packages("forecast")
library(forecast)

milk.new <- milk[1:(12*11)]
milk.new.hw <- HoltWinters(milk.new)
xhat <- forecast(milk.new.hw,1)$mean
allResiduals <- c()

for(t in 133:168){
  allResiduals[t-132] <- milk[t]-xhat
  
  milk.new <- milk[1:t]
  milk.new.hw <- HoltWinters(milk.new)
  xhat <- forecast(milk.new.hw,1)$mean
}

print(mean(allResiduals^2))
print(mean(abs(allResiduals)))