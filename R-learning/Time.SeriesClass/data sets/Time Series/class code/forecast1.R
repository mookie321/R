#Install the necessary packages
install.packages("zoo")
install.packages("rdatamarket")

#Load the package
library(rdatamarket)

#Load the dataset and store it as "milk"
milk <- dmseries("https://datamarket.com/data/set/22ox/monthly-milk-production-pounds-per-cow-jan-62-dec-75#!ds=22ox&display=line")

#Plot "milk"
plot(milk)

#Plot the autocorrelation function of "milk"
acf(milk)

#Create a new dataset that contains only the first 11 years of data
milk.new <- milk[1:(12*11)]
str(milk.new)

#Fit a Holt Winters to "milk.new"
milk.new.hw <- HoltWinters(milk.new)

#Recover the chosen values for alpha, beta and gamma
milk.new.hw$alpha
milk.new.hw$beta
milk.new.hw$gamma

#Print the sum squared errors
milk.new.hw$SSE

#Fit an another Holt Winters to "milk.new", using a fixed alpha value (alpha=0.5)
milk.new.hw.2 <- HoltWinters(milk.new,alpha=.5)
milk.new.hw.2$beta
milk.new.hw.2$gamma
milk.new.hw.2$SSE

#Fit a temporary HW with given parameters
milk.new.hw.temp <- HoltWinters(milk.new,
                                alpha=.5,
                                beta=.1,
                                gamma=.3)

aa <- milk.new.hw.temp

#Recover the one step ahead forecasts
xHat <- aa$fitted[,1]
x <- milk.new[-(1:12)]

#Compare forecasts to actual data to calculate residuals (errors)
x.res <- x-xHat

#Calculate and print the MSE (mean squared error)
print(mean(x.res^2))

#Calculate and print the MAD (mean absolute deviation)
print(mean(abs(x.res)))