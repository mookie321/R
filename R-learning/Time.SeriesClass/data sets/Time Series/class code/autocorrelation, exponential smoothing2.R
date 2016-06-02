#Load and plot data
data("sunspots")
plot(sunspots)

#Save the number of datapoints
sunspots.n <- length(sunspots)

#Fit exponential smoothing
EXPO.ALPHA <- 0.2 #Alpha value for smoothing

a <- sunspots[1]

#Create a for loop to calculate all values of a
for(t in 2:sunspots.n){
  a[t] <- EXPO.ALPHA*sunspots[t] + (1-EXPO.ALPHA)*a[t-1]
}

#Plot the fitted smoother and data
ts.plot(sunspots,a,col=c(1,2))

# Note that a[t] uses X[t] (e.g. sunspots[t])
# Prediction of X[t+k] at time t is given by a[t]
# Similarly, prediction of X[t] at time t-1 is given by a[t-1]

# Therefore, to compare the prediction accuracy of our method for one step ahead prediction...
# ...we need to compare a[t-1] versus X[t]

expo.residuals <- a[1:(sunspots.n-1)] - sunspots[2:sunspots.n]

# To evaluate how good this method is we can use either the mean squared errors (MSE):
mean(expo.residuals^2) #322.8939

# or the mean of the absolute deviations (MAD):
mean(abs(expo.residuals)) #12.95376

# In order to obtain the ideal model, we need to find the ideal EXPO.ALPHA value
# Following for loop goes through a series of candidates for ALPHA, and for each candidate ...
# ... fits an exponential smoothing and estimates the error of this model by computing the MSE

EXPO.ALPHA.CANDIDATES <- seq(0,1,length.out=101)
all.MSE <- c()

for(i in 1:length(EXPO.ALPHA.CANDIDATES)){
  
  #Set alpha
  EXPO.ALPHA <- EXPO.ALPHA.CANDIDATES[i]
  
  #Compute a[t] using our previous code
  a <- sunspots[1]
  
  for(t in 2:sunspots.n){
    a[t] <- EXPO.ALPHA*sunspots[t] + (1-EXPO.ALPHA)*a[t-1]
  }
  
  #compute residuals
  expo.residuals <- a[1:(sunspots.n-1)] - sunspots[2:sunspots.n]
  
  #Finally store the MSE
  all.MSE[i] <- mean(expo.residuals^2)
  
}

#Plot the alpha values versus the MSE of the fitted model
plot(EXPO.ALPHA.CANDIDATES,all.MSE)

#Find the best alpha value (one with the lowest MSE)
EXPO.ALPHA.CANDIDATES[which.min(all.MSE)] #0.53

#Set alpha to the best value and obtain the ideal exponential smoothing fit
EXPO.ALPHA <- EXPO.ALPHA.CANDIDATES[which.min(all.MSE)]
a <- sunspots[1]

for(t in 2:sunspots.n){
  a[t] <- EXPO.ALPHA*sunspots[t] + (1-EXPO.ALPHA)*a[t-1]
}

#Plot the fitted exponential smoother
ts.plot(sunspots,a,col=c(1,2),xlim=c(1900,1910))

#Plot the dataset versus the predictions for years 1900-1910
sunspots.predictions <- c(NA,a[-sunspots.n])
ts.plot(sunspots,sunspots.predictions,col=c(1,2),lty=c(1,2),xlim=c(1900,1910))

#alternatively compute the smoother using R
sunspots.HW <- HoltWinters(sunspots,alpha=0.53,beta=FALSE,gamma=FALSE)
sunspots.HW$SSE/sunspots.n #MSE of the model

plot(sunspots.HW)
