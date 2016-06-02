alpha <- 0.8
sigma2 <- 1

X <- matrix(0,nrow=100,
            ncol=100000)
for(j in 1:100000){
  for(t in 2:100){
    X[t,j] <- alpha*X[t-1,j] + rnorm(1,sd = sigma2)
  }
}

X.average <- apply(X,1,mean)
ts.plot(X.average)