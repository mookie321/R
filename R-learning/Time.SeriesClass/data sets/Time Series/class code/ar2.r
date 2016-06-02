alpha <- 0.01
sigma2 <- 100

X <- c(0)
for(t in 2:1000){
  X[t] <- alpha*X[t-1] + rnorm(1,sd = sigma2)
}

#acf(X)

X.1 <- X[1:99]
X.0 <- X[2:100]
print(plot(X.0,X.1))

lm1 <- lm(X.0~X.1-1)
print(summary(lm1))
