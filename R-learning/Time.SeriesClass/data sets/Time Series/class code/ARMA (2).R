ALPHA <- c(0.4,-0.2) #p=2
BETA <- c(0.7,0.5) #q=2

BIG_T <- 100

SIGMA_W <- 1

w_t <- rnorm(BIG_T,mean=0,sd=SIGMA_W)
x_t <- w_t

for(t in (max(length(BETA),length(ALPHA))+1):BIG_T){
  allSum <- 0
  for(s in 1:(length(BETA))){
    allSum <- allSum+BETA[s]*w_t[t-s]
  }
  x_t[t] <- w_t[t] + allSum + 
    ALPHA[1]*x_t[t-1] + ALPHA[2]*x_t[t-2]
  
}

ts.plot(x_t)
acf(x_t)