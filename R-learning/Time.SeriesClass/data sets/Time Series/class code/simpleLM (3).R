true.beta <- 2
max.budget <- 1000000
sample.size <- 40
noise.sd <- 100000

plot(0,0,type="n",xlim=c(1.8,2.2),ylim=c(0,205))

abline(v=2,col=2,lwd=5)
for(i in 1:100){
  marketing.budget <- runif(sample.size,min = 0,max=max.budget)
  
  profit <- true.beta * marketing.budget + rnorm(sample.size,
                                                 mean=0,
                                                 sd = noise.sd)
  
  templm <- lm(profit~marketing.budget)
  temp.interval <- confint(templm,"marketing.budget",level=.95)
  
  points(templm$coefficients[2],i*2,col=1)
  lines(x = c(temp.interval[1],
              temp.interval[2]),
        y= c(i*2,i*2))
}
