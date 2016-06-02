white.noise <- rnorm(10000,mean=0,sd=10)
#plot(white.noise,type='l')

#acf(white.noise)

random.walk <- cumsum(white.noise)
plot(random.walk,type='l',ylim=c(-2000,2000),lwd=.5)


for(i in 1:100){
  
  white.noise <- rnorm(10000,mean=0,sd=10)
  #plot(white.noise,type='l')
  
  #acf(white.noise)
  
  random.walk <- cumsum(white.noise)
  print(lines(random.walk,type='l',lwd=.5))
  
}