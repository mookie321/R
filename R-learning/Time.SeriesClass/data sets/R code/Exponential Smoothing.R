data("sunspots")
plot (sunspots)
str(sunspots)
str(as.numeric(sunspots))
sunspots.num<-as.numeric(sunspots)

a<- sunspots[1]
alpha<-.9

for(t in 2:2820){
  a[t] <- alpha*(sunspots[t]-a[t-1]+a[t-1])
}

lines(a,col=2,lwd=3)
lines(seq(1749,1984,length.out=length(a)),a,col=3,lwd=3)

plot(sunspots)

str(a)
sunspots[150]-a[149]
plot(a)
str(sunspots[-1])
errors <- a[1:(length(a) - 1)] - sunspots[-1]
mean((errors)^2)


alpha<- .9
a<- sunspots[1]


for(t in 2:2820){
  a[t] <- alpha*(sunspots[t]-a[t-1]+a[t-1])
}

errors <- a[1:(length(a) - 1)] - sunspots[-1]

print(mean((errors)^2))

plot(sunspots)



hw.suns<-HoltWinters(sunspots,alpha=,beta=FALSE,gamma=FALSE)
plot(hw.suns)
hw.suns$alpha
