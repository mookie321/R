x <-rnorm(2)
w <- rnorm(200, sd=2)

for (t in 3:200) {
  x[t] <- 0.7 * x[t - 1] - 0.2 * x[t - 2] + w[t]
}


x.t2 <- x[1:198]
# create X[t-1]
x.t1 <- x[2:199]
# create X[t]
x.t <- x[3:200]
temp.lm<-lm(x.t ~ x.t1 + x.t2)

summary(temp.lm)

temp.lm

print(coef(temp.lm))

ts.plot(x)
acf(x)

ts.x<-ts(x)
HoltWinters(ts.x, seasonal = c("additive"))


?HoltWinters
(temp.ar <- ar(x))



print(temp.ar$order)

temp.ar$var.pred[w[x]]

print(summary(temp.ar))



w$var

print(sqrt(temp.ar$var))

temp.ar$var

var.pred

ts.plot(temp.ar$resid)

temp.ar$var.pred

(acf(temp.ar$resid))

acf(temp.ar$resid[-(1:2)])
temp.ar$resid
temp.ar.yw <- ar(temp.ar,method="yule-walker")

getwd()


