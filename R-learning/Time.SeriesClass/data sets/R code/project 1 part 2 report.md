
####This plot of the acf of the breaking bad rating shows that there is significant autocorrelation with the data. It seems that there is a significant positive correlation up to lag 2. Also there seems to be a significant positive correlation at around lag 6 and 7, this could be due to the fact that new plot lines happen around 6 episodes in.


###This plot shows that an arma model should be used to fit the data, and it may have an ar=1 or 2, because the pacf goes to zero after lag 1.

arima(x = x, order = c(p, 0, q))
Coefficients:
         ar1      ma1      ma2      ma3      ma4     ma5     ma6  intercept
      0.9585  -0.3959  -0.0769  -0.2306  -0.6272  0.2692  0.4208     9.0892
s.e.  0.0507   0.1254   0.1397   0.1273   0.1330  0.1534  0.1289     0.2701

sigma^2 estimated as 0.1015:  log likelihood = -20.7,  aic = 59.39

arima(x = x, order = c(p2, 0, q2))
Coefficients:
         ar1      ar2     ar3      ar4     ar5      ma1     ma2      ma3  intercept
      1.2547  -1.1814  1.1366  -0.6604  0.4002  -0.6559  0.9120  -0.7701     9.1133
s.e.  0.1480   0.1836  0.1899   0.1876  0.1182   0.1337  0.0873   0.1298     0.2828

sigma^2 estimated as 0.1002:  log likelihood = -20.03,  aic = 60.05

arima(x = x, order = c(p3, 0, q3))
Coefficients:
         ar1     ar2      ma1      ma2      ma3      ma4     ma5     ma6  intercept
      0.8978  0.0616  -0.3472  -0.1037  -0.2258  -0.6355  0.2399  0.4394     9.0939
s.e.  0.2673  0.2660   0.2433   0.1790   0.1281   0.1376  0.1905  0.1449     0.2782

sigma^2 estimated as 0.1015:  log likelihood = -20.67,  aic = 61.34
