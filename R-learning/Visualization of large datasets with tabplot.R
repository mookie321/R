## https://cran.r-project.org/web/packages/tabplot/vignettes/tabplot-vignette.html
##**load ggplot2 package and diamonds dataset**//
install.packages("tabplot")
library(ggplot2)
library(tabplot)

data("diamonds")

##input na into price when the cut is=="ideal"//
is.na(diamonds$price) <- diamonds$cut =="Ideal"

##input na into cut randomly??
is.na(diamonds$cut) <- (runif(nrow(diamonds)) > 0.8)

##create a table plot of diamonds
tableplot(diamonds)
##create a tableplot of diamonds selecting carat,price,cut,color ,and clarit
tableplot(diamonds, select = c(carat, price, cut, color, clarity),sortCol=price)

##zooming:
##focus on the 5% most expensive diamonds 
tableplot(diamonds, select = c(carat, price, cut, color, clarity), sortCol = price, 
          from = 0, to = 5)

##filtering 
##table shows the data of premium cut diamonds that cost less than 5000$
tableplot(diamonds, subset = price < 5000 & cut == "Premium")
