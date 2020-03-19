library(plyr)
library(tidyverse)

# http://seankross.com/2016/02/29/A-Q-Q-Plot-Dissection-Kit.html
#https://stackoverflow.com/questions/2564258/plot-two-graphs-in-same-plot-in-r

par(mfrow = c(1, 2))

#distribution="norm",
qnorm.quantile <- function(x, ndx) {
  return(qnorm((1.0/(length(x)+1)*ndx))) # distribution="norm"))
}

x <- c(-15, 1, 5, 4, 2, 9, 0, -3.28,  -3, 4, 7, 12, 8, -1.5, 2.8, -0.5, 19.33)
x <- c(7.19, 6.31, 5.89, 4.5, 3.77, 4.25, 5.19, 4.5, 5.79, 6.79)
#x

#x <- c(1.71, 1.78, 2.36, 1.95, 2.59, 1.76, 2.05, 1.52, 1.87, 1.89)

x <- sort(x)

x

qqnorm(x)
qqline(x)


#h <- hist(x,ylim=c(0,5), breaks=20)



#text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))


summaryStats <- summary(x)
summaryStats


#qnorm((1.0/length(x))*1)
y <- x
for (ndx in 1:length(x)){
  y[ndx] <- qnorm.quantile(x, ndx)
  print(paste( x[ndx], "  z =  " , qnorm.quantile(x, ndx)))
}


plot(y, x,,type="l",col="red")


#lines(x,y2,col="green")


####################################################

par(mfrow = c(1, 2))

# normal_density are the y-values for the normal curve
# zs are the x-values for the normal curve
n <- 1000
normal_density <- dnorm(seq(-4, 4, 0.01))
zs <- seq(-4, 4, 0.01)

# Add some spice to the default histogram function
hist_ <- function(x, ...){
  hist(x, breaks = 30, xlab = "Z", ylab = "",  yaxt='n', freq = FALSE, ...)
  lines(zs, normal_density, type = "l", col = "red", lwd = 2)
}

# Gaussian Normal
# rnorm() generates random numbers from a normal distribution
# gaussian_rv is the dataset that will be compared to the Gaussian distribution
gaussian_rv <- rnorm(n)

# Draw the histogram
hist_(gaussian_rv, main = "Gaussian Distribution")

# Draw the Q-Q plot
qqnorm(gaussian_rv)
qqline(gaussian_rv, col = "blue", lwd = 2)