library(tseries)
library(MASS)
library(stats)
library(fGarch)

date_price <- read.csv("dataa.csv", sep=",")
#since 04/01/2011
price <- ts(date_price[168:2373,2], start=c(2011), frequency = 365)
plot(price,type='l')

log_r <- diff(log(price))
plot(log_r, type='l')
##
par(mfrow=c(2,1))
plot(price,type='l')
plot(log_r, type='l')
##