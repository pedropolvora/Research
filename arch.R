library(tseries)
library(MASS)
library(stats)
library(fGarch)
library(astsa)
library(TSA)
library(forecast)

date_price <- read.csv("dataa.csv", sep=",")
#since 04/01/2011
price <- ts(date_price[168:2373,2], start=c(2011), frequency = 365)
plot(price,type='l')

log_r <- diff(log(price))
plot(log_r, type='l')
abline(0,0, col='green')

## price vs. returns
par(mfrow=c(2,1))
plot(price,type='l')
plot(log_r, type='l')
abline(0,0, col='green')
##
### FITTING AN ARCH MODEL - It can be fruitful to look at the ACF and PACF of both yt and yt2. 
### For instance, if yt appears to be WN and yt2 appears to be AR(1), then an ARCH(1) model for the variance is suggested.
### If the PACF of the yt2 suggests AR(m), then ARCH(m) may work. 
### GARCH models may be suggested by an ARMA type look to the ACF and PACF of yt2. 
### CHECK OF MODEL - Ljung&Box statistics of a' = at/sigma.t

## without ts()
a <- diff(log(date_price[168:2373,2]))
tsdisplay(a)

acf2(a) ## serial correlation - lags: 1 and 6
pacf(a^2) # strong lin. dependence (?) ## ARCH(7) alebo 3?
# with ACF - existence of the heteroscedasticity 

arch7.fit <- garch(a, order = c(0,6))
summary(arch7.fit)
# the estimates of alphas appear to be statistically nonsignificant at the 5% level. 
# Therefore, the model can be simplified.
arch3.fit <- garch(a, order = c(0,3))
summary(arch3.fit)
arch1.fit <- garch(a, order = c(0,1))
summary(arch1.fit)


