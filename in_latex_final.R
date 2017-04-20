#https://www.quantstart.com/articles/Generalised-Autoregressive-Conditional-Heteroskedasticity-GARCH-p-q-Models-for-Time-Series-Analysis
library(tseries)
library(MASS)
library(stats)
library(fGarch)
library(astsa)
library(TSA)
library(forecast)

## 2011-2016
date_price <- read.csv("dataa.csv", sep=",")
log_r <- ts(diff(log(date_price[168:2359,2])), start = c(2011), frequency = 365)

## price vs. returns
par(mfrow=c(2,1))
plot(price,type='l') ### ts()...
plot(log_r, type='l')
#abline(0,0, col='green')


mean(log_r)
sd(log_r)
skewness(log_r)
kurtosis(log_r)

## without ts()
a <- diff(log(date_price[168:2359,2]))
par(mfcol=c(2,1))
plot(log_r, ylab = "Log Returns", main='Daily Log Returns of Bitcoin')
acf((a-mean(a))^2, main = "ACF of Squared Mean-Adjusted Log Returns")
par(mfcol=c(1,1))

lr <- as.numeric(log_r)
lr <- lr[!is.na(lr)]

lrfinal.aic <- Inf
lrfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
  lrcurrent.aic <- AIC(arima(lr, order=c(p, d, q)))
    if (lrcurrent.aic < lrfinal.aic) {
      lrfinal.aic <- lrcurrent.aic
      lrfinal.order <- c(p, d, q)
      lrfinal.arima <- arima(lr, order=lrfinal.order)
      }
}


lrfinal.order ## ARIMA(3,0,1)
plot(acf(resid(lrfinal.arima)), ylim = c(-0.1,0.5), main = "ACF of Residuals of an ARIMA(3,0,1)")
plot(acf(resid(lrfinal.arima)^2), ylim = c(-0.1,0.5), main = "ACF of Squared Residuals of an ARIMA(3,0,1)") ## arch effect
Box.test(resid(lrfinal.arima)^2, type = "Ljung-Box")

lr.garch <- garch(lr, trace=F)
lr.res <- lr.garch$res[-1]
par(mfcol=c(2,1))
plot(acf(lr.res, na.action = na.pass, plot = FALSE), ylim = c(-0.1,0.5), main = "ACF of the GARCH Residuals")
plot(acf(lr.res^2, na.action = na.pass, plot = FALSE), ylim = c(-0.1,0.5), main = "ACF of the Squared GARCH Residuals")
par(mfcol=c(1,1))
#############
#############
#############
#############
#############
############# 






