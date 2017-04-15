#https://www.quantstart.com/articles/Generalised-Autoregressive-Conditional-Heteroskedasticity-GARCH-p-q-Models-for-Time-Series-Analysis
## 2011-2016
log_r <- ts(diff(log(date_price[168:2359,2])), start = c(2011), frequency = 365)
plot(log_r, ylab = "Log Returns")
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
# last 4years - 01/2013-12/2016
lr13 <- ts(diff(log(date_price[899:2359,2])), start = c(2013), frequency = 365)
plot(lr13, ylab = "Log Returns")
lr13 <- as.numeric(lr13)
lr13 <- lr13[!is.na(lr13)]

lr13final.aic <- Inf
lr13final.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
  lr13current.aic <- AIC(arima(lr13, order=c(p, d, q)))
  if (lr13current.aic < lr13final.aic) {
    lr13final.aic <- lr13current.aic
    lr13final.order <- c(p, d, q)
    lr13final.arima <- arima(lr13, order=lr13final.order)
  }
}


lr13final.order ## ARIMA(1,0,4)
plot(acf(resid(lr14final.arima)), ylim = c(-0.1,0.5), main = "ACF of Residuals of an ARIMA(3,0,1)")
plot(acf(resid(lr14final.arima)^2), ylim = c(-0.1,0.5), main = "ACF of Squared Residuals of an ARIMA(3,0,1)") ## arch effect
Box.test(resid(lr14final.arima)^2, type = "Ljung-Box")

lr14.garch <- garch(lr13, trace=F)
lr14.res <- lr14.garch$res[-1]
par(mfcol=c(2,1))
plot(acf(lr14.res, na.action = na.pass, plot = FALSE), ylim = c(-0.1,0.5), main = "ACF of the GARCH Residuals")
plot(acf(lr14.res^2, na.action = na.pass, plot = FALSE), ylim = c(-0.1,0.5), main = "ACF of the Squared GARCH Residuals")
par(mfcol=c(1,1))


# last 3years - 01/2014-12/2016
lr14 <- ts(diff(log(date_price[1264:2359,2])), start = c(2014), frequency = 365)
plot(lr14, ylab = "Log Returns")
lr14 <- as.numeric(lr14)
lr14 <- lr14[!is.na(lr14)]

lr14final.aic <- Inf
lr14final.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
  lr14current.aic <- AIC(arima(lr14, order=c(p, d, q)))
  if (lr14current.aic < lr14final.aic) {
    lr14final.aic <- lr14current.aic
    lr14final.order <- c(p, d, q)
    lr14final.arima <- arima(lr14, order=lr14final.order)
  }
}


lr14final.order ## ARIMA(1,0,4)
plot(acf(resid(lr14final.arima)), ylim = c(-0.1,0.5), main = "ACF of Residuals of an ARIMA(3,0,1)")
plot(acf(resid(lr14final.arima)^2), ylim = c(-0.1,0.5), main = "ACF of Squared Residuals of an ARIMA(3,0,1)") ## arch effect
Box.test(resid(lr14final.arima)^2, type = "Ljung-Box")

lr14.garch <- garch(lr14, trace=F)
lr14.res <- lr14.garch$res[-1]
par(mfcol=c(2,1))
plot(acf(lr14.res, na.action = na.pass, plot = FALSE), ylim = c(-0.1,0.5), main = "ACF of the GARCH Residuals")
plot(acf(lr14.res^2, na.action = na.pass, plot = FALSE), ylim = c(-0.1,0.5), main = "ACF of the Squared GARCH Residuals")
par(mfcol=c(1,1))



# last 2years - 01/2015-12/2016
lr15 <- ts(diff(log(date_price[1629:2359,2])), start = c(2015), frequency = 365)
plot(lr15, ylab = "Log Returns")
lr15 <- as.numeric(lr15)
lr15 <- lr15[!is.na(lr15)]

lr15final.aic <- Inf
lr15final.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
  lr15current.aic <- AIC(arima(lr15, order=c(p, d, q)))
  if (lr15current.aic < lr15final.aic) {
    lr15final.aic <- lr15current.aic
    lr15final.order <- c(p, d, q)
    lr15final.arima <- arima(lr15, order=lr15final.order)
  }
}


lr15final.order ## ARIMA(3,0,2)
plot(acf(resid(lr15final.arima)), ylim = c(-0.1,0.5), main = "ACF of Residuals of an ARIMA(3,0,1)")
plot(acf(resid(lr15final.arima)^2), ylim = c(-0.1,0.5), main = "ACF of Squared Residuals of an ARIMA(3,0,1)") ## arch effect
Box.test(resid(lr15final.arima)^2, type = "Ljung-Box")

lr15.garch <- garch(lr15, trace=F)
lr15.res <- lr15.garch$res[-1]
par(mfcol=c(2,1))
plot(acf(lr15.res, na.action = na.pass, plot = FALSE), ylim = c(-0.1,0.5), main = "ACF of the GARCH Residuals")
plot(acf(lr15.res^2, na.action = na.pass, plot = FALSE), ylim = c(-0.1,0.5), main = "ACF of the Squared GARCH Residuals")
par(mfcol=c(1,1))

gf15 <- garchFit(formula~garch(1,1), data=lr15, trace=FALSE)
summary(gf15)
plot(gf15)


p15 <- predict(gf15)
plot(p15[,3],type="l")











#############
#############
#############
gf11 <- garchFit(formula~garch(1,1), data=lr, trace=FALSE)
summary(gf11)
plot(gf11)
p11 <- predict(gf11)
plot(p11[,3],type="l")


plot(gf11@sigma.t[2000:2192], type="l")
lines(192:221, p11[,3], col="red")

###forecast
gfc11 <- garchFit(formula~garch(1,1), data=lr[1:2100], trace=FALSE)
summary(gfc11)
plot(gfc11)
pc11 <- predict.garch(gfc11)
plot(pc11[,3],type="l")

plot(gfc11@h.t, type="l")
lines(2101:2191, pc11[,3], col="red")



####
gf11 <- garchFit(formula~garch(1,1), data=lr, trace=FALSE, include.mean=F,
                 cond.dist= "norm")
summary(gf11)
#Plot of conditional variance estimates
par(mfrow=c(2,1))
plot(gf11@data, # @data = original data values
     type="l",col="red",ylab="r",main="Log-returns")
plot(gf11@h.t, # @h.t = conditional variance
     type="l",col="blue",
     ylab="cvar",main="Estimated Volatility")
par(mfrow=c(1,1))

# Do standardized residuals come from normal distribution?
stdres<-gf11@residuals/sqrt(gf11@h.t)

hist(stdres,breaks=20,prob=T,
     main="Histogram of standardized residuals \n from GARCH(1,1) for EUR/USD")
