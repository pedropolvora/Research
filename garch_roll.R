https://www.youtube.com/watch?v=wsYXKh_xmSs&t=247s
library(tseries)
library(zoo)
library(forecast)
library(FinTS)
library(rugarch)
### 2011-01-01 
date_price <- read.csv("dataa.csv", sep=",")
date_price[,1] <- as.Date(date_price[,1])
price.zoo <- zoo(date_price[168:2373,-1], order.by = date_price[168:2373,1])
plot(price.zoo, xlab="Date")

log_r.zoo <- diff(log(price.zoo))*100
plot(log_r.zoo)

fit1 <- auto.arima(log_r.zoo, trace=TRUE, ic="aic")
fit1
Box.test(fit1$residuals^2, lag=1, type="Ljung-Box") ##no ARCH effect


res_garch1_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(1,3)))
res_garch1_fit <- ugarchfit(spec = res_garch1_spec, data = log_r.zoo)
res_garch1_fit 

ctrl = list(tol = 1e-7, delta = 1e-9)
res_garch1_roll <- ugarchroll(res_garch1_spec, log_r.zoo, n.start = 120, refit.every = 1,
                              refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                              VaR.alpha = 0.05, keep.coef = TRUE, solver.control = ctrl,
                              fit.control = list(scale = 1))
### alpha = 0.05 - OK
report(res_garch1_roll, type = "VaR", VaR.alpha = 0.05, conf.level = 0.95)
plot(res_garch1_fit)

res_garch1_forc <- ugarchforecast(res_garch1_fit, n.ahead = 12)
res_garch1_forc

#### alpha = 0.01 - NOT OK
res_garch1_roll99 <- ugarchroll(res_garch1_spec, log_r.zoo, n.start = 120, refit.every = 1,
                              refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                              VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl,
                              fit.control = list(scale = 1))

report(res_garch1_roll99, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
plot(res_garch1_fit)

########################
########################
########################
########################
### 2014-2017 # cele zle...
price2014 <- zoo(date_price[1264:2373,-1], order.by = date_price[1264:2373,1])
plot(price2014, xlab="Date")

log_r2014 <- diff(log(price2014))
plot(log_r2014)

fit2014 <- auto.arima(log_r2014, trace=TRUE, test="kpss", ic="aic") ## ARIMA(5,1,0)
fit2014
Box.test(fit2014$residuals^2, lag=12, type="Ljung-Box") ## no ARCH effect

res_garch1_spec2014 <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                                  mean.model = list(armaOrder = c(5,0)))
res_garch1_fit2014 <- ugarchfit(spec = res_garch1_spec2014, data = diff(log_r2014,1)) ## diff because ARIMA(5,1,0)
res_garch1_fit2014

ctrl = list(tol = 1e-7, delta = 1e-9)
res_garch1_roll2014 <- ugarchroll(res_garch1_spec2014, log_r2014, n.start = 120, refit.every = 1,
                              refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                              VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl,
                              fit.control = list(scale = 1))
report(res_garch1_roll2014, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
plot(res_garch1_fit2014)

########################
########################
########################
########################
### 2013-2017
price2013 <- zoo(date_price[899:2373,-1], order.by = date_price[899:2373,1])
plot(price2013, xlab="Date")

log_r2013 <- diff(log(price2013))
plot(log_r2013)

fit2013 <- auto.arima(log_r2013, trace=TRUE, test="kpss", ic="aic") ## ARIMA(5,1,0)
fit2013
Box.test(fit2013$residuals^2, lag=12, type="Ljung-Box") ## no ARCH effect

res_garch1_spec2013 <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                                  mean.model = list(armaOrder = c(5,0)))
res_garch1_fit2013 <- ugarchfit(spec = res_garch1_spec2013, data = diff(log_r2013,1)) ## diff because ARIMA(5,1,0)
res_garch1_fit2013

ctrl = list(tol = 1e-7, delta = 1e-9)
res_garch1_roll <- ugarchroll(res_garch1_spec2013, log_r.zoo, n.start = 120, refit.every = 1,
                              refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                              VaR.alpha = 0.05, keep.coef = TRUE, solver.control = ctrl,
                              fit.control = list(scale = 1))



