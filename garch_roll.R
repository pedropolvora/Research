library(tseries)
library(zoo)
library(forecast)
library(FinTS)
library(rugarch)

date_price <- read.csv("dataa.csv", sep=",")
date_price[,1] <- as.Date(date_price[,1])
price.zoo <- zoo(date_price[168:2373,-1], order.by = date_price[168:2373,1])
plot(price.zoo, xlab="Date")

log_r.zoo <- diff(log(price.zoo))*100
plot(log_r.zoo)

fit1 <- auto.arima(log_r.zoo, trace=TRUE, test="kpss", ic="bic")
fit1
Box.test(fit1$residuals^2, lag=12, type="Ljung-Box") ##no ARCH effect


res_garch1_spec <- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(0,1)))
res_garch1_fit <- ugarchfit(spec = res_garch1_spec, data = log_r.zoo)
res_garch1_fit 

ctrl = list(tol = 1e-7, delta = 1e-9)
res_garch1_roll <- ugarchroll(res_garch1_spec, log_r.zoo, n.start = 120, refit.every = 1,
                              refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                              VaR.alpha = 0.05, keep.coef = TRUE, solver.control = ctrl,
                              fit.control = list(scale = 1))

report(res_garch1_roll, type = "VaR", VaR.alpha = 0.05, conf.level = 0.95)
plot(res_garch1_fit)

res_garch1_forc <- ugarchforecast(res_garch1_fit, n.ahead = 12)
res_garch1_forc

#### alpha = 0.01
res_garch1_roll99 <- ugarchroll(res_garch1_spec, log_r.zoo, n.start = 120, refit.every = 1,
                              refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                              VaR.alpha = 0.01, keep.coef = TRUE, solver.control = ctrl,
                              fit.control = list(scale = 1))

report(res_garch1_roll99, type = "VaR", VaR.alpha = 0.01, conf.level = 0.99)
plot(res_garch1_fit)

res_garch1_forc <- ugarchforecast(res_garch1_fit, n.ahead = 12)
res_garch1_forc

