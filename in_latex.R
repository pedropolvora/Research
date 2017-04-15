## all data 2011-2017
log_r <- ts(diff(log(date_price[168:2359,2])), start = c(2011), frequency = 365)
plot(log_r, ylab = "Log Returns")

mean(log_r)
stdev(log_r)
skewness(log_r)
kurtosis(log_r)

## without ts()
a <- log_r <- diff(log(date_price[168:2359,2]))
acf(a)
acf(a^2, main = "ACF of Squared Log Returns")

square_log_r <- a^2
Box.test(square_log_r, type = "Ljung-Box")

### garch(1,1)
g11 <- garch(log_r, order = c(1,1))
summary(g11)
plot(g11)


plot(log_r, type='l')
lines(g11$fitted.values[,1], type='l', col = "blue", lty = "dashed")
lines(g11$fitted.values[,2], type='l', col = "blue", lty = "dashed")


gf11 <- garchFit(formula~garch(1,1), data=log_r)
summary(gf11)
plot(gf11)
predict(gf11)
