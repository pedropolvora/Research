date_price <- read.csv("dataa.csv", sep=",")
date_price[,1] <- as.Date(date_price[,1])
price <- ts(date_price[168:2373,-1])
plot(price, xlab="Date", type='l')


acf(log(price))
plot(acf(diff(log(price))), ylim= c(-0.2, 1))
pacf(log(price))
pacf(diff(log(price)), ylim= c(-0.2, 1))

summary(arima(diff(log(price)),order=c(0,0,0)))
summary(arima(diff(log(price)),order=c(1,0,0)))
summary(arima(diff(log(price)),order=c(0,0,1)))

summary(arima(diff(log(price)),order=c(1,0,1)))
arima101 <- arima(diff(log(price)),order=c(1,0,1))
##arima(log(price),order=c(1,1,1))

summary(arima(diff(log(price)),order=c(0,0,2)))
arima002 <- arima(diff(log(price)),order=c(0,0,2))
##arima(log(price),order=c(0,1,2))

summary(arima(diff(log(price)),order=c(1,0,2)))
summary(arima(diff(log(price)),order=c(2,0,0)))
summary(arima(diff(log(price)),order=c(2,0,1)))
summary(arima(diff(log(price)),order=c(2,0,2)))

## residuals
res.arima101 <- arima101$residuals
res.arima002 <- arima002$residuals
par(mfcol=c(3,1))
plot(res.arima101)
plot(acf(res.arima101, plot= FALSE), ylim= c(-0.1, 0.4))
pacf(res.arima101, ylim= c(-0.1, 0.4))

plot(res.arima002)
plot(acf(res.arima002, plot= FALSE), ylim= c(-0.1, 0.4))
pacf(res.arima002, ylim= c(-0.1, 0.4))

Box.test(res.arima101, type = "Ljung-Box")
Box.test(res.arima002, type = "Ljung-Box")

### squared residuals
res2.arima101 <- res.arima101^2
res2.arima002 <- res.arima002^2

par(mfcol=c(3,1))
plot(res2.arima101)
plot(acf(res2.arima101, lag.max = 100, plot= FALSE), ylim= c(-0.1, 1))
pacf(res2.arima101, lag.max = 100, ylim= c(-0.1, 1))

plot(res2.arima002)
plot(acf(res2.arima002, lag.max = 100, plot= FALSE), ylim = c(-0.1, 1))
pacf(res2.arima002, lag.max = 100, ylim = c(-0.1, 1))



####
summary(garch(res.arima101,order=c(1,1),trace=F))
summary(garch(res.arima002,order=c(1,1),trace=F))

summary(garch(res.arima101,order=c(1,2),trace=F))
summary(garch(res.arima101,order=c(2,1),trace=F))
summary(garch(res.arima101,order=c(2,2),trace=F))

summary(garch(res.arima101,order=c(0,1),trace=F))
summary(garch(res.arima101,order=c(0,2),trace=F))
summary(garch(res.arima101,order=c(0,3),trace=F))
summary(garch(res.arima101,order=c(0,4),trace=F))
summary(garch(res.arima101,order=c(0,5),trace=F))
summary(garch(res.arima101,order=c(0,6),trace=F))
summary(garch(res.arima101,order=c(0,7),trace=F))
summary(garch(res.arima101,order=c(0,8),trace=F))

summary(garch(res.arima101,order=c(1,4),trace=F)) ###


### forecast
###forecast.Arima(diff(log(price)))
garch11 = garch(diff(log(price)), order=c(1,1))
summary(garch11)
u = predict(garch11)
plot(diff(log(price))[1500:2205], type="l", xlab="Time", ylab="Returns")
lines(u[1500:2205,1], col="blue", lty="dashed")
lines(u[1500:2205,2], col="blue", lty="dashed")





