library(tseries)
library(MASS)
library(stats)

date_price <- read.csv("dataa.csv", sep=",")
price <- date_price[1:2373,2]
plot(price,type='l')

log_r <- diff(log(price))
plot(log_r, type='l')
acf(log_r)
hist(log_r)fitdistr(x, "lognormal")


ks.test(log_r, pnorm, mean(log_r), sd(log_r), alternative = "two.sided")
ks.test(log_r, plnorm, mean(log_r), sd(log_r), alternative = "two.sided")
ks.test(log_r, pt, df = 3, alternative = "two.sided")

fitdistr(log_r, "t") #1.2259...
ks.test(log_r, pt, df = 1, alternative = "two.sided")
ks.test(log_r, pt, df = 2, alternative = "two.sided")


2###test 1 - garch(1,1)
garch(log_r, order(1,1))


