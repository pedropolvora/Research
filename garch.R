### http://yunus.hacettepe.edu.tr/~iozkan/eco665/archgarch.html
require(rmgarch)
require(PerformanceAnalytics)
require(zoo)
library(xts)
library(forecast)
library(TSA)


date_price <- read.csv("dataa.csv", sep=",")
date_price[,1] <- as.Date(date_price[,1])
price.zoo <- zoo(date_price[168:2373,-1], order.by = date_price[168:2373,1])
plot(price.zoo, xlab="Date")

to.yearly(price.zoo)
tail(to.monthly(price.zoo))
start(price.zoo)
end(price.zoo)

plot(apply.monthly(window(price.zoo, start="2011-01-01"), 
                   function(x) sd(x, na.rm=TRUE)), 
     main="Monthly Standard Deviation of series", 
     ylab="Standard Deviation", xlab="Date")

plot(apply.weekly(window(price.zoo, start="2011-01-01"), 
                   function(x) sd(x, na.rm=TRUE)), 
     main="Daily Standard Deviation of series", 
     ylab="Standard Deviation", xlab="Date")



price1 <- window(price.zoo, start="2011-01-01")
price1 <- na.approx(na.trim(CalculateReturns(price1), side="both"))
# start with default GARCH spec.
spec = ugarchspec()
print(spec)
def.fit = ugarchfit(spec = spec, data = price1)
print(def.fit)
# ARCH LM test; null: no ARCH effect
### chance for serial correlation????
# sign bias test; null: no significiant negative and positive reaction shocks 

# Lets chnage GARCH Specs.. Lets use GARCH(1,1)..
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,1), 
                                                model = "sGARCH"), distribution.model = "norm")

# Fit the model
garch.fit = ugarchfit(garch11.spec, data = price1, fit.control=list(scale=TRUE))
print(garch.fit)

plot(garch.fit, which=3)




### forecast ## all wrong...
garch.fit.100 = ugarchfit(garch11.spec, data = price1, fit.control=list(scale=TRUE), out.sample = 100) ## subsample
forc.100 = ugarchforecast(garch.fit.100, n.ahead=100, n.roll = 100)
fpm(forc.100) # fpm (forecast performance measure function)
forc.100
plot(forc.100, which="all")
