require(rmgarch)
require(PerformanceAnalytics)
require(zoo)
library(xts)
library(forecast)
library(TSA)

date_price <- read.csv("dataa.csv", sep=",")
date_price[,1] <- as.Date(date_price[,1])
### 2013-2017
price2013 <- zoo(date_price[899:2373,-1], order.by = date_price[899:2373,1])
plot(price2013, xlab="Date")

plot(apply.monthly(window(price2013, start="2013-01-01"), 
                   function(x) sd(x, na.rm=TRUE)), 
     main="Monthly Standard Deviation of series", 
     ylab="Standard Deviation", xlab="Date")

plot(apply.weekly(window(price2013, start="2013-01-01"), 
                  function(x) sd(x, na.rm=TRUE)), 
     main="Daily Standard Deviation of series", 
     ylab="Standard Deviation", xlab="Date")


price2013 <- window(price2013, start="2013-01-01")
price2013 <- na.approx(na.trim(CalculateReturns(price2013), side="both"))
# start with default GARCH spec.
spec = ugarchspec()
print(spec)
def.fit = ugarchfit(spec = spec, data = price2013)
print(def.fit)
# ARCH LM test; null: no ARCH effect
### chance for serial correlation????
# sign bias test; null: no significiant negative and positive reaction shocks 

# Lets chnage GARCH Specs.. Lets use GARCH(1,1)..
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,1), 
                                                model = "sGARCH"), distribution.model = "norm")

# Fit the model
garch.fit = ugarchfit(garch11.spec, data = price2013, fit.control=list(scale=TRUE))
print(garch.fit)

plot(garch.fit, which=3)

# Lets chnage GARCH Specs.. Lets use GARCH(1,2)..
garch12.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,2), 
                                                model = "sGARCH"), distribution.model = "norm")

# Fit the model
garch.fit = ugarchfit(garch12.spec, data = price2013, fit.control=list(scale=TRUE))
print(garch.fit)

plot(garch.fit, which=3) # better

# Lets chnage GARCH Specs.. Lets use GARCH(2,2)..
garch22.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(2,2), 
                                                model = "sGARCH"), distribution.model = "norm")

# Fit the model
garch.fit = ugarchfit(garch22.spec, data = price2013, fit.control=list(scale=TRUE))
print(garch.fit)

plot(garch.fit, which=3)
## forc ???????
garch.fit.100 = ugarchfit(garch12.spec, data = price2013, fit.control=list(scale=TRUE), out.sample = 50) ## subsample
forc.100 = ugarchforecast(garch.fit.100, n.ahead=50, n.roll = 50)
fpm(forc.100) # fpm (forecast performance measure function)
forc.100
plot(forc.100, which="all")

#############################
#############################
#############################
#############################
### 2014-2017
price2014 <- zoo(date_price[1264:2373,-1], order.by = date_price[1264:2373,1])
plot(price2014, xlab="Date")

plot(apply.monthly(window(price2014, start="2014-01-01"), 
                   function(x) sd(x, na.rm=TRUE)), 
     main="Monthly Standard Deviation of series", 
     ylab="Standard Deviation", xlab="Date")

plot(apply.weekly(window(price2014, start="2014-01-01"), 
                  function(x) sd(x, na.rm=TRUE)), 
     main="Daily Standard Deviation of series", 
     ylab="Standard Deviation", xlab="Date")

price2014 <- window(price2014, start="2014-01-01")
price2014 <- na.approx(na.trim(CalculateReturns(price2014), side="both"))
# start with default GARCH spec.
spec = ugarchspec()
print(spec)
def.fit = ugarchfit(spec = spec, data = price2014)
print(def.fit)
# ARCH LM test; null: no ARCH effect
### chance for serial correlation????
# sign bias test; null: no significiant negative and positive reaction shocks 

# Lets chnage GARCH Specs.. Lets use GARCH(1,1)..
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,1), 
                                                model = "sGARCH"), distribution.model = "norm")

# Fit the model
garch.fit = ugarchfit(garch11.spec, data = price2014, fit.control=list(scale=TRUE))
print(garch.fit)

plot(garch.fit, which=3)

# Lets chnage GARCH Specs.. Lets use GARCH(1,2)..
garch12.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,2), 
                                                model = "sGARCH"), distribution.model = "norm")

# Fit the model
garch.fit = ugarchfit(garch12.spec, data = price2014, fit.control=list(scale=TRUE))
print(garch.fit)

plot(garch.fit, which=3) # better

# Lets chnage GARCH Specs.. Lets use GARCH(2,2)..
garch22.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(2,2), 
                                                model = "sGARCH"), distribution.model = "norm")

# Fit the model
garch.fit = ugarchfit(garch22.spec, data = price2014, fit.control=list(scale=TRUE))
print(garch.fit)

plot(garch.fit, which=3)

#############################
#############################
#############################
#############################
### 2015-2017
price2015 <- zoo(date_price[1629:2373,-1], order.by = date_price[1629:2373,1])
plot(price2015, xlab="Date")

plot(apply.monthly(window(price2015, start="2015-01-01"), 
                   function(x) sd(x, na.rm=TRUE)), 
     main="Monthly Standard Deviation of series", 
     ylab="Standard Deviation", xlab="Date")

plot(apply.weekly(window(price2015, start="2015-01-01"), 
                  function(x) sd(x, na.rm=TRUE)), 
     main="Daily Standard Deviation of series", 
     ylab="Standard Deviation", xlab="Date")

price2015 <- window(price2015, start="2015-01-01")
price2015 <- na.approx(na.trim(CalculateReturns(price2015), side="both"))
# start with default GARCH spec.
spec = ugarchspec()
print(spec)
def.fit = ugarchfit(spec = spec, data = price2015)
print(def.fit)
# ARCH LM test; null: no ARCH effect
### chance for serial correlation????
# sign bias test; null: no significiant negative and positive reaction shocks 

# Lets chnage GARCH Specs.. Lets use GARCH(1,1)..
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,1), 
                                                model = "sGARCH"), distribution.model = "norm")

# Fit the model
garch.fit = ugarchfit(garch11.spec, data = price2015, fit.control=list(scale=TRUE))
print(garch.fit)

plot(garch.fit, which=3)

# Lets chnage GARCH Specs.. Lets use GARCH(1,2)..
garch12.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(1,2), 
                                                model = "sGARCH"), distribution.model = "norm")

# Fit the model
garch.fit = ugarchfit(garch12.spec, data = price2015, fit.control=list(scale=TRUE))
print(garch.fit)

plot(garch.fit, which=3) # better

# Lets chnage GARCH Specs.. Lets use GARCH(2,2)..
garch22.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(garchOrder = c(2,2), 
                                                model = "sGARCH"), distribution.model = "norm")

# Fit the model
garch.fit = ugarchfit(garch22.spec, data = price2015, fit.control=list(scale=TRUE))
print(garch.fit)

plot(garch.fit, which=3)

## forc ???????
garch.fit.100 = ugarchfit(garch12.spec, data = price2015, fit.control=list(scale=TRUE), out.sample = 10) ## subsample
forc.100 = ugarchforecast(garch.fit.100, n.ahead=50, n.roll = 10)
fpm(forc.100) # fpm (forecast performance measure function)
forc.100
plot(forc.100, which="all")


