date_price <- read.csv("dataa.csv", sep=",")
price <- date_price[1:2373,2]
date <- as.Date(date_price[1:2373,1])
return <- rep(0,length(price))
return[1] <- 0
for (i in 2:length(price)) {
  return[i] <- ((price[i]-price[i-1])/price[i-1])
}

library(fGarch)
library(astsa)
acf2(return)

return2 <- diff(log(price))[-1]
acf2(return2)

#ARCH(1)
model1 <- garchFit(~garch(1,0), data=return2, trace=FALSE)
stand.res1 <- model1@residuals / model1@sigma.t
#stresi1 <- residuals(model1,standardize=T)
plot(stand.res1, type="l")
Box.test(stand.res1,10,type="Ljung")
acf2(stand.res1)
acf2(stand.res1^2)
summary(model1)

predict(model1,5)
### not good




#GARCH(1,1)
model2 <- garchFit(~garch(1,1), data=return2, trace=FALSE)
stand.res2 <- model2@residuals / model2@sigma.t
acf2(stand.res2)
acf2(stand.res2^2)
summary(model2)
