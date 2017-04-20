###  RISK METRICS ### later
gf11t <- garchFit(formula~garch(1,1), data=lr, trace=FALSE, include.mean = T)
summary(gf11t)
plot(gf11t)

####
gf11f <- garchFit(formula~garch(1,1), data=lr, trace=FALSE, include.mean=F)
summary(gf11f)
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
