library(rugarch)

# Data import
date_price <- read.csv("dataa.csv", sep=",")
#since 01/01/2011 - 31/12/2016
price <- date_price[168:2359,2]
log_r <- ts(diff(log(price)), start=c(2011), frequency = 365)

############################################################################      
# The GARCH(1,1) model     
#########################################################################
spec = ugarchspec(# variance equation
  variance.model=list(model="sGARCH",garchOrder=c(1,1)),
  # mean equation
  mean.model=list(armaOrder=c(0,0),include.mean=F), 
  # assumed distribution of errors
  distribution.model="norm")

k.garch11 = ugarchfit( spec=spec, data=na.omit(log_r))
k.garch11

# Plot of conditional standard deviation estimates (3)
# and News-Impact curve (12).
plot(k.garch11)
############################################################################      
# The GARCH(1,2) model     
#########################################################################
spec = ugarchspec(# variance equation
  variance.model=list(model="sGARCH",garchOrder=c(1,2)),
  # mean equation
  mean.model=list(armaOrder=c(0,0),include.mean=F), 
  # assumed distribution of errors
  distribution.model="norm")

k.garch12 = ugarchfit( spec=spec, data=na.omit(log_r))
k.garch12
plot(k.garch12)

############################################################################      
# The EGARCH model     
#########################################################################
# Let's examine whether conditional variance reacts asymmetrically 
# to the news arriving to the market.
# Below estimation of the EGARCH(1,1) model.

# lets first define a model specification
spec = ugarchspec(
  variance.model=list(model="eGARCH",garchOrder=c(1,1)),
  mean.model=list(armaOrder=c(0,0),include.mean=F), 
  distribution.model="norm")

# function doesn't accept missing values
k.egarch11 = ugarchfit( spec=spec, data=na.omit(log_r))
k.egarch11
plot(k.egarch11)

##########################################################################                                   
# The TGARCH model
###################################################################
spec = ugarchspec(
  variance.model=list(model="fGARCH",garchOrder=c(1,1),
                      submodel="TGARCH"), #model="fGARCH" (family GARCH) together with submodel="TGARCH"
  mean.model=list(armaOrder=c(0,0),include.mean=F), 
  distribution.model="norm")

k.tgarch11 = ugarchfit( spec=spec, data=na.omit(log_r))
k.tgarch11
plot(k.tgarch11)

#### NIC
par(mfrow=c(3,1))
plot( k.garch11, which = 12)
plot(k.egarch11, which = 12)
plot( k.tgarch11, which = 12)
par(mfrow=c(1,1))


