library(e1071)

date_price <- read.csv("dataa.csv", sep=",")
#since 01/01/2011
price <- date_price[168:2373,2]
plot(price,type='l')

log_r <- ts(diff(log(price)), start=c(2011), frequency = 365)
plot(log_r, type='l')
abline(0,0, col='green')

## pretesting
FinTS.stats(log_r)
jarque.bera.test(log_r)
hist(log_r)

## unit-root test
adf.test(log_r)
ArchTest(log_r, 1) # no ARCH effect
ArchTest(log_r, 5) # no ARCH effect
ArchTest(log_r, 10) # no ARCH effect

## fitting the model
a11 <- garchFit(formula~garch(1,1), data=log_r)
summary(a11)
###       AIC       BIC       SIC      HQIC 
### -3.521837 -3.511500 -3.521844 -3.518060 

a12 <- garchFit(formula~garch(1,2), data=log_r) ## best one GARCCH(1,2)
summary(a12)
###       AIC       BIC       SIC      HQIC 
### -3.527096 -3.514174 -3.527106 -3.522375

a21 <- garchFit(formula~garch(2,1), data=log_r)
summary(a21)
###       AIC       BIC       SIC      HQIC 
### -3.521485 -3.508564 -3.521496 -3.516764 

a22 <- garchFit(formula~garch(2,2), data=log_r)
summary(a22)
###       AIC       BIC       SIC      HQIC 
### -3.526189 -3.510683 -3.526204 -3.520523 


plot(a12)
predict(a12)
