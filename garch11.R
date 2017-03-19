library(tseries)
library(MASS)
library(stats)
library(fGarch)

date_price <- read.csv("dataa.csv", sep=",")
price <- date_price[1:2373,2]
plot(price,type='l')

log_r <- diff(log(price))
plot(log_r, type='l')
acf(log_r)

hist(log_r, breaks = 20)
osx<-seq(min(log_r),max(log_r), 0.01)
lines(osx, dnorm(osx, mean(log_r),sd(log_r)),col='red')
      

ks.test(log_r, pnorm, mean(log_r), sd(log_r), alternative = "two.sided")
ks.test(log_r, plnorm, mean(log_r), sd(log_r), alternative = "two.sided")
ks.test(log_r, pt, df = 3, alternative = "two.sided")

fitdistr(log_r, "t") #1.2259...
ks.test(log_r, pt, df = 1, alternative = "two.sided")
ks.test(log_r, pt, df = 2, alternative = "two.sided")


  ###test 1 - garch(1,1) ####
garch11 <- garch(log_r)
summary(garch11)  # ARCH effects are filtered. However, 
plot(garch11)     # conditional normality seems to be violated


  #### prediction ###
g = garchFit(~garch(1,1), log_r, cond.dist= "norm", include.mean=FALSE, trace=FALSE)  
omega = g@fit$matcoef[1,1]
alpha = g@fit$matcoef[2,1]
beta = g@fit$matcoef[3,1]

alpha+beta ### tesne>1 !!!!

sigma = omega + alpha * log_r[2372]^2 + beta*g@h.t[2372]   #compute sigma^2 for t+1
print(sigma)


##### testin prediction with 2000 data ###
g2 = garchFit(~garch(1,1), log_r[1:2000], cond.dist= "norm", include.mean=FALSE, trace=FALSE)  
omega2 = g2@fit$matcoef[1,1]
alpha2 = g2@fit$matcoef[2,1]
beta2 = g2@fit$matcoef[3,1]

alpha2+beta2 ###tesne >1 !!!!

sigma2 = omega2 + alpha2 * log_r[2000]^2 + beta2*g@h.t[2000]   #compute sigma^2 for t+1
print(sigma2)
plot(g2)



