date_price <- read.csv("dataa.csv", sep=",")
price <- date_price[1:2373,2]
date <- as.Date(date_price[1:2373,1])
return <- rep(0,length(price))
return[1] <- 0
for (i in 2:length(price)) {
  return[i] <- ((price[i]-price[i-1])/price[i-1])
}

year <- as.factor(format(date,'%y'))
day <- as.factor(format(date,'%d'))
ay <- 10
j <- 1
k <- 1 
osx <- c()

r <- c()
d <- c()
ad <- 0

for (i in 1:length(date)){
  if(ad != day[i]){
    r[j] <- return[i]
    ad <- day[i]
    j <- j+1
  }
  if (ay != year[i]){
    d[k] <- j
    ay <- year[i]
    k <- k+1		
  }
}



par(mfrow=c(2,1)) 
plot( r, type="l", xaxt = "n", ylab="returns", xlab="")
axis(1, at = d, labels = c("2011","2012","2013","2014","2015","2016", "2017"))
plot(price, type="l", xaxt = "n", ylab="price", xlab="")
axis(1, at = d, labels = c("2011", "2012","2013","2014","2015","2016", "2017"))

plot(price)
