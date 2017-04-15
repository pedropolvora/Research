bitfinex <- read.csv("bitfinexUSD.csv", header = FALSE, sep = ",")
head(bitfinex)
bitfinexTS <- bitfinex$V1
bitfinexDate <- rep(0,length(bitfinexTS))

for (i in 1: 10){
  bitfinexDate[i] <- as.Date(as.POSIXct(bitfinexTS[i], origin = "1970-01-01"))
}


bitfinexPrice <- bitfinex$V2
plot(bitfinexPrice[1:10000], type= 'l')
bitfinexReturn <- diff(log(bitfinexPrice))[-1]


val <- bitfinexTS[2]
as.POSIXct(val, origin="1970-01-01")
[1] "2012-11-04 22:32:00 CST"
c <- as.Date(as.POSIXct(val, origin="1970-01-01"))
[1] "2012-11-05" 