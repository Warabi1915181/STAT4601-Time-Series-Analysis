library(dplyr)
library(TSA)
library(ggplot2)

btc<-read.csv("BTC-USD.csv")
btc[,1]<-as.Date(btc[,1])
plot(btc$timestamp, btc$adjclose, xlab="Time", ylab="adjclose", type='l')

btc$logclose<-log(btc$adjclose)
plot(btc$timestamp, btc$logclose, xlab="Time", ylab="log(adjclose)", type='l')

diff1<-diff(btc$logclose)
plot(btc$timestamp[1:2933], diff1, xlab="Time", ylab="diff(log(adjclose))", type='l')

diff2<-diff(diff1)
plot(btc$timestamp[1:2932], diff2, xlab="Time", ylab="diff(diff(log(adjclose)))", type='l')

#Weekly data
btc$week<-strftime(btc$timestamp, format="%u")
weeklybtc<-btc[btc$week==1,]
plot(weeklybtc$timestamp, weeklybtc$adjclose, main="Weekly Data", xlab="Time", ylab="adjclose", type='l')
plot(weeklybtc$timestamp, weeklybtc$logclose, main="Weekly Data", xlab="Time", ylab="log(adjclose)", type='l')
weeklydiff1<-diff(weeklybtc$logclose)
plot(weeklybtc$timestamp[1:418], weeklydiff1, main="Weekly Data", xlab="Time", ylab="diff(log(adjclose))", type='l')
weeklydiff2<-diff(weeklydiff1)
plot(weeklybtc$timestamp[1:417], weeklydiff2, main="Weekly Data", xlab="Time", ylab="diff(diff(log(adjclose)))", type='l')

#Monthly data
btc$month<-strftime(btc$timestamp, format="%Y-%m")
monthlybtc<-aggregate(btc, by=list(btc$month), FUN=last)
plot(monthlybtc$timestamp, monthlybtc$adjclose, main="Monthly Data", xlab="Time", ylab="adjclose", type='l')
plot(monthlybtc$timestamp, monthlybtc$logclose, main="Monthly Data", xlab="Time", ylab="log(adjclose)", type='l')
monthlydiff1<-diff(monthlybtc$logclose)
plot(monthlybtc$timestamp[1:96], monthlydiff1, main="Monthly Data", xlab="Time", ylab="diff(log(adjclose))", type='l')
monthlydiff2<-diff(monthlydiff1)
plot(monthlybtc$timestamp[1:95], monthlydiff2, main="Monthly Data", xlab="Time", ylab="diff(diff(log(adjclose)))", type='l')
