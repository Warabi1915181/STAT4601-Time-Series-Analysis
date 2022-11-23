# install.packages('TSA')
# install.packages('ggplot2')
# install.packages('zoo')
# install.packages('forecast')
# install.packages('dplyr')
library(ggplot2)
library(TSA)
library(zoo)
library(forecast)
library(dplyr)

data = read.csv('BTC-USD.csv')
data[,1] = as.Date(data[,1])

#ts <- ts(data$adjclose, start=c(2014, as.numeric(format(as.Date('2014-09-17'), '%j'))), frequency=365)/
ts <- zoo(log(data$adjclose), as.Date('2014-09-17') + 1:length(data$adjclose) - 1)


# model fitting
fit1 <- arima(ts, c(0,2,1), method='ML')	
fit1


fit2 <- arima(ts, c(0,2,6), method='ML')
fit2

fit3 <- arima(ts, c(0,2,34)) # beware this function takes some time to run
fit3

fit4 <- arima(ts, c(0,1,0), seasonal= list(order=c(0,1,1), period=30))
fit4

fit5 <- arima(ts, c(0,1,0), seasonal= list(order=c(0,1,1), period=7))
fit5


# model diagnostics
tsdiag(fit1)
hist(fit1$residuals)
qqnorm(fit1$residuals) 
qqline(fit1$residuals)
plot(fit1$residuals)
acf(fit1$residuals, lag.max=1000)
shapiro.test(fit1$residuals)
