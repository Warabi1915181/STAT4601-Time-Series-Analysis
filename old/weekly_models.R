library(TSA)
library(ggplot2)

csv <- read.csv("BTC-USD.csv")
csv$time <- strptime(csv[,"timestamp"], "%Y-%m-%d")
data <- csv[,c("time", "adjclose")]

#daily data
log_data <- log(data$adjclose)
diff_log_data <- diff(log(data$adjclose))
diff2_log_data <- diff(diff(log(data$adjclose)))

plot(data$time, data$adjclose, type="l", main='Daily', xlab="date", ylab="adjclose")
plot(data$time, log_data, type="l", xlab="date", ylab="log_adjclose")
plot(data$time[2:2934], diff_log_data, type="l", xlab="date", ylab="diff_log_adjclose")
plot(data$time[3:2934], diff2_log_data, type="l", xlab="date", ylab="diff2_log_adjclose")

acf(diff_log_data, lag.max=60)
acf(diff2_log_data , lag.max=60)
pacf(diff_log_data, lag.max=60)
pacf(diff2_log_data , lag.max=60)

#weekly data
weeklydata <- data[5:2930,]
weeklydata$week <- rep(c(1:418), each=7)
weeklydata<-aggregate(weeklydata$adjclose, by=list(weeklydata$week), FUN=mean)
colnames(weeklydata) <- c("time","avg_adjclose")

w_log_data <- log(weeklydata$avg_adjclose)
w_diff_log_data <- diff(log(weeklydata$avg_adjclose))
w_diff2_log_data <- diff(diff(log(weeklydata$avg_adjclose)))

plot(weeklydata$time, weeklydata$avg_adjclose, type='l', main='Weekly', xlab="date", ylab="adjclose")
plot(weeklydata$time, w_log_data, type="l", main='Weekly', xlab="date", ylab="log_adjclose")
plot(weeklydata$time[2:418], w_diff_log_data, type="l", main='Weekly', xlab="date", ylab="diff_log_adjclose")
plot(weeklydata$time[3:418], w_diff2_log_data, type="l", main='Weekly', xlab="date", ylab="diff2_log_adjclose")

acf(w_diff_log_data, lag.max=60)
pacf(w_diff_log_data, lag.max=60)

weekly_fit1 <- arima(data$adjclose, c(0,1,1), method='ML')	
weekly_fit1
weekly_fit2 <- arima(data$adjclose, c(0,1,2), method='ML')	
weekly_fit2
weekly_fit3 <- arima(data$adjclose, c(1,1,0), method='ML')	
weekly_fit3
weekly_fit4 <- arima(data$adjclose, c(13,1,0), method='ML')	
weekly_fit4

plot_fit <- function(fit) {
  tsdiag(fit)
  par(mfrow=c(2,2))
  hist(fit$residuals)
  shapiro.test(fit$residuals)
  qqnorm(fit$residuals);qqline(fit$residuals)
  plot(fit$residuals)
  
  par(mfrow=c(2,1))
  acf(fit$residuals)
  pacf(fit$residuals)
}
plot_fit(weekly_fit4)

#Box.test(residuals(weekly_fit1), lag=21, type = "Ljung-Box", fitdf = 3)
