# install.packages('TSA')
# install.packages('ggplot2')
# install.packages('latex2exp')
library(ggplot2)
library(TSA)
library(latex2exp)

data <- read.csv('MER_T11_06.csv')
levels(factor(data$Description))
data <- data[data$Description=='Total Energy Electric Power Sector CO2 Emissions',]
rownames(data) = 1:dim(data)[1]
data = data[,c(2,3)]
head(data)

data$Value = as.numeric(data$Value)
data$YYYYMM = as.character(data$YYYYMM)
data$YYYYMM = as.Date(paste0(data$YYYYMM, '01'), format='%Y%m%d')
data = na.omit(data)
colnames(data) = c('time', 'value')
head(data)
n = dim(data)[1]

# time plot
ggplot(data) +
  geom_line(aes(time, value)) +
  ylab('value')

# take log
data$log.value = log(data$value)
ggplot(data) +
  geom_line(aes(time, log.value)) +
  ylab('log value')

# Sample ACF and PACF
acf(data$log.value, ci.type='ma', lag.max = 60)
pacf(data$log.value)

# take difference
diff1.value = diff(data$log.value)
ggplot() +
  geom_line(aes(x=data[2:n,1], y=diff1.value)) +
  ylab(TeX('$\\nabla$value'))
acf(diff1.value, ci.type='ma',lag.max=60)
pacf(diff1.value)

# take difference twice
diff2.value = diff(diff1.value)
ggplot() +
  geom_line(aes(x=data[3:n,1], y=diff2.value)) +
  ylab(TeX('$\\nabla^2$value'))
acf(diff2.value, ci.type='ma', lag.max=60)
pacf(diff2.value)

# trend, seasonal, residuals
ts.series = ts(data$value, frequency=12, start=c(1973, 1))
ts.stl = stl(ts.series, s.window='periodic')
plot(ts.stl)


# seasonal difference with period=12
diffs.value = diff(data$log.value, lag = 12)
ggplot() +
  geom_line(aes(x=data[13:n,1], y=diffs.value)) +
  ylab(TeX('$\\nabla_s\\nabla$value'))
acf(diffs.value, ci.type='ma', lag.max=60)
pacf(diffs.value, lag.max=60)

# take difference once, and followed by seasonal difference with period=12
diffs2.value = diff(diff1.value, lag = 12)
ggplot() +
  geom_line(aes(x=data[14:n,1], y=diffs2.value)) +
  ylab(TeX('$\\nabla\\nabla_s$value'))
acf(diffs2.value, ci.type='ma', lag.max=60)
pacf(diffs2.value, lag.max=60)



# model fitting
#################################
#################################
#################################
fit <- arima(ts.series, c(0,1,0), seasonal= list(order=c(1,0,0), period=12))
fit
res <- resid(fit)
acf(res, ci.type='ma', lag.max=60)
#################################
fit <- arima(ts.series, c(0,1,2), seasonal= list(order=c(1,0,0), period=12))
fit
res <- resid(fit)
acf(res, ci.type='ma', lag.max=60)

tsdiag(fit)
hist(fit$residuals)
qqnorm(fit$residuals) 
qqline(fit$residuals)

plot(fit$residuals)
shapiro.test(fit$residuals)
LB.test(fit, lag=10, type="Ljung-Box")


#################################
#################################
#################################
fit <- arima(ts.series, c(0,0,1), seasonal= list(order=c(1,1,0), period=12))
fit
res <- resid(fit)
acf(res, ci.type='ma')
pacf(res)


#################################
fit <- arima(ts.series, c(1,0,1), seasonal= list(order=c(1,1,0), period=12))
fit
res <- resid(fit)
acf(res, ci.type='ma')
pacf(res, lag.max=60)


res.stl <- stl(res1, s.window='periodic')
plot(res.stl)


tsdiag(fit)
hist(fit$residuals)
qqnorm(fit$residuals) 
qqline(fit$residuals)
plot(fit$residuals)
shapiro.test(fit$residuals)
LB.test(fit, lag=10, type="Ljung-Box")



#################################
#################################
#################################
fit1 <- arima(ts.series, c(0,1,1), seasonal= list(order=c(0,1,1), period=12))
fit1
res1 <- resid(fit1)
acf(res1, ci.type='ma')
pacf(res1)

res1.stl <- stl(res1, s.window='periodic')
plot(res1.stl)


tsdiag(fit1)
hist(fit1$residuals)
qqnorm(fit1$residuals) 
qqline(fit1$residuals)
plot(fit1$residuals)
shapiro.test(fit1$residuals)
LB.test(fit1, lag=10, type="Ljung-Box")
############################
fit2 <- arima(ts.series, c(0,1,2), seasonal= list(order=c(0,1,1), period=12))
fit2
res2 <- resid(fit2)
acf(res2, ci.type='ma')
pacf(res2)

res2.stl <- stl(res2, s.window='periodic')
plot(res2.stl)


tsdiag(fit2)
hist(fit2$residuals)
qqnorm(fit2$residuals) 
qqline(fit2$residuals)
plot(fit2$residuals)
shapiro.test(fit2$residuals)
LB.test(fit2, lag=10, type="Ljung-Box")


################################# 
# (BEST)
fit <- arima(ts.series, c(1,1,1), seasonal= list(order=c(0,1,1), period=12))
fit
res <- resid(fit)
acf(res, ci.type='ma')
pacf(res)

res.stl <- stl(res1, s.window='periodic')
plot(res.stl)


tsdiag(fit)
hist(fit$residuals)
qqnorm(fit$residuals) 
qqline(fit$residuals)
plot(fit$residuals)
shapiro.test(fit$residuals)
LB.test(fit, lag=10, type="Ljung-Box")
