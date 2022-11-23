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
fit <- arima(ts.series, c(12,1,0))
fit
res <- resid(fit)
acf(res, ci.type='ma', lag.max=60)  # some minor significant ACFs at lag 2-3. We try adding MA components
#################################
fit <- arima(ts.series, c(12,1,3))
fit
res <- resid(fit)
acf(res, ci.type='ma', lag.max=60) # the significant ACFs at lag2-3 are gone. The coefficients from AR1 to AR11 are less siginificant than previuos model. We try a seasonal arima model.
#################################
fit <- arima(ts.series, c(0,1,3), seasonal= list(order=c(1,0,0), period=12))
fit
res <- resid(fit)
acf(res, ci.type='ma', lag.max=60)  # the residual ACFs look slightly better than previous model (outliers are less significant). Also MA3 is insignificant. We try a smaller model.
#################################
fit <- arima(ts.series, c(0,1,2), seasonal= list(order=c(1,0,0), period=12))
fit
res <- resid(fit)                   # the coefficients are significant. The residual ACF is similar to previous model.
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
fit <- arima(ts.series, c(1,0,0), seasonal= list(order=c(0,1,1), period=12))  # sharp decrease in seasonal part and slow decay in common component for ACF; sharp decrease in common part and slow decay in seasonal component for PACF
fit
res <- resid(fit)
acf(res, ci.type='ma', lag.max = 60)
#################################
fit <- arima(ts.series, c(1,0,1), seasonal= list(order=c(0,1,1), period=12))  # significant coefficients, significant drops in AIC
fit
res <- resid(fit)
acf(res, ci.type='ma')
#################################
fit <- arima(ts.series, c(2,0,1), seasonal= list(order=c(0,1,1), period=12))  # better residual ACF, drop in AIC
fit
res <- resid(fit)
acf(res, ci.type='ma', lag.max = 60)

tsdiag(fit)
hist(fit$residuals)         # histogram is symmetric and looks like a bell shape.
qqnorm(fit$residuals) 
qqline(fit$residuals)
plot(fit$residuals)
shapiro.test(fit$residuals)
LB.test(fit, lag=10, type="Ljung-Box")
#################################
fit <- arima(ts.series, c(1,0,2), seasonal= list(order=c(0,1,1), period=12))  # worse residual ACF compared to arima(2,0,1) x sarima(0,1,1); increase in AIC. Not desirable
fit
res <- resid(fit)
acf(res, ci.type='ma')





#################################
#################################
#################################
fit <- arima(ts.series, c(0,1,1), seasonal= list(order=c(0,1,1), period=12)) # from the ACF plot after differencing and seasonal differencing
fit
res <- resid(fit)
acf(res, ci.type='ma')
pacf(res)
#################################
fit <- arima(ts.series, c(0,1,2), seasonal= list(order=c(0,1,1), period=12))  # significant coefficients, better AIC
fit
res <- resid(fit)
acf(res, ci.type='ma', lag.max = 60)
#################################
fit <- arima(ts.series, c(0,1,2), seasonal= list(order=c(1,1,1), period=12))  # insignificant SAR1 coefficient, worse AIC
fit
res <- resid(fit)
acf(res, ci.type='ma', lag.max = 60)
#################################
fit <- arima(ts.series, c(1,1,2), seasonal= list(order=c(0,1,1), period=12))  # significant ar1 coefficient, better AIC
fit
res <- resid(fit)
acf(res, ci.type='ma', lag.max = 60)
acf(res, ci.type='ma', lag.max = 60)
#################################
fit <- arima(ts.series, c(1,1,1), seasonal= list(order=c(0,1,1), period=12))  # significant coefficient, better AIC
fit
res <- resid(fit)
acf(res, ci.type='ma', lag.max = 60)


tsdiag(fit)
hist(fit$residuals)
qqnorm(fit$residuals) 
qqline(fit$residuals)
plot(fit$residuals)
plot(scale(fit$residuals))
shapiro.test(fit$residuals)
LB.test(fit, lag=10, type="Ljung-Box")
################################# 
fit <- arima(ts.series, c(1,1,1), seasonal= list(order=c(0,1,3), period=12))  # slightly significant coefficients in sma2 and sma3. Better AIC
fit
res <- resid(fit)
acf(res, ci.type='ma', lag.max = 60)

tsdiag(fit)
hist(fit$residuals)
shapiro.test(fit$residuals)
LB.test(fit, lag=10, type="Ljung-Box")