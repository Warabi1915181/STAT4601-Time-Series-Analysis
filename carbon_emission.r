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
acf(diff1.value, ci.type='ma',lag.max=60, main='Sample ACF For First Difference on Log(Emission)')
pacf(diff1.value, main='Sample PACF For First Difference on Log(Emission)')

# take difference twice
diff2.value = diff(diff1.value)
ggplot() +
  geom_line(aes(x=data[3:n,1], y=diff2.value)) +
  ylab(TeX('$\\nabla^2$value'))
acf(diff2.value, ci.type='ma', lag.max=60)
pacf(diff2.value)

# trend, seasonal, residuals
ts.series = ts(data$log.value, frequency=12, start=c(1973, 1))
ts.stl = stl(ts.series, s.window='periodic')
plot(ts.stl)


# seasonal difference with period=12
diffs.value = diff(data$log.value, lag = 12)
ggplot() +
  geom_line(aes(x=data[13:n,1], y=diffs.value)) +
  ylab(TeX('$\\nabla_s$value'))
acf(diffs.value, ci.type='ma', lag.max=60, main='Sample ACF For First Seasonal Difference on Log(Emission)')
pacf(diffs.value, lag.max=60,  main='Sample PACF For First Seasonal Difference on Log(Emission)')

# take difference once, and followed by seasonal difference with period=12
diffs2.value = diff(diff1.value, lag = 12)
ggplot() +
  geom_line(aes(x=data[14:n,1], y=diffs2.value)) +
  ylab(TeX('$\\nabla\\nabla_s$value'))
acf(diffs2.value, ci.type='ma', lag.max=60)
pacf(diffs2.value, lag.max=60)

# model fitting
#################################
############# Good ##############
#################################
fit <- arima(ts.series, c(12,1,0))
fit
acf(fit$residuals, ci.type='ma', main=TeX('Residual ACF Plot for ARIMA$(12,1,0)$'))  # some significant ACFs at lag 4. We try adding MA components
#################################
fit <- arima(ts.series, c(12,1,4))
fit
acf(fit$residuals, ci.type='ma', main=TeX('Residual ACF Plot for ARIMA$(12,1,4)$')) # the significant ACFs at lag 4 are gone. The coefficients from MA2 to MA4 are insignificant. We try a smaller model.
#################################
fit <- arima(ts.series, c(12,1,1))
fit
acf(fit$residuals, ci.type='ma', main=TeX('Residual ACF Plot for ARIMA$(12,1,1)$')) # The performance is similar without MA2 to MA4. The coefficients from AR1 to AR11 are also less significant than previous model. We try a seasonal arima model.
#################################
fit <- arima(ts.series, c(0,1,1), seasonal= list(order=c(1,0,0), period=12))
fit
acf(fit$residuals, ci.type='ma', main=TeX('Residual ACF Plot for SARIMA$(0,1,1) \\times (1,0,0)_{12}$'))  # Worse performance in general. We try bigger models.
#################################
fit <- arima(ts.series, c(1,1,1), seasonal= list(order=c(1,0,0), period=12))
fit
acf(fit$residuals, ci.type='ma', main=TeX('Residual ACF Plot for SARIMA$(1,1,1) \\times (1,0,0)_{12}$'))
#################################
fit <- arima(ts.series, c(0,1,2), seasonal= list(order=c(1,0,0), period=12)) # the coefficients are significant. The residual ACF is similar to previous model. (Final model)
fit                         # AIC = -1839.33
acf(fit$residuals, ci.type='ma', main=TeX('Residual ACF Plot for SARIMA$(0,1,2) \\times (1,0,0)_{12}$'))

tsdiag(fit)
hist(fit$residuals, main='Histogram of Fit Residuals', xlab='fit residuals')
qqnorm(fit$residuals) 
qqline(fit$residuals)

res <- scale(fit$residuals)
res.ts <- ts(res, frequency=12, start=c(1973, 2))
plot(res.ts, xlab='Time', ylab='', main='Standardized Residuals Scatterplot', type='p')
shapiro.test(fit$residuals)
LB.test(fit, lag=10, type="Ljung-Box")
LB.test(fit, lag=20, type="Ljung-Box")




#################################
############# Good ##############
#################################
fit <- arima(ts.series, c(3,0,0), seasonal= list(order=c(0,1,0), period=12))  # slow decay in ACF, quick decay after the first 3 lags (except seasonal part) in PACF. Let's ignore seasonal part for now.
fit                         # AIC = -1823.25
acf(fit$residuals, ci.type='ma', main=TeX('Residual ACF Plot for SARIMA$(3,0,0) \\times (0,1,0)_{12}$'), lag.max = 60)
#################################
fit <- arima(ts.series, c(3,0,0), seasonal= list(order=c(0,1,1), period=12))
fit
acf(fit$residuals, ci.type='ma', main=TeX('Residual ACF Plot for SARIMA$(3,0,0) \\times (0,1,0)_{12}$'), lag.max = 60)

fit <- arima(ts.series, c(3,0,3), seasonal= list(order=c(0,1,1), period=12))  
fit
acf(fit$residuals, ci.type='ma', lag.max = 60)

fit <- arima(ts.series, c(4,0,3), seasonal= list(order=c(0,1,1), period=12))  
fit
acf(fit$residuals, ci.type='ma', lag.max = 60)
pacf(fit$residuals, lag.max = 60)

fit <- arima(ts.series, c(4,0,3), seasonal= list(order=c(0,1,2), period=12))  
fit
acf(fit$residuals, ci.type='ma', lag.max = 60)
#################################
fit <- arima(ts.series, c(3,0,0), seasonal= list(order=c(0,1,2), period=12))  # Improvement in AIC. Coefficients are slightly significant. Significant residual acf at lag 3
fit                         # AIC = -2004.01
acf(fit$residuals, ci.type='ma', main=TeX('Residual ACF Plot for SARIMA$(3,0,0) \\times (0,1,2)_{12}$'), lag.max = 60)
#################################
fit <- arima(ts.series, c(3,0,3), seasonal= list(order=c(0,1,2), period=12))  # large standard error for coefficients in common part. try others parameters for common part
fit                         # AIC = -2036.1
acf(fit$residuals, ci.type='ma', main=TeX('Residual ACF Plot for SARIMA$(3,0,3) \\times (0,1,2)_{12}$'), lag.max = 60)
#################################
fit <- arima(ts.series, c(3,0,4), seasonal= list(order=c(0,1,2), period=12))  # standard error for coefficients in common part are still large. No improvement in AIC. Try another model
fit                         # AIC = -2037.34
acf(fit$residuals, ci.type='ma', main=TeX('Residual ACF Plot for SARIMA$(3,0,4) \\times (0,1,2)_{12}$'), lag.max = 60)
#################################
fit <- arima(ts.series, c(4,0,3), seasonal= list(order=c(0,1,2), period=12))  # improvement in AIC. Standard error for coefficients are smaller. Residuals ACF look better. (Final Model)
fit                         # AIC = -2055.55
acf(fit$residuals, ci.type='ma', main=TeX('Residual ACF Plot for SARIMA$(4,0,3) \\times (0,1,2)_{12}$'), lag.max = 60)

tsdiag(fit)
hist(fit$residuals)         # bell shape.
qqnorm(fit$residuals)       # the head is off the diagonal line
qqline(fit$residuals)
res <- scale(fit$residuals)
res.ts <- ts(res, frequency=12, start=c(1974, 1))
plot(res.ts, xlab='Time', ylab='', main='Standardized Residuals Scatterplot', type='p')
shapiro.test(fit$residuals) # do not pass the shapiro-wilk normality test
LB.test(fit, lag=20, type="Ljung-Box")
LB.test(fit, lag=30, type="Ljung-Box")
LB.test(fit, lag=40, type="Ljung-Box")



#################################
############# Good ##############
#################################
fit <- arima(ts.series, c(5,1,0), seasonal= list(order=c(0,1,0), period=12))  # significant PACF at first 5 lags, let's ignore seasonal part for now. Most coefficients are quite significant. (Final Model)
fit                         # AIC = -1784.58
acf(fit$residuals, ci.type='ma')

tsdiag(fit)
hist(fit$residuals)         # bell shape.
qqnorm(fit$residuals)       # qq plot looks normal
qqline(fit$residuals)
plot(fit$residuals)
shapiro.test(fit$residuals) # passes shapiro-wilk normality test with 0.2281 p-value
LB.test(fit, lag=10, type="Ljung-Box")  # p-value = 0.2032
#################################
fit <- arima(ts.series, c(5,1,0), seasonal= list(order=c(0,1,1), period=12))  # fail normality test, bad residual acf
fit                         # AIC = -2020.19, significant improvement in AIC
acf(fit$residuals, ci.type='ma')
#################################
fit <- arima(ts.series, c(5,1,0), seasonal= list(order=c(1,1,0), period=12))  # Residual still look normal. Slightly worse residual acf. Ljung-Box test p-value = 0.02033. Not too bad.
fit                         # AIC = -1870.39, considerable improvement in AIC
acf(fit$residuals, ci.type='ma')
#################################
fit <- arima(ts.series, c(6,1,0), seasonal= list(order=c(0,1,0), period=12))  # Residuals are normal. Similar residual acf. Insignificant ar6 coefficient.
fit                         # AIC = -1783.06, no improvement.
acf(fit$residuals, ci.type='ma')
#################################
fit <- arima(ts.series, c(5,1,1), seasonal= list(order=c(0,1,0), period=12))  # Residuals are normal. Similar residual acf but slightly worse. Significant coefficient in ma1 but insignificant in ar2-ar5.
fit                         # AIC = -1816.58, considerable improvement
acf(fit$residuals, ci.type='ma')
#################################
fit <- arima(ts.series, c(1,1,1), seasonal= list(order=c(0,1,0), period=12))  # Not very confident that residuals are normal. Worse residual acf. Ljung-Box test p-value is 0.001292. Not optimal.
fit                         # AIC = -1802.72, considerable improvement
acf(fit$residuals, ci.type='ma')




# residual analysis
#################################
fit1 <- arima(ts.series, c(0,1,2), seasonal= list(order=c(1,0,0), period=12))
fit2 <- arima(ts.series, c(), seasonal= list(order=c(), period=12)) 

Residual_analysis <- function(fit, type) {
  res <- resid(fit)
  
  if (type=="acf") {
    acf(res, ci.type='ma')
    print(LB.test(fit, lag=10, type="Ljung-Box"))
  } else if (type=="pacf") {
    pacf(res)
    # abline(h=1.96/sqrt(length(res)), col='blue', lty = "dashed")
    # abline(h=-1.96/sqrt(length(res)), col='blue', lty = "dashed")
  } else if (type=="diag") {
    tsdiag(fit)
  } else if (type=="norm") {
    par(mfrow=c(2,2))
    hist(res)
    qqnorm(res);qqline(res)
    plot(res)
    par(mfrow=c(1,1))
    print(shapiro.test(res))
  } else {
    print("specify type")
  }
}

Residual_analysis(fit1, "diag")
Residual_analysis(fit1, "norm")
Residual_analysis(fit1, "acf")
Residual_analysis(fit1, "pacf")
Residual_analysis(fit2, "diag")
Residual_analysis(fit2, "norm")
Residual_analysis(fit2, "acf")
Residual_analysis(fit2, "pacf")

# For ARIMA(0,1,2)*SARIMA(1,0,0), residual follows MA(1)
adjust_fit1_1 <- arima(ts.series, c(2,1,1), seasonal= list(order=c(1,0,0), period=12)) 
adjust_fit1_1
Residual_analysis(adjust_fit1_1, "diag")
Residual_analysis(adjust_fit1_1, "norm")
Residual_analysis(adjust_fit1_1, "acf")
Residual_analysis(adjust_fit1_1, "pacf")

# For ARIMA()*SARIMA(), residual follows 
adjust_fit1_2 <- arima(ts.series, c(), seasonal= list(order=c(), period=12)) 
adjust_fit1_2
Residual_analysis(adjust_fit1_2, "diag")
Residual_analysis(adjust_fit1_2, "norm")
Residual_analysis(adjust_fit1_2, "acf")
Residual_analysis(adjust_fit1_2, "pacf")

# overfitting
#################################
# add 1 for p, q, P, Q for ARIMA(0,1,2)*SARIMA(1,0,0)
o_fit1_1 <- arima(ts.series, c(1,1,2), seasonal= list(order=c(1,0,0), period=12))
fit1$coef;o_fit1_1$coef
Residual_analysis(o_fit1_1, "acf") #discarded as residuals doesn't follow normal
o_fit1_2 <- arima(ts.series, c(0,1,3), seasonal= list(order=c(1,0,0), period=12))
fit1$coef;o_fit1_2$coef  #discarded as coef doesn't change a lot
o_fit1_3 <- arima(ts.series, c(0,1,2), seasonal= list(order=c(2,0,0), period=12))
fit1$coef;o_fit1_3$coef  #discarded as coef doesn't change a lot
o_fit1_4 <- arima(ts.series, c(0,1,2), seasonal= list(order=c(1,0,1), period=12), method="ML") 
#forcing to use maximum likelihood to avoid AR coef fall outside the stationary process region
fit1$coef;o_fit1_4$coef  #discarded as coef doesn't change a lot

# add 1 for p, q, P, Q for ARIMA()*SARIMA()
o_fit2_1 <- arima(ts.series, c(), seasonal= list(order=c(), period=12))
fit2$coef;o_fit2_1$coef
o_fit2_2 <- arima(ts.series, c(), seasonal= list(order=c(), period=12))
fit2$coef;o_fit2_2$coef
o_fit2_3 <- arima(ts.series, c(), seasonal= list(order=c(), period=12))
fit2$coef;o_fit2_3$coef
o_fit2_4 <- arima(ts.series, c(), seasonal= list(order=c(), period=12))
fit2$coef;o_fit2_4$coef

# Forecasting
#################################
# Remove data of last year
tspred.series = ts(data$log.value[1:583], frequency=12, start=c(1973, 1))

# Model1, ARIMA(0,1,2)*SARIMA(1,0,0)
fit1 <- arima(tspred.series, c(0,1,2), seasonal= list(order=c(1,0,0), period=12))
pred<-12
fit1.pred<-predict(fit1, n.ahead=pred)

# Model2, ARIMA(4,0,3)*SARIMA(0,1,2)
fit2 <- arima(tspred.series, c(4,0,3), seasonal= list(order=c(0,1,2), period=12))
fit2.pred<-predict(fit2, n.ahead=pred)

# ggplot
pred_date<-seq(as.Date("2021-08-01"), by = "month", length.out = 12)
origdata<-data.frame(time=data$time, original=data$log.value)

# Model1
plotpred1<-data.frame(time=pred_date, predict=fit1.pred$pred, ci_low=fit1.pred$pred-1.96*fit1.pred$se, ci_up=fit1.pred$pred+1.96*fit1.pred$se)
plotmodel1<-merge(origdata, plotpred1, by="time", all.x=TRUE)
shortenedplot1<-plotmodel1[450:595,]
ggplot() + 
  geom_line(shortenedplot1, mapping=aes(x = time, y = exp(original))) +
  geom_line(shortenedplot1, mapping=aes(x = time, y = exp(predict)), colour = "red") +
  geom_line(shortenedplot1, mapping=aes(x = time, y = exp(ci_low)), colour = "blue", linetype="dashed") +
  geom_line(shortenedplot1, mapping=aes(x = time, y = exp(ci_up)), colour = "blue", linetype="dashed") +
  ylab("data") +
  ggtitle("Model 1 Forecasting")

# Model2
plotpred2<-data.frame(time=pred_date, predict=fit2.pred$pred, ci_low=fit2.pred$pred-1.96*fit2.pred$se, ci_up=fit2.pred$pred+1.96*fit2.pred$se)
plotmodel2<-merge(origdata, plotpred2, by="time", all.x=TRUE)
shortenedplot2<-plotmodel2[450:595,]
ggplot(shortenedplot2) + 
  geom_line(mapping=aes(x = time, y = exp(original))) +
  geom_line(mapping=aes(x = time, y = exp(predict)), colour = "red") +
  geom_line(mapping=aes(x = time, y = exp(ci_low)), colour = "blue", linetype="dashed") +
  geom_line(mapping=aes(x = time, y = exp(ci_up)), colour = "blue", linetype="dashed") +
  ylab("data") +
  ggtitle("Model 2 Forecasting")

# calculate RMSE
model1data<-plotmodel1[584:595, 1:3]
rmse1<-sqrt(mean(exp(model1data$original) - exp(model1data$predict))^2)
rmse1
model2data<-plotmodel2[584:595, 1:3]
rmse2<-sqrt(mean(exp(model2data$original) - exp(model2data$predict))^2)
rmse2
