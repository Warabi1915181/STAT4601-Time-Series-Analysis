# install.packages('TSA')
# install.packages('ggplot2')
# install.packages('latex2exp')
library(ggplot2)
library(TSA)
library(latex2exp)

data = read.csv('BTC-USD.csv')
data[,1] = as.Date(data[,1])

# time series plot
ggplot(data) +
  geom_line(aes(timestamp, close)) +
  ylab('closing price')

# take log
data$log.close = log(data$close)
ggplot(data) +
  geom_line(aes(timestamp, log.close)) +
  ylab('log closing price')

# Sample ACF and PACF
acf(data$log.close, ci.type='ma')
pacf(data$log.close)

# take difference
diff1.close = diff(data$log.close)
ggplot() +
  geom_line(aes(x=data[2:2934,1], y=diff1.close)) +
  ylab(TeX('$\\nabla$closing price'))
acf(diff1.close, ci.type='ma')
pacf(diff1.close)

# take difference twice
diff2.close = diff(diff1.close)
ggplot() +
  geom_line(aes(x=data[3:2934,1], y=diff2.close)) +
  ylab(TeX('$\\nabla^2$closing price'))
acf(diff2.close, ci.type='ma')
pacf(diff2.close)

# take difference once, and followed by seasonal difference with period=7
diffs.close = diff(diff1.close, lag = 7)
ggplot() +
  geom_line(aes(x=data[9:2934,1], y=diffs.close)) +
  ylab(TeX('$\\nabla_s\\nabla$closing price'))
acf(diffs.close, ci.type='ma', lag.max=60)
pacf(diffs.close, lag.max=60)

# take difference once, and followed by seasonal difference with period=30
diffs.close = diff(diff1.close, lag = 30)
ggplot() +
  geom_line(aes(x=data[32:2934,1], y=diffs.close)) +
  ylab(TeX('$\\nabla_s\\nabla$closing price'))
acf(diffs.close, ci.type='ma', lag.max=60)
pacf(diffs.close, lag.max = 60)
