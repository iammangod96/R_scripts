rm(list = ls())


#loading libraries
library(quantmod)
library(tseries)
library(forecast)
library(xts)
library(time)
library(timeSeries)


#loading datasets
getSymbols("TECHM.NS", from='2012-01-01', to='2015-01-01')
stock_prices <- TECHM.NS[,4]
plot.ts(stock_prices)


#differencing
plot.ts(log(stock_prices))
stock <- diff(log(stock_prices), lag = 1)
stock <- stock[!is.na(stock)]
plot(stock)


#dickey-fuller test
adf.test(stock) #since prob so small, so alternative hypothesis is true


#breakpoint for training and testing data sets
breakpoint <- floor(nrow(stock)*(2.9/3)) #96%


#acf and pacf plots
acf.stock <- acf(stock[1:breakpoint,], lag.max = 100, main = "ACF for q")
pacf.stock <- pacf(stock[1:breakpoint,], lag.max = 100, main = "PACF for p")


#model results storage preparation
fcast <- data.frame(Forecasted = numeric())
actual_series <- xts(0, as.Date("2014-11-25","%Y-%m-%d"))

#forecasting
for(b in breakpoint:(nrow(stock)-1))
{
  stock_train <- stock[1:b,]
  stock_test <- stock[(b+1):nrow(stock),]
  
  fit <- arima(stock_train, order = c(2,0,2), include.mean = FALSE)
  summary(fit)
  
  arima.fcast <- forecast(fit,h=1,level=99)
  plot(arima.fcast, main = "ARIMA forecast")
  
  fcast <- rbind(fcast, arima.fcast$mean[1])
  colnames(fcast) <- c("Forecasted")
  
  actual_return <- stock[(b+1),]
  actual_series = c(actual_series,xts(actual_return))
  rm(actual_return)
  
  print(stock_prices[(b+1),])
  print(stock_prices[(b+2),])
}


#comparing forecast and actual
actual_series <- actual_series[-1]
fcast <- xts(fcast, index(actual_series))

par(mfrow = c(1,1))
plot(actual_series)
lines(fcast, col = "red", lwd = 1.5)


#computing accuracy based on sign
comparison <- merge(actual_series, fcast)
str(comparison)
comparison$sign <- sign(comparison$actual_series) == sign(comparison$Forecasted)

ac <- (sum(comparison$sign == 1)/nrow(comparison))*100
ac
