rm(list = ls())


#loading libraries
library(forecast)
library(tseries)
library(ggplot2)


#loading datasets
train <- read.csv("E:/ser_docs_manish/manish R scripts/day.csv")


#data profiling
str(train)
summary(train)
train$dteday <- as.Date(train$dteday)


#plotting
ggplot(train, aes(dteday, cnt)) + geom_line() + scale_x_date('month')


#removing outliers
cnt_ts <- ts(train[,c('cnt')])
train$cnt_clean <- tsclean(cnt_ts)
ggplot() + geom_line(data = train, aes(dteday, cnt_clean)) 



#moving average plots
train$cnt_ma <- ma(train$cnt_clean, order = 7)
train$cnt_ma30 <- ma(train$cnt_clean, order = 30)
ggplot()+
  geom_line(data = train, aes(train$dteday, train$cnt_clean, colour = "Counts", lwd = 0.1))+
  geom_line(data = train, aes(train$dteday, train$cnt_ma, color = "moving average-7", lwd = 0.1))+
  geom_line(data = train, aes(train$dteday, train$cnt_ma30, color = "moving average-30", lwd = 0.2))


#decomposing
count_ma <- ts(na.omit(train$cnt_ma), frequency = 30)
decomp <- stl(count_ma, s.window = "periodic")
plot(decomp)
deseasonal_cnt <- seasadj(decomp)


#adf test
adf.test(count_ma, alternative = "stationary")


#acf and pacf plots
par(mfrow = c(2,1))
Acf(count_ma, main='')
Pacf(count_ma, main = '')


#differencing order 1
count_d1 <- diff(deseasonal_cnt, differences =  1)
plot(deseasonal_cnt)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")
Acf(count_d1,main='ACF')
Pacf(count_d1,main='PACF')


#auto fitting arima
fit <- auto.arima(deseasonal_cnt, seasonal = FALSE)
tsdisplay(residuals(fit))


#fitting arima
fit2 <- arima(deseasonal_cnt, order = c(1,1,7))
fit2
tsdisplay(residuals(fit2))


#forecasting
fcast <- forecast(fit2, h = 30)
plot(fcast)


#holdout evaluation
hold <- window(ts(deseasonal_cnt), start = 700)
fit_no_holdout <- arima(ts(deseasonal_cnt[-c(700:731)]), order = c(1,1,7))
fcast_no_holdout <- forecast(fit_no_holdout, h = 31)
plot(fcast_no_holdout, main="")
lines(ts(deseasonal_cnt))


#putting back seasonality and improving the naive model
fit_w_seasonality <- auto.arima(deseasonal_cnt, seasonal = TRUE)
fit_w_seasonality
seas_forecast <- forecast(fit_w_seasonality, h = 30)
plot(seas_forecast)
