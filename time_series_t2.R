rm(list = ls())


#loading libraries
library(forecast)
library(zoo) #rollmean, rollapply
library(ggplot2) #ggplot
library(TTR) #EMA


#loading datasets
data(AirPassengers)


#data profiling
str(AirPassengers)
summary(AirPassengers)
plot.ts(AirPassengers)
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)


#rolling mean and variance
rolmean <- rollmean(x = AirPassengers, k = 12)
rolvar <- rollapply(data = AirPassengers, width  = 12, FUN = sd, fill = NA)
ggplot() +
  geom_line(data = AirPassengers, aes(time(AirPassengers),AirPassengers , color = "AirPassenger")) +
  geom_line(data = rolmean, aes(time(rolmean),rolmean, color = "rolling mean")) +
  geom_line(data = rolvar, aes(time(rolvar),rolvar, color = "rolling variance"))


#dickey-fuller test
adf.test(AirPassengers, alternative = "stationary") #giving wrong results


#eliminating trend
ts_log <- log(AirPassengers)
plot.ts(ts_log) #still positive trend showing
#method-1:Removing moving average
rolmean_log <- rollmean(ts_log, k = 12)
ts_log_moving_avg_diff <- ts_log  - rolmean_log
rolmean_log1 <- rollmean(ts_log_moving_avg_diff, k = 12)
ggplot() +
  geom_line(data = ts_log_moving_avg_diff, aes(time(ts_log_moving_avg_diff), ts_log_moving_avg_diff, color = 'ts_log_moving_avg_diff')) +
  geom_line(data = rolmean_log1, aes(time(rolmean_log1), rolmean_log1, color = 'rolmean_log'))
adf.test(ts_log_moving_avg_diff, alternative = 'stationary') #giving wrong results
#method-2:exponentially weighted moving average
exp_w_mean <- EMA(x = ts_log, n  = 12) 
ggplot() +
  geom_line(data = ts_log, aes(time(ts_log),ts_log , color = "AirPassenger_log")) +
  geom_line(data = exp_w_mean, aes(time(exp_w_mean),exp_w_mean, color = "Exp_w_mean")) 
ts_log_expw_diff <- ts_log - exp_w_mean
exp_w_mean1 <- EMA(ts_log_expw_diff, n = 12)
ggplot() +
  geom_line(data = ts_log_expw_diff, aes(time(ts_log_expw_diff), ts_log_expw_diff, color = 'ts_log_moving_avg_diff')) +
  geom_line(data = exp_w_mean1, aes(time(exp_w_mean1), exp_w_mean1, color = 'expmean_log'))

#The simple trend reduction techniques discussed before don't work in all 
#cases, particularly the ones with high seasonality. 


#eliminating trend and seasonality
#method-3:differencing
ts_log_diff <- diff(ts_log)
plot(ts_log_diff)
adf.test(ts_log_diff, alternative = 'stationary')
#method-4:decomposing
ts_log_decompose <- decompose(ts_log)
ts_log_decompose_res <- ts_log_decompose$random
plot(ts_log_decompose)
plot(ts_log_decompose_res)


#forecasting
Acf(ts_log_diff)
Pacf(ts_log_diff)
model <- arima(ts_log_diff, order = c(2,1,2))
fcast <- forecast(model, h = 5)
plot(fcast)
