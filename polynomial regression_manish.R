rm(list = ls())

#data
year <- c(1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969)
population <- c(4835, 4970, 5085, 5160, 5310, 5260, 5235, 5255, 5235, 5210, 5175)
sample1 <- data.frame(year,population)


#plotting data
par(mfrow=c(1,1))
plot(sample1$year, sample1$population, type = 'b', col="blue")


#training model
fit1 <- lm(formula = sample1$population ~ sample1$year)
abline(fit1, col = "red")
summary(fit1)
#46.63%


fit2 <- lm(formula = sample1$population ~ sample1$year + I(sample1$year^2))
#abline(fit2, col = "red") #doesn't work
lines(sample1$year, fitted(fit2), col='pink', type='b') 
summary(fit2)
#94.07%

fit2b <- lm(sample1$population ~ poly(sample1$year, 2, raw = F))
lines(sample1$year, fitted(fit2b), col='pink', type='b') 
summary(fit2b)
#94.07%

fit3b <- lm(sample1$population ~ poly(sample1$year, 3, raw = F))
lines(sample1$year, fitted(fit3b), col='green', type='b') 
summary(fit3b)
#96.22%

fit5b <- lm(sample1$population ~ poly(sample1$year, 5, raw = F))
lines(sample1$year, fitted(fit5b), col='yellow', type='b') 
summary(fit5b)
#97.24%

legend('bottomright',lty=1, col=c('red','pink', 'green',' yellow'), legend= c(1:4),bty='o', cex=.75)


#The R-squared or coefficient of determination can be thought of as a percent. 
#It gives you an idea of how many data points fall within the results of the 
#line formed by the regression equation. The higher the coefficient, the higher
#percentage of points the line passes through when the data points and line are
#plotted. 