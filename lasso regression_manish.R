rm(list = ls())

#loading dataset
house_DS <- read.csv("C:/Users/manish.sharma/Downloads/kc_house_data.csv")


#loading libraries
#install.packages("corrplot")
#install.packages("glmnet")
#nstall.packages("lars")
library("corrplot")     #corrplot
library("glmnet")
library("lars")


#data profiling
str(house_DS)
summary(house_DS)
x <- cor(house_DS[,-2])
par(mfrow = c(1,1))
corrplot(x, method="number")
plot(house_DS$price ~ house_DS$date)
plot(house_DS$long ~ house_DS$lat, col="brown")


#training lasso model
lasso_model <- lars(x = as.matrix(house_DS[,-c(1,2,3,17)]),y =  house_DS[,3], type = "lasso", trace = T)
summary(lasso_model)


#Rss       The Residual sum of Squares
#Cp        The Cp statistic
#Cp -> It is applied in the context of model selection, where a number of predictor variables are available for predicting some outcome, 
#and the goal is to find the best model involving a subset of these predictors. A small value of Cp means that the model is relatively precise.