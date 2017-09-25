rm(list = ls())

#loading dataset
ins <- read.csv("C:/Users/manish.sharma/Downloads/mlwithr/insurance.csv")


#date profiling
str(ins)
table(ins$sex)


#loading libraries
library("MASS") #for stepAIC


#relationships between features
cor(ins[c('age','bmi','children','charges')])*100 #can have only numeric values


#training regression model
fit <- lm(charges ~ ., data = ins)
summary(fit)
#Multiple R-squared:  0.7509


#training stepwise regression model using step()
step_model <- step(object = fit, direction = "backward", trace = T)
summary(step_model)
#Multiple R-squared:  0.7509


#training stepwise regression model using MASS package
step_model_MASS <- stepAIC(fit, direction = "backward")
summary(step_model_MASS) #excluded 'sex' attribute
step_model_MASS$anova
#Multiple R-squared:  0.7509


#The Akaike information criterion (AIC) is a measure of the 
#relative quality of statistical models. Preferred model is
#the one with minimum AIC.

