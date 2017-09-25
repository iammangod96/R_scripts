rm(list = ls())

#loading dataset

ins <- read.csv("C:/Users/manish.sharma/Downloads/mlwithr/insurance.csv")



#loading libraries

library("psych") #for pairs.panels


#data profiling

str(ins)
summary(ins$charges)
hist(ins$charges)
table(ins$region)
summary(ins$age)
summary(ins$bmi)
table(ins$children)

plot(ins$charges~ ins$age)

#relationships between features

cor(ins[c('age','bmi','children','charges')])*100 #can have only numeric values
pairs.panels(ins[c('age','bmi','children','charges')]) #can have non-numeric types also



#training model

ins_model <- lm(formula = charges ~ ., data=ins)
ins_model


#training and testing data
set.seed(5)
train_sample <- sample(1338,1000)
ins_train <- ins[train_sample,]
ins_test <- ins[-train_sample,]
par(mfrow=c(2,1))
hist(ins_train$charges)
hist(ins_test$charges)



#evaluating model

summary(ins_model)

#prediction example
test_data <- data.frame(age <- c(21),sex<-c("male"),bmi<-c(24.000),children<-c(0),smoker<-c("no"),region<-c("northwest"))
ored <- predict(ins_model, ins_test[,-7])
ored <- as.vector(ored)


#improvements
ins$age2 <- ins$age^2 #for quadratic regression with age
ins$bmi30 <- ifelse(ins$bmi > 30, 1, 0)
str(ins)


#training an improved model
ins_model2  <- lm(charges ~ age + age2 + smoker +  sex + bmi + children + region + smoker*bmi30, data = ins)
ins_model2b <- lm(charges ~ poly(age,2)+ smoker +  sex + bmi + children + region + smoker*bmi30, data = ins)
summary(ins_model2)
summary(ins_model2b)



