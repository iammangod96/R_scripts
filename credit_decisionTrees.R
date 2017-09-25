rm(list = ls())
credit <- read.csv("C:/Users/manish.sharma/Downloads/mlwithr/credit.csv")

#data profiling

str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
hist(credit$months_loan_duration, breaks = 20,xlab="months", main="loan duration")
summary(credit$amount)
hist(credit$amount, breaks = 20, xlab="amount", main="load amount")

#training and testing seperation

set.seed(123)
train_sample <- sample(1000,900)
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

#training model

#install.packages("C50")
library("C50")
credit_model <- C5.0(credit_train[-17], factor(credit_train$default)) #17th column = default
summary(credit_model)

#predicting test set

credit_pred <- predict(credit_model, credit_test)

#Evaluation of prediction

CrossTable(credit_test$default, credit_pred, prop.chisq = F,prop.r = F,prop.t = F ,dnn=c('actual default','predicted default'))
#74%