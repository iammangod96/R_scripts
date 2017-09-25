rm(list = ls())


#LOADING DATASETS
train <- read.csv("C:/Users/manish.sharma/Downloads/AV/big_mart_3/train.csv")
test <- read.csv("C:/Users/manish.sharma/Downloads/AV/big_mart_3/test.csv")


#loading libraries
library("psych") #pairs.panels
library(ggplot2) #ggplot
library("mlr") #impute
library("car") #recode



#data profiling
str(train)
summary(train)
#pairs.panels(train)
cor(train[,c(2,4,6,8,12)])*100
plot(Item_Outlet_Sales ~ Item_MRP, data = train)
abline(lm(Item_Outlet_Sales ~ Item_MRP, data = train),col = "red", pch = 2)
hist(train$Outlet_Establishment_Year)


#treating missing values
colSums(is.na(train))
colSums(is.na(test))

train$Outlet_Size <- ifelse(train$Outlet_Size == "", NA, train$Outlet_Size)
test$Outlet_Size <- ifelse(test$Outlet_Size == "", NA, test$Outlet_Size)
imputed_data_train <- impute(train, cols = list(Outlet_Size = imputeMode(), Item_Weight = imputeMean()))
train <- imputed_data_train$data
imputed_data_test <- impute(test, cols = list(Outlet_Size = imputeMode(), Item_Weight = imputeMean()))
test <- imputed_data_test$data


#variable transformation
str(train)

table(train$Item_Fat_Content)
train$Item_Fat_Content <- recode(train$Item_Fat_Content, "c('LF','low fat','Low Fat') = 'Low Fat'; 'reg' = 'Regular'")
test$Item_Fat_Content <- recode(test$Item_Fat_Content, "c('LF','low fat','Low Fat') = 'Low Fat'; 'reg' = 'Regular'")



#training a linear model
lr_model <- lm(data = train, Item_Outlet_Sales ~ . -Item_Identifier -Outlet_Identifier)
summary(lr_model)


#predictiction of linear model
prediction_test <- predict(lr_model , newdata = test)
sol_DF <- data.frame(test$Item_Identifier, test$Outlet_Identifier, prediction_test)
colnames(sol_DF) <- c('Item_Identifier','Outlet_Identifier','Item_Outlet_Sales')
write.csv(sol_DF, file = "BM3_solution.csv", row.names = FALSE)
