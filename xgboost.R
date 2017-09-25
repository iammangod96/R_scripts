rm(list = ls())


#loading dataset
train <- read.csv("C:/Users/manish.sharma/Downloads/Adult_uci/train.csv")
test <- read.csv("C:/Users/manish.sharma/Downloads/Adult_uci/test.csv")
setDT(train)
setDT(test)


#loading libraries
library(data.table)
library(mlr)


#data profiling
colSums(is.na(test))
summary(train)


#quick data cleaning


#remove extra character from target variable
library(stringr)
test [,target := substr(target,start = 1,stop = nchar(target)-1)]


#remove leading whitespaces
char_col <- colnames(train)[ sapply (test,is.character)]
for(i in char_col) set(train,j=i,value = str_trim(train[[i]],side = "left"))
for(i in char_col) set(test,j=i,value = str_trim(test[[i]],side = "left"))


#set all missing value as "Missing" 
train[is.na(train)] <- "Missing" 
test[is.na(test)] <- "Missing"


