rm(list = ls())

#loading dataset
house_DS <- read.csv("C:/Users/manish.sharma/Downloads/kc_house_data.csv")


#loading libraries
library("corrplot")


#data profiling
x <- cor(house_DS[,-2])
corrplot(x)

