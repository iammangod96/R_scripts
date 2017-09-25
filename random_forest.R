rm(list = ls())


#loading datasets
data("iris")


#loading libraries
library("psych") #pairs.panels
library("ggplot2") #qplot
library("rpart")
library("caret")
library("rattle") #fancy trees

#data profiling
summary(iris)
cor(iris[-5])
head(iris)
pairs.panels(iris, cex.cor = 2, cex = 2)
qplot(iris$Petal.Length, iris$Petal.Width, colour = iris$Species)


#data seperation
train.flags <- createDataPartition(iris$Species, p = 0.5, list = FALSE)
train <- iris[train.flags,]
test <- iris[-train.flags,]


#training model