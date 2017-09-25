rm(list = ls())

#loading libraries
library(ggplot2) #ggplot
library(e1071) #svm


#loading dataset
iris <- read.csv("E:/ser_docs_manish/manish R scripts/iris.data", header = FALSE)
colnames(iris) <- c("Sepal_length","Sepal_width","Petal_length","Petal_width","Species")
train <- iris[,1:2] 


#data profiling
ggplot(data = iris, aes(Sepal_length, Sepal_width)) + geom_point(aes(color = Species))


#defining predictors and outcome
predictors <- c("Sepal_length", "Sepal_width")
outcome <- "Species"


#svm model
#svm_model <- svm(data = iris, x = as.matrix(iris[,predictors]), y = iris[,outcome],kernel = 'linear')
svm_model <- svm(data = iris, Species ~ ., kernel = 'linear')
svm_model
plot(svm_model, data = iris,formula =  Petal_length ~ Petal_width)
 