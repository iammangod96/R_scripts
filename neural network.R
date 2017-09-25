rm(list = ls())


#loading libraries
library(data.table)
library(neuralnet)


#loading dataset
train <-  fread("E:/ser_docs_manish/manish R scripts/cereals.csv")


#random sampling
samplesize <- 0.6 * nrow(train)
set.seed(1)
ix <- sample(seq_len(nrow(train)), size = samplesize)


#training and testing data
train_set <- train[ix,]
test_set <- train[-ix,]


#scaling data
max <- apply(train, 2, max)
min <- apply(train,2,min)
scaled <- as.data.frame(scale(train, center = min, scale = max - min))


#scaled training and testing data
trainNN <- scaled[ix,]
testNN <- scaled[ix,]


#neural network model
set.seed(2)
nn_model <- neuralnet(rating ~ calories + fat + protein + sodium + fiber,trainNN, hidden = 3, linear.output =  1)
plot(nn_model)
