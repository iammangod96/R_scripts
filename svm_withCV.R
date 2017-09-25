rm(list = ls())


#loading libraries
library(e1071)


#loading datasets
data("iris")


#sampling training and testing datasets
sample <- iris[sample(nrow(iris)),]
train <- sample[1:105,]
test <- sample[106:150,]


#tuning svm
tune <- tune(svm, 
             Species ~ ., 
             data = train, 
             kernel = "radial", 
             scale = FALSE,
             ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100))
             )

summary(tune)

best.tune(
  method = svm, 
  train.x = Species ~ ., 
  data = train, 
  kernel = "radial", 
  scale = FALSE,
  ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100))
)


#building svm model
model <- svm(Species~.,data=train,kernel ="radial",cost=1,scale=FALSE)


#prediction
pred <- predict(model, test)


#evaluation
pred_df <- data.frame(actual = test$Species, predicted = pred)
acc <- ((sum(pred_df$actual == pred_df$predicted))*100)/(nrow(pred_df))
paste(acc, "%")
