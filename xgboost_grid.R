rm(list = ls())


#loading libraries
library("MASS") #Boston dataset
library("caret") #parameter tuning


#loading dataset
data(Boston)
head(Boston)


#sepearting train and test data
set.seed(5)
ix <- sample(1:nrow(Boston), 0.8*nrow(Boston))
train_df <- Boston[ix,]
test_df <- Boston[-ix,]


#tune settings
xgb_grid <- expand.grid(
  nrounds = c(250,500,1000),
  max_depth = c(1,2,4),
  eta = c(0.001, 0.003, 0.01),
  gamma = c(0,1,2),
  min_child_weight = c(1, 2),
  colsample_bytree = c(1,0.5,0.25),
  subsample = 1
)

grid_ix <- sample(1:nrow(xgb_grid), 10)
xgb_grid1 <- xgb_grid[grid_ix,]

xgb_train_control <- trainControl(
  method = "repeatedcv",
  number  = 5,
  repeats = 2,
  verboseIter = FALSE,
  returnData = FALSE,
  allowParallel = TRUE
)

xgb_train <- train(
  x = as.matrix(train_df[,!names(train_df) %in% c("medv")]),
  y = train_df$medv,
  objective = "reg:linear",
  trControl = xgb_train_control,
  tuneGrid = xgb_grid1,
  method = "xgbTree"
)

#get best parameters
head(xgb_train$results[with(xgb_train$results, order(RMSE)),],1)


#predicting
pred <- predict(xgb_train, newdata = test_df)
mean((pred-test_df$medv)^2)

plot(test_df$medv, pred, col = "red")
abline(0,1,col="blue")

names <- names(train_df)[! names(train_df) %in% c("medv")]
importanceMatrix <- xgb.importance(names, model = xgb_train$finalModel)
xgb.plot.importance(importanceMatrix[1:10,])
