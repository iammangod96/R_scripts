rm(list = ls())


#LOADING DATASETS
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
dtrain <- xgb.DMatrix(data = train$data, label = train$label)
dtest <- xgb.DMatrix(data = test$data , label = test$label)


#training xgboost
watchlist <- list(train = dtrain , test = dtest)
bst <- xgb.train(data = dtrain, max.depth = 2, eta = 1,  nthread = 2, nround = 2, watchlist = watchlist,eval.metric = "error", eval.metric = "logloss" ,objective = "binary:logistic")
#max.depth = 1 : unerfitting
#max.depth = 3 : overfitting
#as eta increased from 0 to 1 error decreases
bst_linear <- xgb.train(data = dtrain,booster = "gblinear", max.depth = 2, eta = 1,  nthread = 2, nround = 2, watchlist = watchlist,eval.metric = "error", eval.metric = "logloss" ,objective = "binary:logistic")


#feature importance
imp_matrix <- xgb.importance(model = bst) #writing model is necessary otherwise error
xgb.plot.importance(imp_matrix)


#save model
xgb.save(bst, "xgboost1")
bst2 <- xgb.load("xgboost1")
