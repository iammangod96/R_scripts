rm(list = ls())


#loading libraries
library(recommenderlab)
library(ggplot2)
set.seed(1)


#loading datasets
data("MovieLense")
MovieLense


#methods which we can apply
methods(class = class(MovieLense))


#computing similarity matrix
similarity_users <- similarity(MovieLense[1:4,], method = "cosine", which = "users")
image(as.matrix(similarity_users))


#different recommendation models
recommender_models <- recommenderRegistry$get_entries(data.class = "realRatingMatrix")
names(recommender_models)
lapply(recommender_models, "[[", "description")


#########################################################
#data profiling

#ratings
vector_ratings <- as.vector(MovieLense@data)
unique(vector_ratings)
vector_ratings <- vector_ratings[vector_ratings !=  0]
vector_ratings <- factor(vector_ratings)
qplot(vector_ratings)


#exploring views
views_per_movie <- colCounts(MovieLense)
table_views <- data.frame(movie = names(views_per_movie), views_per_movie)
table_views <- table_views[order(table_views$views_per_movie, decreasing = T),]


#exploring ratings
average_ratings <- colMeans(MovieLense)
qplot(average_ratings) + stat_bin(binwidth = 0.1)


#removing less viewed(<=100) movies
average_ratings_relevant <- average_ratings[views_per_movie > 100]
qplot(average_ratings_relevant) + stat_bin(binwidth =  0.1)


#visualising the matrix
image(MovieLense)
image(MovieLense[1:10,1:15])


#calculating top 1 percentile of users and movies
min_n_movies <- quantile(rowCounts(MovieLense), 0.99)
min_n_users <- quantile(colCounts(MovieLense), 0.99)
min_n_movies
min_n_users
image(MovieLense[rowCounts(MovieLense) > min_n_movies, colCounts(MovieLense) > min_n_users])



#########################################################

#Data preparation (min_n_movies > 50, min_n_users > 100)
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50, colCounts(MovieLense) > 100]
image(ratings_movies)


min_movies <- quantile(rowCounts(MovieLense), 0.98)
min_users <- quantile(colCounts(MovieLense), 0.98)


#normalizing
#Having users who give high (or low) ratings to all their movies might bias the results. We can remove this effect by normalizing the data in such a way that the average rating of each user is 0.

ratings_movies_norm <- normalize(ratings_movies)
image(ratings_movies_norm[rowCounts(ratings_movies_norm) > min_movies, colCounts(ratings_movies_norm) > min_users])


#binarizing data
ratings_movies_watched <- binarize(ratings_movies, minRating = 1)
image(ratings_movies_watched)


#train and test data
which_train <- sample(x = c(TRUE,FALSE), size = nrow(ratings_movies), replace = TRUE, prob = c(0.8,0.2))
rec_data_train <- ratings_movies[which_train,]
rec_data_test <- ratings_movies[!which_train,]


#recommendation engine model
rec_model <- Recommender(data = rec_data_train, method = "IBCF", parameter = list(k = 30))
rec_model
summary(rec_model)


n_recommend <- 6
rec_predicted <- predict(rec_model, newdata = rec_data_test, n = n_recommend)
rec_predicted
class(rec_predicted)
slotNames(rec_predicted)
rec_user_1 <- rec_predicted@items[[1]]
movies_user_1 <- rec_predicted@itemLabels[rec_user_1]
movies_user_1



