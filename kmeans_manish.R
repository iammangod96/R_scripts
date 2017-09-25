rm(list = ls())

#loading dataset
teens <- read.csv("C:/Users/manish.sharma/Downloads/mlwithr/snsdata.csv")


#data profiling
str(teens)
summary(teens) #NAs in gender,age
hist(teens$friends)


#remove age outliers
hist(teens$age)
teens$age <- ifelse(teens$age >= 14.5 & teens$age < 20, teens$age, NA)


#preprocessing
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender),1,0)
teens$no_gender <- ifelse(is.na(teens$gender),1,0)
aggregate(formula = age ~ gradyear,data = teens, FUN = mean, na.rm = T)
ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x,na.rm = T))
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
summary(ave_age)


#standardizing data
interests <- teens[5:40]
interest_z <- as.data.frame(lapply(interests, scale))


#training kmeans model
set.seed(5)
teen_clusters <- kmeans(interest_z, 5)
teen_clusters


#plot clusters
library(cluster)
library(fpc)
plotcluster(interest_z, teen_clusters$cluster)
