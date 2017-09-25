#This dataset is using knn nearest neighbour classification
path <- "C:/Users/manish.sharma/Downloads/mlwithr"
setwd(path)
cancer <- read.csv("wisc_bc_data.csv")
str(cancer)
cancer <- cancer[-1]
table(cancer[,1])
typeof(cancer$diagnosis)
#converting diagnosis(target) column to factor
cancer$diagnosis <- factor(cancer$diagnosis, levels = c("B","M"), labels = c("Benign","Malignant"))

round(prop.table(table(cancer$diagnosis))*100,digits=1)
summary(cancer[c("radius_mean","area_mean","smoothness_mean")])

#graphs
par(mfrow=c(3,1))
hist(cancer$radius_mean, main="Radius_Mean", xlab="Radius", breaks=20)
hist(cancer$area_mean, main="Area_Mean", xlab="Area", breaks=20)
hist(cancer$smoothness_mean, main="Smoothness_Mean", xlab="Smoothness", breaks=20)
range(cancer$radius_mean)
range(cancer$area_mean)
range(cancer$smoothness_mean)

#normalize function
normalize <- function(x)
{
  return ((x-min(x))/(max(x)-min(x)))
}
cancer_n <- as.data.frame(lapply(cancer[2:31],normalize))
summary(cancer_n$radius_mean)

#train and test seperation for normalized data
train_n = cancer_n[1:469,]
test_n = cancer_n[469:569,]
summary(train_n$radius_mean)
summary(test_n$radius_mean)
train_labels <- cancer[1:469,1]
test_labels <- cancer[469:569,1]

#knn from "class" package
library("class")
cancer_pred <- knn(train = train_n, test = test_n, cl = train_labels, k = 21)

#evaluation of knn
library("gmodels")
CrossTable(x = test_labels, y = cancer_pred, prop.chisq = FALSE)

#z-score transformed data
cancer_z <- as.data.frame(scale(cancer[-1]))
summary(cancer_z$radius_mean)

#train and test seperation of z-score transformed data
train_z <- cancer_z[1:469,]
test_z <- cancer_z[469:569,]
train_z_labels <- cancer[1:469,1]
test_z_labels <- cancer[469:569,1]

#knn and evaluation on z-score transformed data
cancer_z_pred <- knn(train=train_z, test=test_z, cl=train_z_labels, k = 1)
CrossTable(x= test_z_labels, y = cancer_z_pred, prop.chisq = FALSE)

