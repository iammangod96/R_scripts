#This is for learning exploratory data analysis
path <- "C:/Users/manish.sharma/Downloads/mlwithr"
setwd(path)
list.files()
usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)
str(usedcars)
summary(usedcars)

#variable information graphs
par(mfrow=c(2,2))
boxplot(usedcars$price, main="prices", ylab="price ($)")
boxplot(usedcars$mileage, main="Mileage", ylab="Miles")
hist(usedcars$price, main="Prices", xlab = "price ($)" , breaks=20)
hist(usedcars$mileage, main="Mileage", xlab="Miles")

#bivariate relationship graphs
par(mfrow=c(1,1))
plot(x=usedcars$mileage, y = usedcars$price, xlab="Miles", ylab="Price($)")
cor(usedcars$mileage, usedcars$price)
library("gmodels")
usedcars$conservative <- usedcars$color %in% c("Black","Gray","Silver","White")
table(usedcars$conservative)
CrossTable(y = usedcars$conservative, x = usedcars$model)
