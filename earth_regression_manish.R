rm(list = ls())

#loading dataset
ins <- read.csv("C:/Users/manish.sharma/Downloads/mlwithr/insurance.csv")


#date profiling
str(ins)


#loading libraries
#install.packages("earth")
library("earth")
#The term "MARS" is trademarked and licensed to Salford Systems. In order to avoid trademark infringements, many open source implementations 
#of MARS are called "Earth"


ins_model <- earth(formula = charges ~ ., trace = T, data = ins)
summary(ins_model)
#RSq 0.7566907


ins_model2 <- earth(formula = charges ~ ., trace = T, data = ins, penalty = -1)
#plot(ins_model2)
summary(ins_model2)
#RSq 0.7567177


par(mfrow=c(2,1))

plot(lowess(ins$age, ins$charges), col = "blue", type="l")
points(ins$age, ins$charges, col = "green")

plot(lowess(ins$bmi, ins$charges), col = "blue", type="l")
points(ins$bmi, ins$charges, col = "green")

summary(ins$bmi)
