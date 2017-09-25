rm(list=ls())

#loadi
#loading libraries
#install.packages("arules")
library(arules)

#loading dataset
gr <- read.transactions("C:/Users/manish.sharma/Downloads/mlwithr/groceries.csv", sep = ',')


#data profiling
summary(gr)
inspect(gr[12])
itemFrequency(gr[,1:12])
itemFrequencyPlot(gr, topN = 10)
image(sample(gr,500))

myrules <- apriori(data = gr, parameter = list(support = 0.006,confidence = 0.25 , minlen =  2) )
myrules
inspect(myrules[1:5])
summary(myrules)
myrules_sorted <- sort(myrules, by = 'lift')
inspect(myrules_sorted[1:5])

berryrules <- subset(myrules, items %in% "berries")
inspect(berryrules)

write(myrules, file = "myrules.csv", sep=",", row.names = F, quote = T)
