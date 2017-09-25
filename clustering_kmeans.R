rm(list = ls())


#loading libraries
library(cluster)
library(ggplot2)


#loading datasets
data(iris)
iris$Species <- as.numeric(iris$Species)
iris


#elbow method to find number of clusters
cost_df <- data.frame()
for(i in 1:100)
{
  kmeans <- kmeans(x = iris, centers = i, iter.max = 50)
  cost_df <- rbind(cost_df, cbind(i, kmeans$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")
ggplot(data = cost_df, aes(x = cluster, y = cost, group = 1)) +
  theme_bw(base_family = "Garamond") +
  geom_line(color = "darkgreen") + 
  theme(text = element_text(size = 20))+
  ggtitle("Reduction In Cost For Values of 'k'\n") +
  xlab("\nClusters") +
  ylab("Within-Cluster Sum of Squares\n")


#applying kmeans
kmeans <- kmeans(x = iris, centers = 5)


#plotting clusters
clusplot(iris, kmeans$cluster, color = TRUE, shade = TRUE, lines = 0, labels = 4)
