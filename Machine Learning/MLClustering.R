library(ggplot2)
library(gridExtra)
library(cluster)
library(factoextra)

# Load the dataset from file
LasVegas <- read.csv("/Users/robinxyuan/Downloads/LasVegas.csv", header = TRUE, sep = ";")
# Attach variables into variable area from column head
attach(LasVegas)

ggplot(LasVegas, aes(Nr..hotel.reviews, Helpful.votes, color = 'red')) + geom_point()

vegasSet <- LasVegas[, c("Nr..hotel.reviews", "Helpful.votes")]
# vegasSet <- vegasSet[sample(nrow(vegasSet), 300, replace=F),]
# Standardize the data
vegasSet <- scale(vegasSet)

# Calculate the euclidean distance
distance <- get_dist(vegasSet, method = 'euclidean')
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Find the optimal k value
set.seed(123)
fviz_nbclust(vegasSet, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)
fviz_nbclust(vegasSet, kmeans, method = "silhouette")
fviz_nbclust(vegasSet, kmeans, method = "gap_stat")

# K = 2 Clustering
k2 <- kmeans(vegasSet, centers = 2, nstart = 100)
print(k2)
k2$cluster <- as.factor(k2$cluster)

fviz_cluster(k2, data = vegasSet)

# K = 3, 4, 5 CLustering
k3 <- kmeans(vegasSet, centers = 3, nstart = 100)
k4 <- kmeans(vegasSet, centers = 4, nstart = 100)
k5 <- kmeans(vegasSet, centers = 5, nstart = 100)

p1 <- fviz_cluster(k2, geom = "point", data = vegasSet) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = vegasSet) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = vegasSet) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = vegasSet) + ggtitle("k = 5")

# Plot the cluster
fviz_cluster(k2, data = vegasSet,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid",
             ggtheme = theme_minimal()
)

# Combine the 4 plots
grid.arrange(p1, p2, p3, p4, nrow = 2)

k2$tot.withins
plot(vegasSet, col=(k2$cluster), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)
