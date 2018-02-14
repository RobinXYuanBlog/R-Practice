#-------------CORRECTION TD12
#---------EX01
#Complete
d = as.dist(matrix(c(0, 0.3, 0.4, 0.7, 
                     0.3, 0, 0.5, 0.8,
                     0.4, 0.5, 0.0, 0.45,
                     0.7, 0.8, 0.45, 0.0), nrow=4))
plot(hclust(d, method="complete"))

#single
plot(hclust(d, method="single"))
#c) (1,2), (3,4)
#d) (1, 2, 3), (4)

#---------EX02
x = cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 2, 2, 0))
plot(x[,1], x[,2])
set.seed(1)
labels = sample(2, nrow(x), replace=T)
#1 1 2 2 1 2
#c)
centroid1 = c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
centroid2 = c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
plot(x[,1], x[,2], col=(labels+1), pch=4, cex=1)
points(centroid1[1], centroid1[2], col=2, pch=20,cex=2)
points(centroid2[1], centroid2[2], col=3, pch=20,cex=2)
#d)
euclid = function(a, b) {
  return(sqrt((a[1] - b[1])^2 + (a[2]-b[2])^2))
}
assign_labels = function(x, centroid1, centroid2) {
  labels = rep(NA, nrow(x))
  for (i in 1:nrow(x)) {
    if (euclid(x[i,], centroid1) < euclid(x[i,], centroid2)) {
      labels[i] = 1
    } else {
      labels[i] = 2
    }
  }
  return(labels)
}
labels = assign_labels(x, centroid1, centroid2)
#e)
last_labels = rep(0, 6)
while (!all(last_labels == labels)) {
  last_labels = labels
  centroid1 = c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
  centroid2 = c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
  print(centroid1)
  print(centroid2)
  labels = assign_labels(x, centroid1, centroid2)
}

#f
plot(x[,1], x[,2], col=(labels+1), pch=4, cex=1)
points(centroid1[1], centroid1[2], col=2, pch=20, cex=2)
points(centroid2[1], centroid2[2], col=3, pch=20, cex=2)
centroid1
centroid2

#----------PRACTICAL APPLICATION
# K-Means Clustering
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
km.out=kmeans(x,2,nstart=20)
km.out$cluster #The K-means clustering perfectly separated the observations into two clus- ters even though we did not supply any group information to kmeans().
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)
#If there were more than two variables performing ACP would be a good idea.
set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)
#When K = 3, K-means clustering splits up the two clusters.
km.out$tot.withinss #The total within-cluster sum of squares, we seek to minimize (see the lecture)
#Within cluster variation in the lecture.

#------ Hierarchical Clustering
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2) 
#"complete" and "average" linkage generally separate the observations into their truly groups. 
#However, "single" linkage isolates one point.
cutree(hc.single, 4)
xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")