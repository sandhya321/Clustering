

RCDF <- read.csv("home/Cust_Spend_Data.csv", header=TRUE)
View(RCDF)

d.euc <- dist(x=RCDF[,3:7], method = "euclidean") 
d.euc


clus1 <- hclust(d.euc, method = "average")
plot(clus1, labels = as.character(RCDF[,2]))

## scale function standardizes the values
scaled.RCDF <- scale(RCDF[,3:7])
head(scaled.RCDF, 10)
d.euc
d.euc <- dist(x=scaled.RCDF, method = "euclidean") 
clus2 <- hclust(d.euc, method = "average")
plot(clus2, labels = as.character(RCDF[,2]))
rect.hclust(clus2, k=4, border="red")
clus2$height

View(RCDF)
## profiling the clusters
RCDF$Clusters <- cutree(clus2, k=3)
aggr = aggregate(RCDF[,-c(1,2, 8)],list(RCDF$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(RCDF$Clusters)),
                            aggr[,-1])

View(clus.profile)


## K Means Clustering


KRCDF <- read.csv("/home/Cust_Spend_Data.csv", header=TRUE)
View(KRCDF)
## scale function standardizes the values
scaled.RCDF <- scale(KRCDF[,3:7])

View(scaled.RCDF)
class(scaled.RCDF)

## Identifying the optimal number of clusters form WSS

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(scaled.RCDF, nc=5)

## Identifying the optimal number of clusters
##install.packages("NbClust")

library(NbClust)
?NbClust

set.seed(1234)
nc <- NbClust(KRCDF[,c(-1,-2)], min.nc=2, max.nc=4, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
          xlab="Numer of Clusters", ylab="Number of Criteria",
          main="Number of Clusters Chosen by 26 Criteria")


?kmeans
kmeans.clus = kmeans(x=scaled.RCDF, centers = 3, nstart = 25)
kmeans.clus

tmp <- as.data.frame(scaled.RCDF)
tmp$Clusters <- kmeans.clus$cluster
View(tmp)
## plotting the clusters
##install.packages("fpc")
library(fpc)
plotcluster(scaled.RCDF, kmeans.clus$cluster)

library(cluster)
?clusplot
clusplot(scaled.RCDF, kmeans.clus$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1)

## profiling the clusters
KRCDF$Clusters <- kmeans.clus$cluster
View(KRCDF)
aggr = aggregate(KRCDF[,-c(1,2, 8)],list(KRCDF$Clusters),mean)
clus.profile <- data.frame( Cluster=aggr[,1],
                            Freq=as.vector(table(KRCDF$Clusters)),
                            aggr[,-1])

View(clus.profile)

