library(purrr)
library(cluster) 
library(gridExtra)
library(grid)
library(factoextra)
library(NbClust)


set.seed(123)

# function to calculate total intra-cluster sum of square 
iss <- function(k) {
    kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10

iss_values <- map_dbl(k.values, iss)

df = data.frame(k = k.values, iss = iss_values)

ggplot(df, aes(k, iss))+
    geom_point()+
    geom_line()+
    scale_x_continuous(breaks = seq(0,10,1))



# Silhouette method

k2 <- kmeans(customer_data[,3:5],2,iter.max=100,nstart=100,algorithm="Lloyd" )
s2<-plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")), xlab = "Silhoutte width", main = "Silhouette Plot")

k3 <- kmeans(customer_data[,3:5],3,iter.max=100,nstart=100,algorithm="Lloyd" )
s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")), xlab = "Silhoutte width", main = "Silhouette Plot")

k4 <- kmeans(customer_data[,3:5],4,iter.max=100,nstart=100,algorithm="Lloyd" )
s4<-plot(silhouette(k4$cluster,dist(customer_data[,3:5],"euclidean")), xlab = "Silhoutte width", main = "Silhouette Plot")

k5 <- kmeans(customer_data[,3:5],5,iter.max=100,nstart=100,algorithm="Lloyd" )
s5<-plot(silhouette(k5$cluster,dist(customer_data[,3:5],"euclidean")), xlab = "Silhoutte width", main = "Silhouette Plot")

k6 <- kmeans(customer_data[,3:5],6,iter.max=100,nstart=100,algorithm="Lloyd" )

s6<-plot(silhouette(k6$cluster,dist(customer_data[,3:5],"euclidean")), xlab = "Silhoutte width", main = "Silhouette Plot")

k7 <- kmeans(customer_data[,3:5],7,iter.max=100,nstart=100,algorithm="Lloyd" )
s7<-plot(silhouette(k7$cluster,dist(customer_data[,3:5],"euclidean")), xlab = "Silhoutte width", main = "Silhouette Plot")


k8 <- kmeans(customer_data[,3:5],8,iter.max=100,nstart=100,algorithm="Lloyd" )
s8<-plot(silhouette(k8$cluster,dist(customer_data[,3:5],"euclidean")), xlab = "Silhoutte width", main = "Silhouette Plot")


k9 <- kmeans(customer_data[,3:5],9,iter.max=100,nstart=100,algorithm="Lloyd" )
s9<-plot(silhouette(k9$cluster,dist(customer_data[,3:5],"euclidean")), xlab = "Silhoutte width", main = "Silhouette Plot")

k10 <- kmeans(customer_data[,3:5],10,iter.max=100,nstart=100,algorithm="Lloyd" )
s10<-plot(silhouette(k10$cluster,dist(customer_data[,3:5],"euclidean")), xlab = "Silhoutte width", main = "Silhouette Plot")






#FVIZ_NBCLUST FOR APPROPRIATE NUMBER OF CLUSTERS 

fviz_nbclust(customer_data[,3:5], kmeans, method = "wss")


fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")

fviz_nbclust(customer_data[,3:5], kmeans, method = "gap_stat")


# GAP STATISTIC

set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)


#PRINCIPAL COMPONENT ANALYSIS


pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)


pcclust$rotation[,1:2]


#Plotting clusters

plot(customer_data$Annual.Income..k.., customer_data$Spending.Score..1.100., col = as.factor(k6$cluster), pch = 19)

ggplot(customer_data, aes(Annual.Income..k.., Spending.Score..1.100., col=as.factor(k6$cluster) ))+
    geom_point(stat = "identity",pch=19)+
    scale_color_discrete(name="Legend",
                         breaks=c("1", "2", "3", "4", "5","6"),
                         labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
    ggtitle("Segments of Customers", subtitle = "Using K-means Clustering")


kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}
digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters
plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))





