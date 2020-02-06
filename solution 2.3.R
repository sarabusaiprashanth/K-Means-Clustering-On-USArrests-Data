library(readr) 
(data("USArrests"))
names(USArrests)
View(USArrests)
attach(USArrests)
summary(USArrests)

#Murder          Assault         UrbanPop          Rape      
#Min.   : 0.800   Min.   : 45.0   Min.   :32.00   Min.   : 7.30  
#1st Qu.: 4.075   1st Qu.:109.0   1st Qu.:54.50   1st Qu.:15.07  
#Median : 7.250   Median :159.0   Median :66.00   Median :20.10  
#Mean   : 7.788   Mean   :170.8   Mean   :65.54   Mean   :21.23  
#3rd Qu.:11.250   3rd Qu.:249.0   3rd Qu.:77.75   3rd Qu.:26.18  
#Max.   :17.400   Max.   :337.0   Max.   :91.00   Max.   :46.00 
#As per my Knowledge Min and max values varies in all the variables and it requires
#scaling/Standardization on the data to plot. 


attributes(USArrests)

#Data Preparation 
USArrests <- na.omit(USArrests)
USArrests <- scale(USArrests) #Scaling 



state.names = row.names(USArrests)
barplot(Murder, names.arg = state.names, las = 2, ylab = "Murder Rate per 100,000", 
        main = "Murder Rate in the United States in 1973")

barplot(Assault, names.arg = state.names, las = 2, ylab = "Murder Rate per 100,000", 
        main = "Assault Rate in the United States in 1973")

barplot(UrbanPop, names.arg = state.names, las = 2, ylab = "Murder Rate per 100,000", 
        main = "UrbanPop Rate in the United States in 1973")

barplot(Rape, names.arg = state.names, las = 2, ylab = "Murder Rate per 100,000", 
        main = "Rape Rate in the United States in 1973")





hist(Murder)
hist(Assault)
hist(UrbanPop)
hist(Rape)

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

head(USArrests)

k2 <- kmeans(USArrests, centers = 2, nstart = 25)
k2
summary(k2)
str(k2)
fviz_cluster(k2, data = USArrests)


k3 <- kmeans(USArrests, centers = 3, nstart = 25)
k4 <- kmeans(USArrests, centers = 4, nstart = 25)
k5 <- kmeans(USArrests, centers = 5, nstart = 25)
k6 <- kmeans(USArrests, centers = 5, nstart = 25)
k7 <- kmeans(USArrests, centers = 5, nstart = 25)
k8 <- kmeans(USArrests, centers = 5, nstart = 25)
k9 <- kmeans(USArrests, centers = 5, nstart = 25)
k10 <- kmeans(USArrests, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = USArrests) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = USArrests) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = USArrests) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = USArrests) + ggtitle("k = 5")
p5 <- fviz_cluster(k6, geom = "point",  data = USArrests) + ggtitle("k = 6")
p6 <- fviz_cluster(k7, geom = "point",  data = USArrests) + ggtitle("k = 7")
p7 <- fviz_cluster(k8, geom = "point",  data = USArrests) + ggtitle("k = 8")
p8 <- fviz_cluster(k9, geom = "point",  data = USArrests) + ggtitle("k = 9")
p9 <- fviz_cluster(k10, geom = "point",  data = USArrests) + ggtitle("k = 10")



library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow = 2)

#Detecing Optimal Number of clusters

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(USArrests, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 10
k.values <- 1:10

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)


#optimal number of clusters 
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


























