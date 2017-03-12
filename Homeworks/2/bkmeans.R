######
# Bisecting K-Means
#####
rm(list = ls(all = T))

library(RColorBrewer)

# set seed to ensure consistent results
set.seed(100)

# Read data file
# When	submitting, ensure that the path to this file is just "hw2-data.csv" 
# and doesn't point	to a path on your machine 
data.df <- read.csv('hw2-data.csv')

# TODO: Implement bisecting k means.
# Input:
# data.df: data frame based on hw2-data.csv
# trials.max: Max. number of trials for kmeans, as per Algorithm 8.2 in textbook. 
# You are allowed to use the pre-defined kmeans() function.
# k: Number of clusters to find in bisecting k-means

# Output:
# Your output/function return value will be a list containing 2 elements
# first element of the list is a vector containing cluster assignments (i.e., values 1 to k assigned to each data point)
# second element of the list is a vector containing SSE of each cluster
# Additional Information:
# When identifying which cluster to split, choose the one with maximum SSE
# When performing kmeans, pick two random points the cluster with largest SSE as centers at every iteration. 
# Be mindful that there might be duplicates in the data.
# terminating condition: when k clusters have been found

bisectingkmeans <- function(data.df, trials.max, k){
  # start your implementation here
  clusterId <- 1
  clusterAssignment <- rep(1, nrow(data.df))
  clusters <- list(data.df)
  clusterSse <- list(sum((data.df[,2:3] - colMeans(data.df[,2:3]))^2))
  
  while(length(clusters) < k){
    #Remove cluster with maxSSE from the list of clusters
    maxSseClusterId <- which.max(clusterSse)
    clusterToSplit <- clusters[[maxSseClusterId]]
    clusters[[maxSseClusterId]] <- NULL
    clusterSse[[maxSseClusterId]] <- NULL
    
    kmtotsse <- .Machine$integer.max
    #For 1 to trials.max: perform kmeans()
    for(i in 1:trials.max){
      kmtrial <- kmeans(clusterToSplit[,2:3], 2)
      #Select two clusters for bisection
      if(kmtrial$totss < kmtotsse){
        km <- kmtrial
      }
    }
    
    #Add these two clusters to the list of clusters
    clusters <- append(clusters, list(clusterToSplit[km$cluster == 1,]))
    clusters <- append(clusters, list(clusterToSplit[km$cluster == 2,]))
    clusterSse <- append(clusterSse, km$withinss)
  }
  
  
  for(i in 1:length(clusters)){
    clusterAssignment[clusters[[i]][,1]] <- i
  }
  
  result <- list()
  result[[1]] <- clusterAssignment
  result[[2]] <- unlist(clusterSse)
  result
}

# Write code for comparing result from bisecting kmeans here - Part b
kmeans_comparison <- function(data.df, result, k){
  centroids <- data.df[c(210, 247, 265, 278, 288), 2:3]
  km <- kmeans(data.df[,2:3], centers = centroids)
  par(mfrow = c(1, 2))
  plot(data.df[, -1], col = brewer.pal(k, "Set3")[km$cluster], pch = '.',cex = 3)
  plot(data.df[, -1], col = brewer.pal(k, "Set3")[result[[1]]], pch = '.',cex = 3)
  par(mfrow = c(1, 1))
  abs(sum(result[[2]]) - sum(km$withinss))
}

# Don't edit anything beyond this line
# Please note, TA will test with different configurations of trails.max and k.
k=5
result <- bisectingkmeans(data.df, trials.max = 25 , k)
plot(data.df[, -1], col = brewer.pal(k, "Set3")[result[[1]]], pch = '.',
     cex = 3)

kmeans_comparison(data.df, result, k)

