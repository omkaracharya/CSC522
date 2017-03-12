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
  
  #Initializing all the data points to cluster 1
  clusterID <- 1
  newClusterAssignment <- rep(1, nrow(data.df))

  #All the data points as a list  
  clusterPoints <- list(data.df)

  #Cluster SSE value
  clusterSSE <- list(sum((data.df[,2:3] - colMeans(data.df[,2:3]))^2))
  
  #Loop until k clusters are not found
  while(length(clusterPoints) < k){
    #Remove cluster with maxSSE from the list of clusters
    maxSSEClusterID <- which.max(clusterSSE)
    clusterToBeSplitted <- clusterPoints[[maxSSEClusterID]]
    #Remove those points temporarily, will be placing back at the end of loop
    clusterPoints[[maxSSEClusterID]] <- NULL
    clusterSSE[[maxSSEClusterID]] <- NULL

    #Initializing Total SSE value to INT_MAX
    totSSE <- .Machine$integer.max
    
    #For 1 to trials.max: perform kmeans()
    for(i in 1:trials.max){
      #k = 2 for kmeans
      currentTrial <- kmeans(clusterToBeSplitted[,2:3], 2)
      #Select two clusters for bisection
      if(currentTrial$totss < totSSE){
        #Select the final model with the lowest SSE
        finalModel <- currentTrial
      }
    }
    
    #Add these two clusters to the list of clusters
    #Adding the first cluster
    clusterPoints <- append(clusterPoints, list(clusterToBeSplitted[finalModel$cluster == 1,]))
    #Adding the second cluster  
    clusterPoints <- append(clusterPoints, list(clusterToBeSplitted[finalModel$cluster == 2,]))
    #Adding SSE value
    clusterSSE <- append(clusterSSE, finalModel$withinss)
  }
  
  #Generating final cluster assignment result
  for(i in 1:length(clusterPoints)){
    newClusterAssignment[clusterPoints[[i]][,1]] <- i
  }
  
  #Creating the result to be returned
  answer <- list()
  #All the points with cluster assignments
  answer[[1]] <- newClusterAssignment
  #The SSE value of the clusters
  answer[[2]] <- unlist(clusterSSE)
  answer
}

# Write code for comparing result from bisecting kmeans here - Part b
kmeans_comparison <- function(data.df, result, k){
  #centroids for kmeans
  centroids <- data.df[c(265, 288, 278, 247, 210), 2:3]
  #kmeans algorithm
  km <- kmeans(data.df[,2:3], centers = centroids)

  #Plot for comparing two cluster assignments
  #par(mfrow = c(1, 2))
  #plot(data.df[, -1], col = brewer.pal(k, "Set3")[km$cluster], pch = '.',cex = 3)
  #plot(data.df[, -1], col = brewer.pal(k, "Set3")[result[[1]]], pch = '.',cex = 3)
  #par(mfrow = c(1, 1))

  print("Kmeans Cluster Assignment:")
  print(km$cluster)
  
  print("Bisecting Kmeans Cluster Assignment:")
  print(result[[1]])

  print("SSE for Kmeans:")
  print(sum(km$withinss))

  print("SSE for Bisecting Kmeans:")
  print(sum(result[[2]]))
  
  print("Kmeans Cluster Sizes:")
  print(km$size)
  print("Bisecting Kmeans Cluster Sizes:")
  print(ftable(result[[1]])[1:5])
  print("Difference between Kmeans SSE and Bisecting Kmeans SSE:")
  print(abs(sum(result[[2]]) - sum(km$withinss)))
  
  sum(result[[1]] == km$cluster)

#  Compares two clusterings based on a number of parameters  
#  install.packages('fpc')
#  library(fpc)
#  d <- dist(data.df)
#  cluster.stats(d, result[[1]], km$cluster)
  
  
}

# Don't edit anything beyond this line
# Please note, TA will test with different configurations of trails.max and k.
k=5
result <- bisectingkmeans(data.df, trials.max = 25 , k)
plot(data.df[, -1], col = brewer.pal(k, "Set3")[result[[1]]], pch = '.',
     cex = 3)

kmeans_comparison(data.df, result, k)