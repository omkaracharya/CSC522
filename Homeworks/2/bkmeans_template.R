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
  
}

# Write code for comparing result from bisecting kmeans here - Part b
kmeans_comparison <- function(data.df, result, k){
  
}

# Don't edit anything beyond this line
# Please note, TA will test with different configurations of trails.max and k.
k=5
result <- bisectingkmeans(data.df, iter.max = 25 , k)
plot(data.df[, -1], col = brewer.pal(k, "Set3")[result[[1]]], pch = '.',
     cex = 3)

kmeans_comparison(data.df, result, k)

