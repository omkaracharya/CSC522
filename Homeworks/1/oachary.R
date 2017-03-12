minMaxNormalization <- function(df){
  minMaxRange <- max(df) - min(df)
  normalizedDf <- 255 / (minMaxRange) * (df - min(df))
  normalizedDf
}

zScoreNormalization <- function(df, sd, mean){
  normalizedDf <- (df - mean) / sd
  normalizedDf
}

meanFunc <- function(dv){
  mean <- sum(dv) / length(dv)
  mean
}

sdFunc <- function(dv, mean){
  sd <- (dv - mean) ^ 2
  sd <- sum(sd) / (length(sd) - 1)
  sd <- sqrt(sd)
  sd
}

img.df <- read.csv("hw1-data.csv")
img.vec <- unlist(img.df)
img.mean <- meanFunc(img.vec)
img.stdDev <- sdFunc(img.vec, img.mean)
img.mm.vec <- minMaxNormalization(img.df)
img.zn.vec <- zScoreNormalization(img.df, img.stdDev, img.mean)