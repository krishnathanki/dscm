#install pre-requisites packages
install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")

#load required libraries
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
View(iris)

mydata = select(iris,c(1,2,3,4))

#wss plot to choose maximum number of clusters
wssplot(mydata)

#K-Means cluster
KM = kmeans(mydata,2)

#cluster plot
autoplot(KM,mydata,frame=TRUE)

#cluster centres
KM$centers

#wss plot function
#wssplot <- function(data, nc=15, seed=1234){
#  wss <- (nrow(data)-1)*sum(apply(data,2,var))
#  for (i in 2:nc){
#    set.seed(seed)
#    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
#  plot(1:nc, wss, type="b", xlab="Number of Clusters",
#       ylab="Within groups sum of squares")
#}