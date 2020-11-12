# This is the Task 2 script field #
#NAME:Wenwei Cui #
#PROJECT:Unsupervised Learning-Iris dataset #

# Install required packages #
install.packages("dplyr")
install.packages("ggfortify")
install.packages("ggplot2")

# Load require libraries #
library(dplyr)
library(ggplot2)
library(ggfortify)

# Unsupervised Learning #
View(iris)
mydata = select(iris, c(1,2,3,4))

# WSS Plot Function #
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
# Wss plot to choose maximum clusters #
wssplot(mydata)
# Spotting the kink in the curve to choose the optimal cluster #

# K-Means clusters #
KM = kmeans(mydata, 2)

# Evaluate cluster Analysis #
# Cluster plot #
autoplot(KM, mydata, frame=TRUE)

# Cluster Centers #
KM$centers
