# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle","NbClust"))
install.packages(c("cluster", "rattle","NbClust"))

library(cluster)
library(rattle)
library(NbClust)


# Now load the data and look at the first few rows
# Let us examine the data structure
str(wine)
#The wine dataset contains 1 categorical variable (label) and 
#13 numerical variables. 
data(wine, package="rattle")
head(wine)



# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
# The 13 numerical variables in the dataset are not scaled, 
# We use the scale function in R for scaling and centering data
# We assign the dataset as a training dataset.

data.wine.train <- scale(wine[-1])

#Now let us look at the descriptive statistics for the data
summary(data.wine.train)

#The data is centered around the mean (average) and scaled


# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data.wine.train, nc=15, seed=1234){
	              wss <- (nrow(data.wine.train)-1)*sum(apply(data.wine.train,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data.wine.train, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", 
		           xlab="Number of Clusters",
		           ylab="Within groups sum of squares")}

wssplot(data.train)

# Exercise 2:
#   * How many clusters does this method suggest?
##  According to the graph, where the graph bends, 
##  we find the best number of clusters is 3

#   * Why does this method work? What's the intuition behind it?

##  Since the original data set also contains 3 classes (levels),
##  it makes perfect sense to have 3 clusters

#   * Look at the code for wssplot() and figure out how it works


# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(data.wine.train, min.nc=2, max.nc=15, method="kmeans")

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

##  The bar plot suggests 3 distinct clusters.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(data.wine.train, 3)

fit.km

##  The result shows information about cluster means, clustering vector, 
##  sum of square by cluster and available components.

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

## Because the original data set wine also has 3 classes, 
## it is reasonable if we compare these classes with 3 clusters fited by K-Means

confuseTable.km <- table(wine$Type, fit.km$cluster)

confuseTable.km

## From the resulting confusion table, we see only 6 samples are missed.


# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(data.wine.train, fit.km$cluster)

##  We can also use the plotcluster function from the fpc package to plot.
library(fpc)
plotcluster(data.wine.train, fit.km$cluster)

##  To answer the question if this is considered good clustering, 
##  from the clusplot and plotcluster resuts,
##  we can see the data is clustered very well, there is no collapse between clusters.
##  Based on this visualization, we can conclude that this is good clustering

##  Another way to determine if this is good clustering, is to use the use randIndex from flexclust 
##  to compare the two parititions - one from data set and one from result of clustering method.

library(flexclust)
randIndex(confuseTable.km)

##  The resulting Rand Index is 0.897, which is very close to 1, which indicates
##  that the K-means is a good method for clustering.

### References

### Choosing number of cluster in K-Means, http://stackoverflow.com/a/15376462/1036500
### K-means Clustering (from "R in Action"), http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/
