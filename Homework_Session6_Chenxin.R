#Homework| Section 6 Unsupervised Learning
# Chenxin

#-------------------------Exercise 3 Chap.10---------------------------------------------------

# 1. In this problem, you will perform K-means clustering manually, with K = 2, 
# on a small example with n = 6 observations and p = 2 features. The observations are as follows.

set.seed(1)
x = cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))


#(a) Plot the observations
plot(x[,1], x[,2])


# (b) Randomly assign a cluster label to each observation. You can use the sample() command in R to do this. 
# Report the cluster labels for each observation.

labels = sample(2, nrow(x), replace=T)
labels

# (c) Compute the centroid for each cluster.
centroid1 = c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
centroid2 = c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
centroid1
centroid2

# (d) Assign each observation to the centroid to which it is closest, in terms of Euclidean distance. 
# Report the cluster labels for each observation.

euclid = function(a, b) {
  return(sqrt((a[1] - b[1])^2 + (a[2]-b[2])^2))
}
assign_labels = function(x, centroid1, centroid2) {
  labels = rep(NA, nrow(x))
  for (i in 1:nrow(x)) {
    if (euclid(x[i,], centroid1) < euclid(x[i,], centroid2)) {
      labels[i] = 1
    } else {
      labels[i] = 2
    }
  }
  return(labels)
}
labels = assign_labels(x, centroid1, centroid2)
labels

# (e) Repeat (c) and (d) until the answers obtained stop changing

last_labels = rep(-1, 6)
while (!all(last_labels == labels)) {
  last_labels = labels
  centroid1 = c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
  centroid2 = c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
  print(centroid1)
  print(centroid2)
  labels = assign_labels(x, centroid1, centroid2)
}

labels

# (f) In your plot from (a), color the observations according to the cluster labels obtained.
plot(x[,1], x[,2], col=(labels+1), pch=20, cex=2)
points(centroid1[1], centroid1[2], col=2, pch=4)
points(centroid2[1], centroid2[2], col=3, pch=4)


#-------------------------Exercise 8 Chap.10---------------------------------------------------

# In Section 10.2.3, a formula for calculating PVE was given in Equation 10.8. 
# We also saw that the PVE can be obtained using the sdev output of the prcomp() function.
# On the USArrests data, calculate PVE in two ways:

library(ISLR)
set.seed(1)

# (a) Using the sdev output of the prcomp() function, as was done in Section 10.2.3.

pr.out = prcomp(USArrests, center=T, scale=T)
pr.var = pr.out$sdev^2
pve = pr.var / sum(pr.var)
pve


# (b) By applying Equation 10.8 directly. That is, use the prcomp() function to compute the principal component loadings. 
# Then, use those loadings in Equation 10.8 to obtain the PVE.

loadings = pr.out$rotation
pve2 = rep(NA, 4)
dmean = apply(USArrests, 2, mean)
dsdev = sqrt(apply(USArrests, 2, var))
dsc = sweep(USArrests, MARGIN=2, dmean, "-")
dsc = sweep(dsc, MARGIN=2, dsdev, "/")
for (i in 1:4) {
  proto_x = sweep(dsc, MARGIN=2, loadings[,i], "*")
  pc_x = apply(proto_x, 1, sum)
  pve2[i] = sum(pc_x^2)
}
pve2 = pve2/sum(dsc^2)
pve2

#-------------------------Exercise 9 Chap.10---------------------------------------------------

# Consider the USArrests data. We will now perform hierarchical clustering on the states.

# (a) Using hierarchical clustering with complete linkage and Euclidean distance, cluster the states.

hc.complete = hclust(dist(USArrests), method="complete")
plot(hc.complete)

# (b) Cut the dendrogram at a height that results in three distinct clusters. Which states belong to which clusters?

cutree(hc.complete, 3)

# (c) Hierarchically cluster the states using complete linkage and Euclidean distance, 
# after scaling the variables to have standard deviation one.

dsc = scale(USArrests)
hc.s.complete = hclust(dist(dsc), method="complete")
plot(hc.s.complete)

# (d) What effect does scaling the variables have on the hierarchical clustering obtained? In your opinion, 
# should the variables be scaled before the inter-observation dissimilarities are computed?
# Provide a justification for your answer.

cutree(hc.s.complete, 3)

table(cutree(hc.s.complete, 3))
# 1  2  3 
# 8 11 31 

table(cutree(hc.s.complete, 3), cutree(hc.complete, 3))

#    1  2  3
# 1  6  2  0
# 2  9  2  0
# 3  1 10 20


#-------------------------Exercise 10 Chap.10---------------------------------------------------

# In this problem, you will generate simulated data, and then perform PCA and K-means clustering on the data.

# (a) Generate a simulated data set with 20 observations in each of three classes (i.e. 60 observations total), and 50 variables.

set.seed(2)
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1

# (b) Perform PCA on the 60 observations and plot the first two principal component score vectors. 
# Use a different color to indicate the observations in each of the three classes. 
# If the three classes appear separated in this plot, then continue on to part (c). 
# If not, then return to part (a) and modify the simulation so that there is greater separation between the three classes. 
# Do not continue to part (c) until the three classes show at least some separation in the first two principal component score vectors.

pca.out = prcomp(x)
summary(pca.out)

pca.out$x[,1:2]

plot(pca.out$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=19) 

# (c) Perform K-means clustering of the observations with K = 3. How well do the clusters that you obtained in K-means clustering compare to the true class labels?

km.out = kmeans(x, 3, nstart=20)

table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))
#    1  2  3
# 1  0 20  0
# 2  0  0 20
# 3 20  0  0

# (d) Perform K-means clustering with K = 2. Describe your results.

km.out = kmeans(x, 2, nstart=20)

km.out$cluster

# (e) Now perform K-means clustering with K = 4, and describe your results.

km.out = kmeans(x, 4, nstart=20)

km.out$cluster

# (f) Now perform K-means clustering with K = 3 on the first two principal component score vectors, rather than on the raw data.
# That is, perform K-means clustering on the 60 × 2 matrix of which the first column is the first principal component score vector, 
# and the second column is the second principal component score vector. Comment on the results.

km.out = kmeans(pca.out$x[,1:2], 3, nstart=20)

table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))
#    1  2  3
# 1  0  0 20
# 2  0 20  0
# 3 20  0  0

# (g) Using the scale() function, perform K-means clustering with K = 3 on the data after scaling each variable to have standard deviation one. 
# How do these results compare to those obtained in (b)? Explain.

km.out = kmeans(scale(x), 3, nstart=20)

km.out$cluster

# Poorer results than (b): the scaling of the observations effects the distance between them.