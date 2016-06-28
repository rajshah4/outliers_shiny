The app allows you to see the trade-offs on various types of outlier / anomaly detection algorithms. Outliers are marked with a star and cluster centers with an X.


Some things to try:
+ What is the effect of scaling the data?
+ How do outliers affect clustering and the identification of outliers?
+ Try making two clusters with different densities, how does that affect detection?
+ Try using the eyes or corner sample and place a point betweent the clusters.  Which algorithms identify the point as an outlier?
+ Try playing around with the smiley face or doughnut and understand how different algorithms identify points as outliers

Models used with corresponding R package in parenthesis:
+ Hierarchical Clustering (DMwR)
+ Kmeans (distance metrics from proxy)
  + Kmeans Euclidean Distance 
  + Kmeans Mahalanobis
  + Kmeans Manhattan
+ Fuzzy kmeans (all from fclust)
  + Fuzzy kmeans - Gustafson and Kessel 
  + Fuzzy k-medoids
  + Fuzzy k-means with polynomial fuzzifier
+ Local Outlier Factor (dbscan)
+ RandomForest (proximity from randomForest)
  + Isolation Forest (IsolationForest)
+ Autoencoder (Autoencoder)

Scaling function uses scale from base R 
 
Inspiration:
+ <a href="http://cs.stanford.edu/people/karpathy/convnetjs/" target=" blank">CovNetJS</a>
+ <a href="http://www.naftaliharris.com/blog/visualizing-k-means-clustering/" target=" blank">Visualizing K-Means Clustering</a>


