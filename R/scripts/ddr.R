install.packages("kmeans.ddR")
library(kmeans.ddR)

a <- dmapply(function(x) { x }, rep(3,5))
b <- dlist(1,2,3,4,5,nparts=1L)
