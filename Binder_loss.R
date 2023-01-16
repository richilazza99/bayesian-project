if(!require('salso')) {
  install.packages('salso')
  library('salso')
}
sm=as.matrix(s)
est <- salso(sm,"binder",maxNClusters = 7, nCores=1)
summ <- summary(est)
plot(summ, type="heatmap")
plot(summ, type="mds")
plot(summ, type="dendrogram")

s_best=summ[["estimate"]]
s_squared <- matrix(s_best, ncol=7, byrow=T)
s_squared
