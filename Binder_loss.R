if(!require('salso')) {
  install.packages('salso')
  library('salso')
}
s <- read.delim(choose.files(), header=F,',')
sm=as.matrix(s)
est <- salso(sm,"binder",maxNClusters = 10, nCores=1)
summ <- summary(est)
plot(summ, type="heatmap")
#plot(summ, type="mds")
#plot(summ, type="dendrogram")

s_best=summ[["estimate"]]
s_out <- matrix(s_best, ncol=1, byrow=T)
write.csv(s_out, "s_binder.csv")