library('salso')

s <- read.csv("outp", header=FALSE)   #import the cluster allocations of the model on stan
sm=as.matrix(s)
est <- salso(sm,maxNClusters = 10, nCores=1)  #fix the cluster allocation minimizing the VI loss
summ <- summary(est)

plot(summ, type="heatmap")                              

s_best=summ[["estimate"]]
s_out <- matrix(s_best, ncol=1, byrow=T)       
write.csv(s_out, "~/bayesian-project/true_dataset_s_binder.csv")   # save the cluster allocations found in a csv file
s_out

