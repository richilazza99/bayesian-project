library('salso')

s <- read.csv("outp", header=FALSE)
sm=as.matrix(s)
est <- salso(sm,maxNClusters = 10, nCores=1)
summ <- summary(est)

plot(summ, type="heatmap")

s_best=summ[["estimate"]]
s_out <- matrix(s_best, ncol=1, byrow=T)
write.csv(s_out, "~/bayesian-project/true_dataset_s_binder.csv")
s_out

library(maptools)
library(ggplot2)
library(tidyverse)

cluster.beta=read.csv("outputs/s_binder.csv")  #mettere matrice cluster allocation dei beta
dataset=read.csv("data/Fecondità.csv")   #carica il dataset completo

cluster.beta$region=sort(unique(dataset$Territorio))
colnames(cluster.beta)[2]="cluster"
cluster.beta$cluster=factor(cluster.beta$cluster)

cluster.beta$region[75]="Trento"
cluster.beta$region[74]="Bolzano-Bozen"
cluster.beta$region[98]="Aosta"
cluster.beta$region[79]="Reggio Emilia"
cluster.beta$region[34]="Forli'"
cluster.beta$region[78]="Reggio Calabria"

cluster.beta=cluster.beta[-c(9,13,27,30,45,47,56,73,81,101,104),c(2,3)]

italy_map <- map_data("italy")
italy_proj <- "+proj=aea +lat_1=38.15040684902542
+lat_2=44.925490198742295 +lon_0=12.7880859375"

mapdata=left_join(italy_map,cluster.beta,by='region')


map.cluster = ggplot(mapdata,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=cluster),color='black')
map.cluster