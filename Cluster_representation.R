library(maptools)
library(ggplot2)
library(tidyverse)

cluster.beta=read.csv("outputs/s_binder.csv")  # import  a vector of cluster allocation
dataset=read.csv("data/Fecondità.csv")         #import the dataset

cluster.beta$region=sort(unique(dataset$Territorio))       # identify the provinces in the dataset
colnames(cluster.beta)[2]="cluster"
cluster.beta$cluster=factor(cluster.beta$cluster)

#Match the names in the dataset to the one in the maptools library 
cluster.beta$region[75]="Trento"
cluster.beta$region[74]="Bolzano-Bozen"
cluster.beta$region[98]="Aosta"
cluster.beta$region[79]="Reggio Emilia"
cluster.beta$region[34]="Forli'"
cluster.beta$region[78]="Reggio Calabria"

cluster.beta=cluster.beta[-c(9,13,27,30,45,47,56,73,81,101,104),c(2,3)]

#create a graphical representation on map of the clusters allocations

italy_map <- map_data("italy")
italy_proj <- "+proj=aea +lat_1=38.15040684902542
+lat_2=44.925490198742295 +lon_0=12.7880859375"

mapdata=left_join(italy_map,cluster.beta,by='region')


map.cluster = ggplot(mapdata,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=cluster),color='black')
map.cluster