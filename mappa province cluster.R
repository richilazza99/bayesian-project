library(maptools)
library(ggplot2)
library(tidyverse)

#In this script we plot the map of Italy with the computed clusters using different colors

#cluster.beta=read.csv("#######")  mettere matrice cluster allocation dei beta
#dataset=read.csv("Fecondita.csv")   carica il dataset completo


cluster.beta$region=sort(unique(dataset$Territorio))
colnames(cluster.beta)[2]="cluster"
cluster.beta$cluster=factor(cluster.beta$cluster)


#setdiff(cluster.beta$region,unique(italy_map$region))
#setdiff(unique(italy_map$region),cluster.beta$region)

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


