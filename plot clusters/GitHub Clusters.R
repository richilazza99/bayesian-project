library(tidyverse)
library(raster)
library(rasterVis)
library(ggplot2)

true_clusters_ggplot=data.frame(clusters=true_clusters_plot,long=rep(NA,100),lat=rep(NA,100))
ll=seq(from=1,to=10,by=1)
long.ext=c(ll,ll,ll,ll,ll,ll,ll,ll,ll,ll)
lat.ext=rep(NA,100)
lat.ext[91:100]=10
true_clusters_ggplot$long=long.ext - 0.5
true_clusters_ggplot$lat=lat.ext - 0.5

#plot true clusters
ggplot(true_clusters_ggplot, aes(x = long, y = lat, fill = clusters)) + 
  geom_tile(data=true_clusters_ggplot,aes(alpha=0.9) ) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  xlab("x") + ylab("y")



s_vi_data=data.frame(clusters=s_vi$V1,long=true_clusters_ggplot$long,lat=true_clusters_ggplot$lat)
s_vi_data$clusters=factor(s_vi_data$clusters)


#plot S_VI
ggplot(s_vi_data_2, aes(x = long, y = lat, fill = clusters)) + 
  geom_tile(data=s_vi_data_2,aes(alpha=0.9) ) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  xlab("x") + ylab("y")


s_vi_data_2=s_vi_data
s_vi_data_2$long=s_vi_data_2$long - 0.5
s_vi_data_2$lat=s_vi_data_2$lat - 0.5

s_binder_data=data.frame(clusters=s_binder$V1,long=s_vi_data$long - 0.5,lat=s_vi_data$lat - 0.5)
s_binder_data$clusters=factor(s_binder_data$clusters)

#plot binder loss
ggplot(s_binder_data, aes(x = long, y = lat, fill = clusters)) + 
  geom_tile(data=s_binder_data,aes(alpha=0.9) ) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  xlab("x") + ylab("y")