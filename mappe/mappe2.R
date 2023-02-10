library(maptools)
library(ggplot2)
library(tidyverse)

write.table(dataset.nomi.giusti, 'dataset.nomi.mappa.txt', col.names=colnames(dataset.nomi.giusti))

dataset=read.csv("dataset.completo.csv")
regions.dataset=sort(unique(dataset$Territorio))

italy_map <- map_data("italy")
italy_proj <- "+proj=aea +lat_1=38.15040684902542
+lat_2=44.925490198742295 +lon_0=12.7880859375"

set.seed(1492)
choro_dat <- data.frame(region=unique(italy_map$region))
choro_dat=cbind(choro_dat,value=sample(100, length(choro_dat$region)))

mapdata1=left_join(italy_map,choro_dat,by='region')



map1 = ggplot(mapdata1,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=value),color='black')
map1

map2 = map1 + scale_fill_gradient(name='Values', low='yellow',high='red',na.value='grey50')
map2

regions.map=sort(unique(italy_map$region))


length(regions.map)
length(regions.dataset)

length(intersect(regions.map,unique(dataset.nomi.giusti$Territorio)))
setdiff(regions.map,unique(dataset.nomi.giusti$Territorio))

setdiff(regions.dataset,regions.map)

dataset.nomi.giusti=dataset

names.province=c('Bolzano / Bozen',"Valle d'Aosta / Vallée d'Aoste",
                 "Reggio nell'Emilia","Forlì-Cesena","Reggio di Calabria")

for (i in 1:1060){
  if (dataset.nomi.giusti$Territorio[i]=="Valle d'Aosta / Vallée d'Aoste"){
    dataset.nomi.giusti$Territorio[i]="Aosta"
  }
}

for (i in 1:1060){
  if (dataset.nomi.giusti$Territorio[i]=="Provincia Autonoma Bolzano / Bozen"){
    dataset.nomi.giusti$Territorio[i]="Bolzano-Bozen"
  }
}

for (i in 1:1060){
  if (dataset.nomi.giusti$Territorio[i]=="Reggio nell'Emilia"){
    dataset.nomi.giusti$Territorio[i]="Reggio Emilia"
  }
}

for (i in 1:1060){
  if (dataset.nomi.giusti$Territorio[i]=="Forlì-Cesena"){
    dataset.nomi.giusti$Territorio[i]="Forli'"
  }
}

for (i in 1:1060){
  if (dataset.nomi.giusti$Territorio[i]=="Reggio di Calabria"){
    dataset.nomi.giusti$Territorio[i]="Reggio Calabria"
  }
}

for (i in 1:1060){
  if (dataset.nomi.giusti$Territorio[i]=="Provincia Autonoma Trento"){
    dataset.nomi.giusti$Territorio[i]="Trento"
  }
}


#MAPPA CLUSTER BETA

#inattivita maschi
mean.inattivita.maschi=dataset %>% group_by(Territorio) %>% summarise(inattivita.maschi.media=mean(tasso.di.inattività.dei.maschi))
colnames(mean.inattivita.maschi)= c('region','values')

mapdata.inattivita.maschi=left_join(italy_map,mean.inattivita.maschi,by='region')


map1.inattivita.maschi = ggplot(mapdata.inattivita.maschi,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=values),color='black')
map1.inattivita.maschi


map2.inattivita.maschi = map1.inattivita.maschi + scale_fill_gradient(name='Inattività maschi', low='yellow',high='red',na.value='grey50')
map2.inattivita.maschi

#inattivita femmine
mean.inattivita.femmine=dataset %>% group_by(Territorio) %>% summarise(inattivita.femmine.media=mean(tasso.di.inattività.delle.femmine))
colnames(mean.inattivita.femmine)= c('region','values')

mapdata.inattivita.femmine=left_join(italy_map,mean.inattivita.femmine,by='region')


map1.inattivita.femmine = ggplot(mapdata.inattivita.femmine,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=values),color='black')
map1.inattivita.femmine


map2.inattivita.femmine = map1.inattivita.femmine + scale_fill_gradient(name='Inattività femmine', low='yellow',high='red',na.value='grey50')
map2.inattivita.femmine


#inattivita totale
mean.inattivita.totale=dataset %>% group_by(Territorio) %>% summarise(inattivita.totale.media=mean(tasso.di.inattività.totale))
colnames(mean.inattivita.totale)= c('region','values')

mapdata.inattivita.totale=left_join(italy_map,mean.inattivita.totale,by='region')


map1.inattivita.totale = ggplot(mapdata.inattivita.totale,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=values),color='black')
map1.inattivita.totale


map2.inattivita.totale = map1.inattivita.totale + scale_fill_gradient(name='Inattività totale', low='yellow',high='red',na.value='grey50')
map2.inattivita.totale



#occupazione maschi
mean.occupazione.maschi=dataset %>% group_by(Territorio) %>% summarise(occupazione.maschi.media=mean(tasso.di.occupazione.dei.maschi))
colnames(mean.occupazione.maschi)= c('region','values')

mapdata.occupazione.maschi=left_join(italy_map,mean.occupazione.maschi,by='region')


map1.occupazione.maschi = ggplot(mapdata.occupazione.maschi,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=values),color='black')
map1.occupazione.maschi

map2.occupazione.maschi = map1.occupazione.maschi + scale_fill_gradient(name='Occupazione maschi', low='yellow',high='red',na.value='grey50')
map2.occupazione.maschi


#occupazione femmine
mean.occupazione.femmine=dataset %>% group_by(Territorio) %>% summarise(occupazione.femmine.media=mean(tasso.di.occupazione.delle.femmine))
colnames(mean.occupazione.femmine)= c('region','values')

mapdata.occupazione.femmine=left_join(italy_map,mean.occupazione.femmine,by='region')


map1.occupazione.femmine = ggplot(mapdata.occupazione.femmine,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=values),color='black')
map1.occupazione.femmine

map2.occupazione.femmine = map1.occupazione.femmine + scale_fill_gradient(name='Occupazione femmine', low='yellow',high='red',na.value='grey50')
map2.occupazione.femmine


#occupazione totale
mean.occupazione.totale=dataset %>% group_by(Territorio) %>% summarise(occupazione.totale.media=mean(tasso.di.occupazione.totale))
colnames(mean.occupazione.totale)= c('region','values')

mapdata.occupazione.totale=left_join(italy_map,mean.occupazione.totale,by='region')


map1.occupazione.totale = ggplot(mapdata.occupazione.totale,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=values),color='black')
map1.occupazione.totale

map2.occupazione.totale = map1.occupazione.totale + scale_fill_gradient(name='Occupazione totale', low='yellow',high='red',na.value='grey50')
map2.occupazione.totale

v=setdiff(unique(dataset.nomi.giusti$Territorio),unique(italy_map$region))
dataset.giusto=dataset.nomi.giusti


#time series matplots

lat.mean=italy_map %>% group_by(region) %>% summarise(lat.media=mean(lat))
north=(lat.mean$lat.media > 42.5)
lat.mean=cbind(lat.mean,north=north)


#inattivita maschi
inattivita.maschi=dataset.giusto[,c(1,2,6)]
time.inattivita.maschi=cbind(inattivita.maschi,north=rep(NA,950))

for(i in 1:950){
  n=time.inattivita.maschi[i,1]
  is.north=lat.mean[which(lat.mean$region==n),3]
  if(is.north)
    time.inattivita.maschi[i,4]=1
  else
    time.inattivita.maschi[i,4]=0
}


time.inattivita.maschi$north=factor(time.inattivita.maschi$north)

time.inattivita.maschi.plot = ggplot(data=time.inattivita.maschi,aes(x=TIME,y=tasso.di.inattività.dei.maschi,group=Territorio,colour=north)) + geom_line()
time.inattivita.maschi.plot


#inattivita femmine
inattivita.femmine=dataset.giusto[,c(1,2,7)]
time.inattivita.femmine=cbind(inattivita.femmine,north=rep(NA,950))

for(i in 1:950){
  n=time.inattivita.femmine[i,1]
  is.north=lat.mean[which(lat.mean$region==n),3]
  if(is.north)
    time.inattivita.femmine[i,4]=1
  else
    time.inattivita.femmine[i,4]=0
}


time.inattivita.femmine$north=factor(time.inattivita.femmine$north)

time.inattivita.femmine.plot = ggplot(data=time.inattivita.femmine,aes(x=TIME,y=tasso.di.inattività.delle.femmine,group=Territorio,colour=north)) + geom_line()
time.inattivita.femmine.plot


#inattivita totale
inattivita.totale=dataset.giusto[,c(1,2,8)]
time.inattivita.totale=cbind(inattivita.totale,north=rep(NA,950))

for(i in 1:950){
  n=time.inattivita.totale[i,1]
  is.north=lat.mean[which(lat.mean$region==n),3]
  if(is.north)
    time.inattivita.totale[i,4]=1
  else
    time.inattivita.totale[i,4]=0
}


time.inattivita.totale$north=factor(time.inattivita.totale$north)

time.inattivita.totale.plot = ggplot(data=time.inattivita.totale,aes(x=TIME,y=tasso.di.inattività.totale,group=Territorio,colour=north)) + geom_line()
time.inattivita.totale.plot


#occupazione maschi
occupazione.maschi=dataset.giusto[,c(1,2,9)]
time.occupazione.maschi=cbind(occupazione.maschi,north=rep(NA,950))

for(i in 1:950){
  n=time.occupazione.maschi[i,1]
  is.north=lat.mean[which(lat.mean$region==n),3]
  if(is.north)
    time.occupazione.maschi[i,4]=1
  else
    time.occupazione.maschi[i,4]=0
}


time.occupazione.maschi$north=factor(time.occupazione.maschi$north)

time.occupazione.maschi.plot = ggplot(data=time.occupazione.maschi,aes(x=TIME,y=tasso.di.occupazione.dei.maschi,group=Territorio,colour=north)) + geom_line()
time.occupazione.maschi.plot


#occupazione femmine
occupazione.femmine=dataset.giusto[,c(1,2,10)]
time.occupazione.femmine=cbind(occupazione.femmine,north=rep(NA,950))

for(i in 1:950){
  n=time.occupazione.femmine[i,1]
  is.north=lat.mean[which(lat.mean$region==n),3]
  if(is.north)
    time.occupazione.femmine[i,4]=1
  else
    time.occupazione.femmine[i,4]=0
}


time.occupazione.femmine$north=factor(time.occupazione.femmine$north)

time.occupazione.femmine.plot = ggplot(data=time.occupazione.femmine,aes(x=TIME,y=tasso.di.occupazione.delle.femmine,group=Territorio,colour=north)) + geom_line()
time.occupazione.femmine.plot


#occupazione totale
occupazione.totale=dataset.giusto[,c(1,2,11)]
time.occupazione.totale=cbind(occupazione.totale,north=rep(NA,950))

for(i in 1:950){
  n=time.occupazione.totale[i,1]
  is.north=lat.mean[which(lat.mean$region==n),3]
  if(is.north)
    time.occupazione.totale[i,4]=1
  else
    time.occupazione.totale[i,4]=0
}


time.occupazione.totale$north=factor(time.occupazione.totale$north)

time.occupazione.totale.plot = ggplot(data=time.occupazione.totale,aes(x=TIME,y=tasso.di.occupazione.totale,group=Territorio,colour=north)) + geom_line()
time.occupazione.totale.plot


save.image("matplots.RData")
