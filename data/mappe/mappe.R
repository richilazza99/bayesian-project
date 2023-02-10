
library(maptools)
library(ggplot2)
library(tidyverse)
#library(ggalt)
#library(ggthemes)
#library(tibble)
#library(viridis)

raw=read.csv('DCIS_FECONDITA1_09122022103658644.csv',header=T)

# get italy region map
italy_map <- map_data("italy")
# probabilmente non ci sono tutte le province che abbiamo nel dataset :(

# your data will need to have these region names
print(unique(italy_map$region))
print(unique(raw$Territorio))



# we'll simulate some data for this
set.seed(1492)
choro_dat <- data.frame(region=unique(italy_map$region))
choro_dat=cbind(choro_dat,value=sample(100, length(choro_dat$region)))



# we'll use this in a bit
italy_proj <- "+proj=aea +lat_1=38.15040684902542
+lat_2=44.925490198742295 +lon_0=12.7880859375"

library(tidyverse)
mapdata1=left_join(italy_map,choro_dat,by='region')

map1 = ggplot(mapdata1,aes(x=long, y= lat, group=group)) +
       geom_polygon(aes(fill=value),color='black')
map1


map2 = map1 + scale_fill_gradient(name='Values', low='yellow',high='red',na.value='grey50')
map2

############ Provinces Regions ##############
intersect(unique(italy_map$region),unique(raw$Territorio))

setdiff(unique(italy_map$region),unique(raw$Territorio))


v=setdiff(unique(raw$Territorio),unique(italy_map$region))

names.province=c('Bolzano / Bozen',"Valle d'Aosta / Vallée d'Aoste",
                 "Reggio nell'Emilia","Forlì-Cesena","Reggio di Calabria")
right.names=setdiff(unique(italy_map$region),unique(raw$Territorio))

for(i in 1:length(names.province)){
  raw[which(raw$Territorio==names.province[i]),2]=right.names[i]
}


data.map=raw[-which(raw$Territorio %in% v),c(2,4,5,7)]

####### fecondità
data.map.fecondita=data.map[which(data.map$Tipo.dato=='tasso di fecondità totale'),c(1,3,4)]

print(unique(data.map.fecondita$Territorio))

setdiff(unique(data.map.fecondita$Territorio),unique(italy_map$region))
setdiff(unique(italy_map$region),unique(data.map.fecondita$Territorio))


mean.fecondita=data.map.fecondita %>% group_by(Territorio) %>% summarise(fecondita.media=mean(Value))
colnames(mean.fecondita)= c('region','values')

mapdata.fecondita=left_join(italy_map,mean.fecondita,by='region')


map1.fecondita = ggplot(mapdata.fecondita,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=values),color='black')
map1.fecondita


map2.fecondita = map1.fecondita + scale_fill_gradient(name='Fecondità', low='yellow',high='red',na.value='grey50')
map2.fecondita

##### madre 
data.map.madre=data.map[which(data.map$Tipo.dato=='età media delle madri al parto'),c(1,3,4)]

mean.madre=data.map.madre %>% group_by(Territorio) %>% summarise(madre.media=mean(Value))
colnames(mean.madre)= c('region','values')

mapdata.madre=left_join(italy_map,mean.madre,by='region')


map1.madre = ggplot(mapdata.madre,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=values),color='black')
map1.madre


map2.madre = map1.madre + scale_fill_gradient(name='Age of the Mother', low='yellow',high='red',na.value='grey50')
map2.madre


##### padre 
data.map.padre=data.map[which(data.map$Tipo.dato=='età media dei padri alla nascita del figlio'),c(1,3,4)]

mean.padre=data.map.padre %>% group_by(Territorio) %>% summarise(padre.media=mean(Value))
colnames(mean.padre)= c('region','values')

mapdata.padre=left_join(italy_map,mean.padre,by='region')


map1.padre = ggplot(mapdata.padre,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=values),color='black')
map1.padre


map2.padre = map1.padre + scale_fill_gradient(name='Age of the Father', low='yellow',high='red',na.value='grey50')
map2.padre

####### matplot of time series ############
time.fecondita = ggplot(data=data.map.fecondita,aes(x=TIME,y=Value,group=Territorio)) + geom_line()
time.fecondita

# dividing into north and south
lat.mean=italy_map %>% group_by(region) %>% summarise(lat.media=mean(lat))
north=(lat.mean$lat.media > 42.5)
lat.mean=cbind(lat.mean,north=north)

time.fecondita.data=cbind(data.map.fecondita,north=rep(NA,960))

for(i in 1:960){
  n=time.fecondita.data[i,1]
  is.north=lat.mean[which(lat.mean$region==n),3]
  if(is.north)
    time.fecondita.data[i,4]=1
  else
    time.fecondita.data[i,4]=0
}

time.fecondita.data$north=factor(time.fecondita.data$north)

time.fecondita = ggplot(data=time.fecondita.data,aes(x=TIME,y=Value,group=Territorio,colour=north)) + geom_line()
time.fecondita

##### madre
time.madre.data=cbind(data.map.madre,north=rep(NA,960))

for(i in 1:960){
  n=time.madre.data[i,1]
  is.north=lat.mean[which(lat.mean$region==n),3]
  if(is.north)
    time.madre.data[i,4]=1
  else
    time.madre.data[i,4]=0
}

time.madre.data$north=factor(time.madre.data$north)

time.madre = ggplot(data=time.madre.data,aes(x=TIME,y=Value,group=Territorio,colour=north)) + geom_line()
time.madre

### padre
time.padre.data=cbind(data.map.padre,north=rep(NA,960))

for(i in 1:960){
  n=time.padre.data[i,1]
  is.north=lat.mean[which(lat.mean$region==n),3]
  if(is.north)
    time.padre.data[i,4]=1
  else
    time.padre.data[i,4]=0
}

time.padre.data$north=factor(time.padre.data$north)

time.padre = ggplot(data=time.padre.data,aes(x=TIME,y=Value,group=Territorio,colour=north)) + geom_line()
time.padre


######## spatial autocorrelation indices ############









save.image('QualitativeStudy.RData')

