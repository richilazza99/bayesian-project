
library(maptools)
library(ggplot2)
library(tidyverse)
#library(ggalt)
#library(ggthemes)
#library(tibble)
#library(viridis)

raw=read.csv('DCIS_FECONDITA1_09122022103658644.csv',header=T)

load("QualitativeStudy.RData")

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

library(spdep)

long.mean=italy_map %>% group_by(region) %>% summarise(long.media=mean(long))



coordinates=rep(0,ncol=3,nrow=95)
coordinates=data.frame(lat=lat.mean$lat.media,long=long.mean$long.media,region=lat.mean$region)


coordinate.province=italy_map.padre %>% group_by(region) %>% summarise(padre.media=mean(Value))


itadata=raster::getData(name = "countries", country = "ITA", level = 2)
boundaries <- raster::getData(name = "GADM", country = "ITA", level = 2)
boundaries2 <- raster::getData(name = "alt", country = "ITA", level = 2)


list.provinces=boundaries$NAME_2
our.provinces=coordinates$region
intersect(our.provinces,list.provinces)
to.delete=setdiff(list.provinces,our.provinces)

indeces=rep(0,15)

provinces=boundaries$NAME_2
provinces=provinces[!provinces %in% to.delete]

for (i in 1:15){
  name=to.delete[i]
  indeces[i]=which(boundaries$NAME_2==name)
  
}
pippo=nb_q
pippo[[6]]=NULL
length(pippo)
pippo[[14]]=NULL
pippo[[15]]=NULL
pippo[[28]]=NULL
pippo[[83]]=NULL



for (j in 1:95){
  vector=pippo[[j]]
  vector=vector[!vector %in% indeces]
  pippo[[j]]=vector
    
}

province.to.index=matrix()

nb.matrix=pippo
anna.matrix=matrix(0,nrow=95,ncol=110)
for (i in 1:95){
  vector=pippo[[i]]
  for (j in 1:110){
    
    check=j %in% vector
    if (check==TRUE){
      anna.matrix[i,j]=1
    }
  }
}
diag(anna.matrix)=1

nic=anna.matrix[,-indeces]
dim(nic)
rownames(nic)=colnames(nic)

setdiff(list.provinces,our.provinces)
our.provinces[14]="Bolzano"
our.provinces[30]="Florence"
our.provinces[32]="Forli' - Cesena"
our.provinces[46]="Mantua"
our.provinces[47]="Massa Carrara"
our.provinces[56]="Padua"
our.provinces[61]="Pesaro E Urbino"
our.provinces[70]="Reggio Di Calabria"
our.provinces[71]="Reggio Nell'Emilia"
our.provinces[79]="Syracuse"

intersect(our.provinces,list.provinces)



library(tmap)
library(raster)
library(spdep)
# Show data
tm_shape(boundaries) +
  tm_polygons()
nb_q <- poly2nb(boundaries)
nb_q.2 <- poly2nb(boundaries2)


# Plot original results
  coords <- coordinates(boundaries)

  
print(nb_q)
  
  
  
  
# Show the results
plot(boundaries)
plot(nb_q, coordinates, col="grey", add = TRUE)
# Sparse matrix
  nb_B <- nb2listw(nb_q, style="B", zero.policy=TRUE)
B <- as(nb_B, "symmetricMatrix")

# Calculate shortest distance
g1 <- graph.adjacency(B, mode="undirected")
sp_mat <- shortest.paths(g1)

nb <- poly2nb(s1, queen=TRUE)




Moran.I(, ozone.dists.inv)




save.image('QualitativeStudy.RData')

