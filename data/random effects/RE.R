re=t(ws[,-1])
re=data.frame(re)
nomi=sort(unique(dataset_completo$Territorio))
ws$Territorio=nomi
time=seq(from=2011,to=2020,by=1)
Time=rep(NA,1060)

Time=vec_rep(time,106)
ws=ws[,-1]
ws_2=c(ws$`0`, ws$`1`,ws$`2`,ws$`3`,ws$`4`,ws$`5`,ws$`6`,ws$`7`,ws$`8`,ws$`9`)

Time[955:1060]=2020
ws_2=data.frame(ws_2)
ws_2$TIME=Time
ws_2$Territorio=vec_rep(nomi,10)
ws_2$clusters=vec_rep(clusters,10)
colnames(ws_2)[1]='RE'
ws_2$clusters=factor(ws_2$clusters)

ggplot(data=ws_2,aes(x=TIME,y=RE,group=Territorio)) +
  geom_line(data=ws_2,aes(x=TIME,y=RE,color=clusters))



clusters=c(1, 2, 1, 2, 1, 1, 2, 1, 1, 2, 2, 3, 4, 1, 3, 2, 2, 1, 2, 1, 3, 2,
           2, 1, 2, 1, 1, 3, 2, 1, 2, 1, 1, 1, 2, 1, 5, 2, 2, 4, 2, 2, 3, 2,
           3, 2, 1, 2, 1, 1, 4, 2, 2, 3, 3, 3, 3, 1, 3, 2, 1, 3, 1, 2, 1, 1,
           1, 1, 1, 2, 1, 2, 1, 6, 3, 1, 1, 1, 3, 2, 1, 3, 4, 1, 1, 2, 1, 1,
           1, 1, 2, 2, 1, 1, 3, 1, 2, 3, 1, 1, 2, 2, 1, 2, 1, 2)


italy_map <- map_data("italy")

lat.mean=italy_map %>% group_by(region) %>% summarise(lat.media=mean(lat))
north=(lat.mean$lat.media > 42.5)
lat.mean=cbind(lat.mean,north=north)


w_italia=ws_2

nomi_95=nomi
nomi_95[75]="Trento"
nomi_95[74]="Bolzano-Bozen"
nomi_95[98]="Aosta"
nomi_95[79]="Reggio Emilia"
nomi_95[34]="Forli'"
nomi_95[78]="Reggio Calabria"
nomi_95=nomi_95[-c(9,13,27,30,45,47,56,73,81,101,104)]

w_italia=w_italia[which(w_italia$Territorio %in% nomi_95),]

w_italia$Territorio[which(w_italia$Territorio=="Provincia Autonoma Trento")]="Trento"
w_italia$Territorio[which(w_italia$Territorio=="Provincia Autonoma Bolzano / Bozen")]="Bolzano-Bozen"
w_italia$Territorio[which(w_italia$Territorio=="Valle d'Aosta / Vallée d'Aoste")]="Aosta"
w_italia$Territorio[which(w_italia$Territorio=="Reggio nell'Emilia")]="Reggio Emilia"
w_italia$Territorio[which(w_italia$Territorio=="Forlì-Cesena")]="Forli'"
w_italia$Territorio[which(w_italia$Territorio=="Reggio di Calabria")]="Reggio Calabria"
w_italia=w_italia[order(w_italia$Territorio),]

pippo=rep(NA,950)
nn=rep(FALSE,10)

for (i in 2:95){
  nome=lat.mean$region[i]
  n=lat.mean$north[i]
  nn=c(nn,rep(n,10))
}
w_italia$north=nn


ggplot(data=w_italia,aes(x=TIME,y=RE,group=region)) +
  geom_line(data=w_italia,aes(x=TIME,y=RE,color=north)) + 
  scale_color_manual(values=c("orange", "orangered"))

w.media=rowMeans(ws[,-11])
w.media_plot=data.frame(W=w.media[-c(9,13,27,30,45,47,56,73,81,101,104)],region=w_italia$Territorio)


mapdata=left_join(italy_map,w.media_plot,by='region')


map = ggplot(mapdata,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=W),color='black')
map

map.2 = map+ scale_fill_gradient(name='Random Effect', low='yellow',high='red',na.value='grey50')
map.2

colnames(w_italia)[3]="region"
mapdata2=left_join(italy_map,w_italia,by='region')

map.north = ggplot(data=mapdata2,aes(x=long, y= lat, group=group)) +
  geom_polygon(aes(fill=north),color='black')
map.north


mean(italy_map$lat[which(italy_map$region=="Messina")])
