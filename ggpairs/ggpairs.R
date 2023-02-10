dataset=read.csv("dataset.completo.csv")

load("matplots.RData")

dati=dataset.giusto[,-2]

dati=cbind(dati,rep(NA,950))
names(dati)[11]="is.north"


for(i in 1:950){
  n=dati[i,1]
  is.north=lat.mean[which(lat.mean$region==n),3]
  if(is.north)
    dati[i,11]=1
  else
   dati[i,11]=0
}

dati$is.north=factor(dati$is.north)

names(dati)=c("Territorio","Età madri","Età padri","Fecondità","Inattività maschi","Inattività femmine","Inattività",
              "Occupazione maschi","Occupazione femmine","Occupazione","is.north")
ggpairs(dati, aes(colour=is.north),columns=2:10)
