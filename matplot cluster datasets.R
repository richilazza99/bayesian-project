

data=read.csv("dataset.completo.csv")
nomi=sort(unique(data$Territorio))
data$cluster=rep(NA,1060)


clusters=rep(NA,nrow=106,ncol=2)
clusters=c(1, 2, 1, 2, 1, 1, 2, 1, 1, 2, 2, 3, 4, 1, 3, 2, 2, 1, 2, 1, 3, 2,
2, 1, 2, 1, 1, 3, 2, 1, 2, 1, 1, 1, 2, 1, 5, 2, 2, 4, 2, 2, 3, 2,
3, 2, 1, 2, 1, 1, 4, 2, 2, 3, 3, 3, 3, 1, 3, 2, 1, 3, 1, 2, 1, 1,
1, 1, 1, 2, 1, 2, 1, 6, 3, 1, 1, 1, 3, 2, 1, 3, 4, 1, 1, 2, 1, 1,
1, 1, 2, 2, 1, 1, 3, 1, 2, 3, 1, 1, 2, 2, 1, 2, 1, 2)
clusters=factor(clusters)

data=data[order(data$Territorio),]

data$gorizia=rep(0,1060)
for (i in 1:1060){
  for (j in 1:106){
    if(data$Territorio[i]==clusters$region[j]){
      data$cluster[i]=clusters$clusters[j]
    }
  }
}
data$gorizia[which(data$Territorio=="Gorizia")]=1
data$gorizia[which(data$Territorio=="Provincia Autonoma Bolzano / Bozen")]=1
data$gorizia=factor(data$gorizia)
clusters=data.frame(clusters)
clusters$region=nomi


data$cluster=factor(data$cluster)
cluster.fecondita = ggplot(data=data,aes(x=TIME,y=tasso.di.fecondità.totale,group=Territorio,colour=cluster)) + geom_line(size=1.1)+ gghighlight::gghighlight(use_direct_label = F,cluster ==  "6") 
cluster.fecondita
library(ggplot2)
library(gghighlight)
