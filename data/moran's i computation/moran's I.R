#the indeces are listed in MORAN.ind and also saved as "tabella moran's I.png"



dataset=read.csv("dataset.completo.csv")
proximity=read.csv("proximity.EXACT.csv")
unique(dataset$Territorio)
setdiff(unique(dataset$Territorio),rownames(proximity))
colnames(proximity)[9]="Barletta-Andria-Trani"

rownames(proximity)=colnames(proximity)
library(ape)

load("moran's I .RData")
save.image("moran's I .RData")
W=0
for (i in 1:106){
  for (j in 1:106){
    W=W+proximity_0[i,j]
  }
}


N=106

moran = function(x){
  m=mean(x)
  den=0
  for (i in 1:106){
    den=den+(x[i]-m)^2
  }
  num=0
  for (i in 1:106){
    for (j in 1:106){
      num=num+proximity_0[i,j]*(x[i]-m)*(x[j]-m)
    }
  }
  
  
  I=(N/W)*(num/den)
  
  return(I)
}

moran(prova)
N=106

prova=dataset[which(dataset$TIME==2011),c(1,3)]
prova=prova[order(prova$Territorio),2]

#eta madre
madre_2011=dataset[which(dataset$TIME==2011),c(1,3)]
madre_2012=dataset[which(dataset$TIME==2012),c(1,3)]
madre_2013=dataset[which(dataset$TIME==2013),c(1,3)]
madre_2014=dataset[which(dataset$TIME==2014),c(1,3)]
madre_2015=dataset[which(dataset$TIME==2015),c(1,3)]
madre_2016=dataset[which(dataset$TIME==2016),c(1,3)]
madre_2017=dataset[which(dataset$TIME==2017),c(1,3)]
madre_2018=dataset[which(dataset$TIME==2018),c(1,3)]
madre_2019=dataset[which(dataset$TIME==2019),c(1,3)]
madre_2020=dataset[which(dataset$TIME==2020),c(1,3)]

madre_2011=madre_2011[order(madre_2011$Territorio),2]
madre_2012=madre_2012[order(madre_2012$Territorio),2]
madre_2013=madre_2013[order(madre_2013$Territorio),2]
madre_2014=madre_2014[order(madre_2014$Territorio),2]
madre_2015=madre_2015[order(madre_2015$Territorio),2]
madre_2016=madre_2016[order(madre_2016$Territorio),2]
madre_2017=madre_2017[order(madre_2017$Territorio),2]
madre_2018=madre_2018[order(madre_2018$Territorio),2]
madre_2019=madre_2019[order(madre_2019$Territorio),2]
madre_2020=madre_2020[order(madre_2020$Territorio),2]

I_madre = c(Moran.I(madre_2011,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
Moran.I(madre_2012,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
Moran.I(madre_2013,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
Moran.I(madre_2014,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
Moran.I(madre_2015,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
Moran.I(madre_2016,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
Moran.I(madre_2017,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
Moran.I(madre_2018,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
Moran.I(madre_2019,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
Moran.I(madre_2020,proximity_0, scaled = FALSE, alternative = "two.sided")$observed)

I_madre
I_madre_finale=mean(I_madre)


#eta padre
padre_2011=dataset[which(dataset$TIME==2011),c(1,4)]
padre_2012=dataset[which(dataset$TIME==2012),c(1,4)]
padre_2013=dataset[which(dataset$TIME==2013),c(1,4)]
padre_2014=dataset[which(dataset$TIME==2014),c(1,4)]
padre_2015=dataset[which(dataset$TIME==2015),c(1,4)]
padre_2016=dataset[which(dataset$TIME==2016),c(1,4)]
padre_2017=dataset[which(dataset$TIME==2017),c(1,4)]
padre_2018=dataset[which(dataset$TIME==2018),c(1,4)]
padre_2019=dataset[which(dataset$TIME==2019),c(1,4)]
padre_2020=dataset[which(dataset$TIME==2020),c(1,4)]

padre_2011=padre_2011[order(padre_2011$Territorio),2]
padre_2012=padre_2012[order(padre_2012$Territorio),2]
padre_2013=padre_2013[order(padre_2013$Territorio),2]
padre_2014=padre_2014[order(padre_2014$Territorio),2]
padre_2015=padre_2015[order(padre_2015$Territorio),2]
padre_2016=padre_2016[order(padre_2016$Territorio),2]
padre_2017=padre_2017[order(padre_2017$Territorio),2]
padre_2018=padre_2018[order(padre_2018$Territorio),2]
padre_2019=padre_2019[order(padre_2019$Territorio),2]
padre_2020=padre_2020[order(padre_2020$Territorio),2]


I_padre = c(Moran.I(padre_2011,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(padre_2012,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(padre_2013,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(padre_2014,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(padre_2015,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(padre_2016,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(padre_2017,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(padre_2018,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(padre_2019,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(padre_2020,proximity_0, scaled = FALSE, alternative = "two.sided")$observed)

I_padre
I_padre_finale=mean(I_padre)



#fecondita
fecondita_2011=dataset[which(dataset$TIME==2011),c(1,5)]
fecondita_2012=dataset[which(dataset$TIME==2012),c(1,5)]
fecondita_2013=dataset[which(dataset$TIME==2013),c(1,5)]
fecondita_2014=dataset[which(dataset$TIME==2014),c(1,5)]
fecondita_2015=dataset[which(dataset$TIME==2015),c(1,5)]
fecondita_2016=dataset[which(dataset$TIME==2016),c(1,5)]
fecondita_2017=dataset[which(dataset$TIME==2017),c(1,5)]
fecondita_2018=dataset[which(dataset$TIME==2018),c(1,5)]
fecondita_2019=dataset[which(dataset$TIME==2019),c(1,5)]
fecondita_2020=dataset[which(dataset$TIME==2020),c(1,5)]


fecondita_2011=fecondita_2011[order(fecondita_2011$Territorio),2]
fecondita_2012=fecondita_2012[order(fecondita_2012$Territorio),2]
fecondita_2013=fecondita_2013[order(fecondita_2013$Territorio),2]
fecondita_2014=fecondita_2014[order(fecondita_2014$Territorio),2]
fecondita_2015=fecondita_2015[order(fecondita_2015$Territorio),2]
fecondita_2016=fecondita_2016[order(fecondita_2016$Territorio),2]
fecondita_2017=fecondita_2017[order(fecondita_2017$Territorio),2]
fecondita_2018=fecondita_2018[order(fecondita_2018$Territorio),2]
fecondita_2019=fecondita_2019[order(fecondita_2019$Territorio),2]
fecondita_2020=fecondita_2020[order(fecondita_2020$Territorio),2]

I_fecondita = c(Moran.I(fecondita_2011,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(fecondita_2012,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(fecondita_2013,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(fecondita_2014,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(fecondita_2015,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(fecondita_2016,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(fecondita_2017,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(fecondita_2018,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(fecondita_2019,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
            Moran.I(fecondita_2020,proximity_0, scaled = FALSE, alternative = "two.sided")$observed)

I_fecondita
I_fecondita_finale=mean(I_fecondita)


#inattività maschi
maschi_2011=dataset[which(dataset$TIME==2011),c(1,6)]
maschi_2012=dataset[which(dataset$TIME==2012),c(1,6)]
maschi_2013=dataset[which(dataset$TIME==2013),c(1,6)]
maschi_2014=dataset[which(dataset$TIME==2014),c(1,6)]
maschi_2015=dataset[which(dataset$TIME==2015),c(1,6)]
maschi_2016=dataset[which(dataset$TIME==2016),c(1,6)]
maschi_2017=dataset[which(dataset$TIME==2017),c(1,6)]
maschi_2018=dataset[which(dataset$TIME==2018),c(1,6)]
maschi_2019=dataset[which(dataset$TIME==2019),c(1,6)]
maschi_2020=dataset[which(dataset$TIME==2020),c(1,6)]


maschi_2011=maschi_2011[order(maschi_2011$Territorio),2]
maschi_2012=maschi_2012[order(maschi_2012$Territorio),2]
maschi_2013=maschi_2013[order(maschi_2013$Territorio),2]
maschi_2014=maschi_2014[order(maschi_2014$Territorio),2]
maschi_2015=maschi_2015[order(maschi_2015$Territorio),2]
maschi_2016=maschi_2016[order(maschi_2016$Territorio),2]
maschi_2017=maschi_2017[order(maschi_2017$Territorio),2]
maschi_2018=maschi_2018[order(maschi_2018$Territorio),2]
maschi_2019=maschi_2019[order(maschi_2019$Territorio),2]
maschi_2020=maschi_2020[order(maschi_2020$Territorio),2]

I_maschi = c(Moran.I(maschi_2011,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                Moran.I(maschi_2012,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                Moran.I(maschi_2013,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                Moran.I(maschi_2014,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                Moran.I(maschi_2015,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                Moran.I(maschi_2016,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                Moran.I(maschi_2017,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                Moran.I(maschi_2018,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                Moran.I(maschi_2019,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                Moran.I(maschi_2020,proximity_0, scaled = FALSE, alternative = "two.sided")$observed)

I_maschi
I_maschi_finale=mean(I_maschi)

#inattività femmine
femmine_2011=dataset[which(dataset$TIME==2011),c(1,7)]
femmine_2012=dataset[which(dataset$TIME==2012),c(1,7)]
femmine_2013=dataset[which(dataset$TIME==2013),c(1,7)]
femmine_2014=dataset[which(dataset$TIME==2014),c(1,7)]
femmine_2015=dataset[which(dataset$TIME==2015),c(1,7)]
femmine_2016=dataset[which(dataset$TIME==2016),c(1,7)]
femmine_2017=dataset[which(dataset$TIME==2017),c(1,7)]
femmine_2018=dataset[which(dataset$TIME==2018),c(1,7)]
femmine_2019=dataset[which(dataset$TIME==2019),c(1,7)]
femmine_2020=dataset[which(dataset$TIME==2020),c(1,7)]


femmine_2011=femmine_2011[order(femmine_2011$Territorio),2]
femmine_2012=femmine_2012[order(femmine_2012$Territorio),2]
femmine_2013=femmine_2013[order(femmine_2013$Territorio),2]
femmine_2014=femmine_2014[order(femmine_2014$Territorio),2]
femmine_2015=femmine_2015[order(femmine_2015$Territorio),2]
femmine_2016=femmine_2016[order(femmine_2016$Territorio),2]
femmine_2017=femmine_2017[order(femmine_2017$Territorio),2]
femmine_2018=femmine_2018[order(femmine_2018$Territorio),2]
femmine_2019=femmine_2019[order(femmine_2019$Territorio),2]
femmine_2020=femmine_2020[order(femmine_2020$Territorio),2]

I_femmine = c(Moran.I(femmine_2011,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
             Moran.I(femmine_2012,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
             Moran.I(femmine_2013,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
             Moran.I(femmine_2014,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
             Moran.I(femmine_2015,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
             Moran.I(femmine_2016,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
             Moran.I(femmine_2017,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
             Moran.I(femmine_2018,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
             Moran.I(femmine_2019,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
             Moran.I(femmine_2020,proximity_0, scaled = FALSE, alternative = "two.sided")$observed)

I_femmine
I_femmine_finale=mean(I_femmine)



#inattività totale
inattivita_2011=dataset[which(dataset$TIME==2011),c(1,8)]
inattivita_2012=dataset[which(dataset$TIME==2012),c(1,8)]
inattivita_2013=dataset[which(dataset$TIME==2013),c(1,8)]
inattivita_2014=dataset[which(dataset$TIME==2014),c(1,8)]
inattivita_2015=dataset[which(dataset$TIME==2015),c(1,8)]
inattivita_2016=dataset[which(dataset$TIME==2016),c(1,8)]
inattivita_2017=dataset[which(dataset$TIME==2017),c(1,8)]
inattivita_2018=dataset[which(dataset$TIME==2018),c(1,8)]
inattivita_2019=dataset[which(dataset$TIME==2019),c(1,8)]
inattivita_2020=dataset[which(dataset$TIME==2020),c(1,8)]


inattivita_2011=inattivita_2011[order(inattivita_2011$Territorio),2]
inattivita_2012=inattivita_2012[order(inattivita_2012$Territorio),2]
inattivita_2013=inattivita_2013[order(inattivita_2013$Territorio),2]
inattivita_2014=inattivita_2014[order(inattivita_2014$Territorio),2]
inattivita_2015=inattivita_2015[order(inattivita_2015$Territorio),2]
inattivita_2016=inattivita_2016[order(inattivita_2016$Territorio),2]
inattivita_2017=inattivita_2017[order(inattivita_2017$Territorio),2]
inattivita_2018=inattivita_2018[order(inattivita_2018$Territorio),2]
inattivita_2019=inattivita_2019[order(inattivita_2019$Territorio),2]
inattivita_2020=inattivita_2020[order(inattivita_2020$Territorio),2]

I_inattivita = c(Moran.I(inattivita_2011,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
              Moran.I(inattivita_2012,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
              Moran.I(inattivita_2013,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
              Moran.I(inattivita_2014,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
              Moran.I(inattivita_2015,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
              Moran.I(inattivita_2016,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
              Moran.I(inattivita_2017,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
              Moran.I(inattivita_2018,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
              Moran.I(inattivita_2019,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
              Moran.I(inattivita_2020,proximity_0, scaled = FALSE, alternative = "two.sided")$observed)

I_inattivita
I_inattivita_finale=mean(I_inattivita)

#occupazione maschi
occ.maschi_2011=dataset[which(dataset$TIME==2011),c(1,9)]
occ.maschi_2012=dataset[which(dataset$TIME==2012),c(1,9)]
occ.maschi_2013=dataset[which(dataset$TIME==2013),c(1,9)]
occ.maschi_2014=dataset[which(dataset$TIME==2014),c(1,9)]
occ.maschi_2015=dataset[which(dataset$TIME==2015),c(1,9)]
occ.maschi_2016=dataset[which(dataset$TIME==2016),c(1,9)]
occ.maschi_2017=dataset[which(dataset$TIME==2017),c(1,9)]
occ.maschi_2018=dataset[which(dataset$TIME==2018),c(1,9)]
occ.maschi_2019=dataset[which(dataset$TIME==2019),c(1,9)]
occ.maschi_2020=dataset[which(dataset$TIME==2020),c(1,9)]


occ.maschi_2011=occ.maschi_2011[order(occ.maschi_2011$Territorio),2]
occ.maschi_2012=occ.maschi_2012[order(occ.maschi_2012$Territorio),2]
occ.maschi_2013=occ.maschi_2013[order(occ.maschi_2013$Territorio),2]
occ.maschi_2014=occ.maschi_2014[order(occ.maschi_2014$Territorio),2]
occ.maschi_2015=occ.maschi_2015[order(occ.maschi_2015$Territorio),2]
occ.maschi_2016=occ.maschi_2016[order(occ.maschi_2016$Territorio),2]
occ.maschi_2017=occ.maschi_2017[order(occ.maschi_2017$Territorio),2]
occ.maschi_2018=occ.maschi_2018[order(occ.maschi_2018$Territorio),2]
occ.maschi_2019=occ.maschi_2019[order(occ.maschi_2019$Territorio),2]
occ.maschi_2020=occ.maschi_2020[order(occ.maschi_2020$Territorio),2]

I_occ.maschi = c(Moran.I(occ.maschi_2011,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.maschi_2012,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.maschi_2013,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.maschi_2014,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.maschi_2015,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.maschi_2016,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.maschi_2017,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.maschi_2018,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.maschi_2019,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.maschi_2020,proximity_0, scaled = FALSE, alternative = "two.sided")$observed)

I_occ.maschi
I_occ.maschi_finale=mean(I_occ.maschi)


#occupazione femmine
occ.femmine_2011=dataset[which(dataset$TIME==2011),c(1,10)]
occ.femmine_2012=dataset[which(dataset$TIME==2012),c(1,10)]
occ.femmine_2013=dataset[which(dataset$TIME==2013),c(1,10)]
occ.femmine_2014=dataset[which(dataset$TIME==2014),c(1,10)]
occ.femmine_2015=dataset[which(dataset$TIME==2015),c(1,10)]
occ.femmine_2016=dataset[which(dataset$TIME==2016),c(1,10)]
occ.femmine_2017=dataset[which(dataset$TIME==2017),c(1,10)]
occ.femmine_2018=dataset[which(dataset$TIME==2018),c(1,10)]
occ.femmine_2019=dataset[which(dataset$TIME==2019),c(1,10)]
occ.femmine_2020=dataset[which(dataset$TIME==2020),c(1,10)]


occ.femmine_2011=occ.femmine_2011[order(occ.femmine_2011$Territorio),2]
occ.femmine_2012=occ.femmine_2012[order(occ.femmine_2012$Territorio),2]
occ.femmine_2013=occ.femmine_2013[order(occ.femmine_2013$Territorio),2]
occ.femmine_2014=occ.femmine_2014[order(occ.femmine_2014$Territorio),2]
occ.femmine_2015=occ.femmine_2015[order(occ.femmine_2015$Territorio),2]
occ.femmine_2016=occ.femmine_2016[order(occ.femmine_2016$Territorio),2]
occ.femmine_2017=occ.femmine_2017[order(occ.femmine_2017$Territorio),2]
occ.femmine_2018=occ.femmine_2018[order(occ.femmine_2018$Territorio),2]
occ.femmine_2019=occ.femmine_2019[order(occ.femmine_2019$Territorio),2]
occ.femmine_2020=occ.femmine_2020[order(occ.femmine_2020$Territorio),2]

I_occ.femmine = c(Moran.I(occ.femmine_2011,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.femmine_2012,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.femmine_2013,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.femmine_2014,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.femmine_2015,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.femmine_2016,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.femmine_2017,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.femmine_2018,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.femmine_2019,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                 Moran.I(occ.femmine_2020,proximity_0, scaled = FALSE, alternative = "two.sided")$observed)

I_occ.femmine
I_occ.femmine_finale=mean(I_occ.femmine)



#occupazione totale
occ_2011=dataset[which(dataset$TIME==2011),c(1,11)]
occ_2012=dataset[which(dataset$TIME==2012),c(1,11)]
occ_2013=dataset[which(dataset$TIME==2013),c(1,11)]
occ_2014=dataset[which(dataset$TIME==2014),c(1,11)]
occ_2015=dataset[which(dataset$TIME==2015),c(1,11)]
occ_2016=dataset[which(dataset$TIME==2016),c(1,11)]
occ_2017=dataset[which(dataset$TIME==2017),c(1,11)]
occ_2018=dataset[which(dataset$TIME==2018),c(1,11)]
occ_2019=dataset[which(dataset$TIME==2019),c(1,11)]
occ_2020=dataset[which(dataset$TIME==2020),c(1,11)]


occ_2011=occ_2011[order(occ_2011$Territorio),2]
occ_2012=occ_2012[order(occ_2012$Territorio),2]
occ_2013=occ_2013[order(occ_2013$Territorio),2]
occ_2014=occ_2014[order(occ_2014$Territorio),2]
occ_2015=occ_2015[order(occ_2015$Territorio),2]
occ_2016=occ_2016[order(occ_2016$Territorio),2]
occ_2017=occ_2017[order(occ_2017$Territorio),2]
occ_2018=occ_2018[order(occ_2018$Territorio),2]
occ_2019=occ_2019[order(occ_2019$Territorio),2]
occ_2020=occ_2020[order(occ_2020$Territorio),2]

I_occ = c(Moran.I(occ_2011,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                  Moran.I(occ_2012,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                  Moran.I(occ_2013,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                  Moran.I(occ_2014,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                  Moran.I(occ_2015,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                  Moran.I(occ_2016,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                  Moran.I(occ_2017,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                  Moran.I(occ_2018,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                  Moran.I(occ_2019,proximity_0, scaled = FALSE, alternative = "two.sided")$observed,
                  Moran.I(occ_2020,proximity_0, scaled = FALSE, alternative = "two.sided")$observed)

I_occ
I_occ_finale=mean(I_occ)

MORAN =c(I_madre_finale,I_padre_finale,I_fecondita_finale,
         I_maschi_finale,I_femmine_finale,I_inattivita_finale,
         I_occ.maschi_finale,I_occ.femmine_finale,I_occ_finale)
names(MORAN)=c("età madre","età padre","fecondità",
               "inattività maschi","inattività femmine","inattività totale",
               "occupazione maschi","occupazione femmine","occupazione totale")
MORAN

MORAN.ind=data.frame(MORAN)
file.create("MORAN_INDECES.csv")
write_csv(MORAN.ind,"MORAN_INDECES.csv")

save.image("MORAN_INDECES.RData")
load("MORAN_INDECES.RData")
