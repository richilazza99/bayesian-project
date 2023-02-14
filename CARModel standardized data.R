
load('CARModel_std.RData')

library(CARBayes)
library(CARBayesST)
library(coda)

dat = read.csv('Fecondita copia.csv')
dat=dat[-which(dat$Territorio=='Trentino Alto Adige / Südtirol'),]

W.comp = read.csv('Proximity_matrix_finale.csv')

w.prova = as.matrix(W.comp)
diag(w.prova) = 0
isSymmetric(w.prova) # False
colnames(w.prova) = rownames(w.prova)
isSymmetric(w.prova) # true
W.finale = w.prova

data.finale = dat[order(dat$Territorio),] # stesso ordine di W.finale
data.finale = data.finale[order(data.finale$TIME),] # ordine giusto per passarlo a carbayes

std.data = scale(data.finale[,-c(1,2)])

std.data.finale = cbind(data.finale[,1:2],std.data)


############### CAR Model ##############

mc.chain1 = ST.CARar(tasso.di.fecondità.totale ~ età.media.delle.madri.al.parto + 
                       tasso.di.inattività.delle.femmine +
                       tasso.di.occupazione.delle.femmine +
                       tasso.di.inattività.dei.maschi +
                       tasso.di.occupazione.dei.maschi, 
                     family = 'gaussian',data = std.data.finale, W = W.finale,
                     burnin =10000, n.sample = 20000, AR = 1)

mc.chain2 = ST.CARar(tasso.di.fecondità.totale ~ età.media.delle.madri.al.parto + 
                       tasso.di.inattività.delle.femmine +
                       tasso.di.occupazione.delle.femmine +
                       tasso.di.inattività.dei.maschi +
                       tasso.di.occupazione.dei.maschi, 
                     family = 'gaussian',data = std.data.finale, W = W.finale,
                     burnin =10000, n.sample = 20000, AR = 1)

mc.chain3 = ST.CARar(tasso.di.fecondità.totale ~ età.media.delle.madri.al.parto + 
                       tasso.di.inattività.delle.femmine +
                       tasso.di.occupazione.delle.femmine +
                       tasso.di.inattività.dei.maschi +
                       tasso.di.occupazione.dei.maschi, 
                     family = 'gaussian',data = std.data.finale, W = W.finale,
                     burnin =10000, n.sample = 20000, AR = 1)


beta.samples = mcmc.list(mc.chain1$samples$beta,mc.chain2$samples$beta,mc.chain3$samples$beta)
plot(beta.samples) # convergence

rho.samples = mcmc.list(mc.chain1$samples$rho,mc.chain2$samples$rho,mc.chain3$samples$rho)
plot(rho.samples) # convergence

tau2.samples = mcmc.list(mc.chain1$samples$tau2 ,mc.chain2$samples$tau2,mc.chain3$samples$tau2)
plot(tau2.samples) # convergence

nu2.samples = mcmc.list(mc.chain1$samples$nu2 ,mc.chain2$samples$nu2,mc.chain3$samples$nu2)
plot(nu2.samples) # convergence


mc.chain1$modelfit
# WAIC -51.301762

mc.chain2$modelfit
# WAIC -29.32123

mc.chain3$modelfit
# WAIC -39.66667

mc.chain1$summary.results

mc.chain4 = ST.CARar(tasso.di.fecondità.totale ~ età.media.delle.madri.al.parto + 
                       tasso.di.inattività.delle.femmine +
                       tasso.di.occupazione.delle.femmine +
                       tasso.di.inattività.dei.maschi +
                       tasso.di.occupazione.dei.maschi, 
                     family = 'gaussian',data = std.data.finale, W = W.finale,
                     burnin = 60, n.sample = 4500, AR = 1)


mc.chain4$modelfit # 15.866263







save.image('CARModel_std.RData')


