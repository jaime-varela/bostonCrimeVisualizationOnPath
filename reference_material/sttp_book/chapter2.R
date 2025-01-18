#
# Code relating to material in Chapter 2. 
#
# Data-sets are available from www.lancs.ac.uk/staff/diggle/pointpatternbook
#
# load splancs package
#
library(splancs)
#
# computing and plotting nearest neighbour distances
#
npts<-65
japanese<-matrix(scan("../data/japanese.txt"),npts,2,T)
pdf("japanese_nndistances.pdf",height=6,width=6)
par(pty="s")
poly<-matrix(c(0,0,1,0,1,1,0,1),4,2,T); polymap(poly); pointmap(japanese,pch=19,add=T)
d<-nndistG(japanese)$dists
range(d)
#[1] 0.0100000 0.1004988
dgrid<-0.001*(0:101)
ngrid<-length(dgrid)
Ghat<-rep(0,ngrid)
for (i in 1:ngrid) Ghat[i]<-sum(d<=dgrid[i])
G0<-1-exp(-npts*pi*dgrid*dgrid)
plot(G0,Ghat,type="l")
#
# simulation envelope for complete spatial randomness
#
nsim<-99
Gmat<-matrix(0,ngrid,1+nsim); Gmat[,1]<-Ghat
set.seed(90123)
for (isim in 1:nsim) {
   xy<-csr(poly,npts)
   di<-nndistG(xy)$dists
   for (i in 1:ngrid) Gmat[i,1+isim]<-sum(di<=dgrid[i])
   }
Gmax<-apply(Gmat[,2:(1+nsim)],1,max); Gmin<-apply(Gmat[,2:(1+nsim)],1,min)
lines(G0,Gmin,lty=2); lines(G0,Gmax,lty=2)
dev.off()
#
# Monte Carlo test
#
Gbar<-apply(Gmat,1,mean) # could use G0 in place of Gbar in  what follows - see Note
T<-sum((Ghat-Gbar)^2)
for (isim in 1:nsim) {
   T<-c(T,sum((Gmat[,1+isim]-Gbar)^2))
   }
pval<-sum(T>=T[1])/(1+nsim)
pval
# [1] 0.69
#
# Notes
# 1. Using either G0 or Gbar in the defintion of the statistic T should give similar
# results except if npts is small, in which case edge-effects may make a material difference.
# 2. A theoretical argument for using Gbar rather than Ghat is that under the null hypothesis,
# Gbar is an unbiased estimate of the expectation of Ghat, which in turn would be exactly
# equal to G0 in the absence of edge-effects, but otherwise only approximately so.
#
# Lansings woods data plots
#
hickories<-matrix(scan("../data/hickories"),703,2,T)
maples<-matrix(scan("../data/maples"),514,2,T)
oaks<-matrix(scan("../data/oaks"),929,2,T)
pdf("hickories.pdf",height=6,width=6)
par(pty="s")
plot(c(0,1,1,0,0),c(0,0,1,1,0),type="l",xlim=c(0,1),ylim=c(0,1),xlab=" ",ylab=" ",xaxt="n",yaxt="n",bty="n")
points(hickories,pch=19,cex=0.5)
dev.off()
pdf("maples.pdf",height=6,width=6)
par(pty="s")
plot(c(0,1,1,0,0),c(0,0,1,1,0),type="l",xlim=c(0,1),ylim=c(0,1),xlab=" ",ylab=" ",xaxt="n",yaxt="n",bty="n")
points(maples,pch=19,cex=0.5)
dev.off()
pdf("oaks.pdf",height=6,width=6)
par(pty="s")
plot(c(0,1,1,0,0),c(0,0,1,1,0),type="l",xlim=c(0,1),ylim=c(0,1),xlab=" ",ylab=" ",xaxt="n",yaxt="n",bty="n")
points(oaks,pch=19,cex=0.5)
dev.off()
