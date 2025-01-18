#
# Code relating to material in Chapter 4. 
#
# Data-sets are available from www.lancs.ac.uk/staff/diggle/pointpatternbook
#
# load splancs package
#
#library(splancs)
#
# add user-defined functions
#
source("functions.R") 
#
# Comparison between histogram-like and kernel-smoothed estimate of the pair 
# correlation function, based on a simulated realisation of a Poison process 
# with 500 events on the unit square   
# 
poly<-matrix(c(0,0,1,0,1,1,0,1),4,2,T)
n<-500
u<-0.05*(1:10)
pdf("paircorr_plot.pdf",height=4,width=6)
#
# draw horizontal line y=1
#
plot(c(-1,u),rep(1,1+length(u)),type="l",xlim=c(0,max(u)),ylim=c(0.95,1.1),xlab="u",ylab="r(u)")
#
# simulate realisation of complete spatial randomness
#
set.seed(9451)
pts<-csr(poly,n)
#
# compute histogram-like estimate of pair correlation function via Ripley's K-function
#
h.hist<-0.05
rhist<-rho.hist(pts,poly,10,h.hist)
#
# plot result (mildly tedious)
#
x<-c(rhist$u-0.5*h.hist,rhist$u[10]+0.5*h.hist)
x<-c(matrix(x,2,11,byrow=TRUE))
y<-rhist$r
y<-c(matrix(y,2,10,byrow=TRUE))
y<-c(0,y,0)
lines(x,y,lwd=2)
#
# compute and plot kernel-smoothed estimate of pair correlation function
#
h.kernel<-h.hist*sqrt(7/12)
rkernel<-rho.kernel(pts,poly,0.5,h.kernel)
lines(rkernel$u,rkernel$r,lwd=2,lty=2)
#
# repeat with bigger band-wdith
#
h.kernel<-2*h.hist*sqrt(7/12)
rkernel<-rho.kernel(pts,poly,0.5,h.kernel)
lines(rkernel$u,rkernel$r,lwd=2,lty=3)
dev.off()
#
# estimated K-functions for the amacrine cell data (plotted in colour)
#
on<-scan("../data/amacrines.on.d")
on<-matrix(on,152,2,byrow=TRUE)
off<-scan("../data/amacrines.off.d")
off<-matrix(off,142,2,byrow=TRUE)
rectangle<-matrix(c(0,0,1.6,0,1.6,1,0,1),4,2,byrow=TRUE)
u<-0.002*(1:125)
K11<-khat(on,rectangle,u)
K22<-khat(off,rectangle,u)
K12<-k12hat(on,off,rectangle,u)
Kall<-khat(rbind(on,off),rectangle,u)
K0<-pi*u*u
pdf("K_amacrines.pdf",height=6,width=8)
plot(u,K11-K0,type="l",lwd=2,ylim=c(-0.015,0.005),xlab="u",ylab="Khat-K0")
lines(u,K22-K0,lty=2,lwd=2)
lines(u,Kall-K0,lwd=2,col="blue")
lines(u,K12-K0,lwd=2,col="red")
lines(u[1:30],-K0[1:30])
legend(0,-0.01,lwd=rep(2,4),lty=c(1,2,1,1),col=c("black","black","blue","red"),
   legend=c("on","off","all","cross"))
dev.off()











    