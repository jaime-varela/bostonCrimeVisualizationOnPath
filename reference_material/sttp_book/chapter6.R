#
# Code relating to material in Chapter 6. 
#
# load splancs package
#
library(splancs)
#
# load user-defined functions
#
source("functions.R")
#
# examples of scripts used to produce some of the diagrams in chapter 6
#
# Poisson cluster processes (cf Figure 6.1)
#
set.seed(1351)
xy<-PCP(100,25,0.025,poisson=FALSE)
par(pty="s",mfrow=c(1,2))
pointmap(xy,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
square<-matrix(c(0,0,1,0,1,1,0,1),4,2,byrow=TRUE)
polymap(square,add=TRUE)
#
set.seed(1351)
xy<-PCP(100,25,0.025,poisson=TRUE)
pointmap(xy,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
#
# inhomogeneous Poisson process (cf Figure 6.2)
#
set.seed(6891)
lambda<-function(x) {
   exp(-x[1]-2*x[2])
   }
xy<-IPP(100)
par(pty="s",mfrow=c(1,1))
pointmap(xy,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
#
# Poisson cluster/Cox process (cf Figure 6.3)
#
set.seed(1351)
xy<-PCP(100,25,0.05,poisson=T)
par(pty="s",mfrow=c(1,1))
pointmap(xy,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
#
# simple sequential inhibition (cf Figure 6.5)
#
set.seed(1351)
xy<-SSI(25,0.08)
par(pty="s",mfrow=c(2,2))
pointmap(xy,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
set.seed(1351)
xy<-SSI(50,0.08)
pointmap(xy,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
set.seed(1351)
xy<-SSI(75,0.08)
pointmap(xy,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
set.seed(1351)
xy<-SSI(100,0.08)
pointmap(xy,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
#
# pairwise interaction point processes (cf Figure 6.6)
#
par(pty="s",mfrow=c(2,2))
set.seed(1351)
h<-function(u,theta) {
#
# simple inhibition
#
   1*(u>theta)
   }
xy<-PIPP.h(100,0.05)
pointmap(xy,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
plot(c(0,0.05),c(0,0),type="l",xlab="u",ylab="h(u)",xlim=c(0,0.2),ylim=c(0,2))
lines(c(0.05,0.2),c(1,1))
lines(c(0.05,0.05),c(0,1),lty=2)
set.seed(1351)
h<-function(u,theta) {
#
# non-simple inhibition
#
   const<-1/(theta[2]-theta[1])
   0*(u<=theta[1])+(const*(u-theta[1]))*(u>theta[1])*(u<theta[2])+1*(u>=theta[2])
   }
theta<-c(0.05,0.1)
xy<-PIPP.h(100,theta)
pointmap(xy,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
u<-0.001*(0:200)
plot(u,h(u,theta),type="l",xlab="u",ylab="h(u)",xlim=c(0,0.2),ylim=c(0,2))
#
# pairwise interaction fixed-n point process with partly attractive interaction (cf Figure 6.7)
#
par(pty="s",mfrow=c(2,2))
h<-function(u,theta) {
#
# inhibition and attraction
#
   const<-(theta[3]-1)/(theta[2]-theta[1])
   0*(u<=theta[1])+(theta[3]-const*(u-theta[1]))*(u>theta[1])*(u<theta[2])+1*(u>=theta[2])
   #0*(u<=theta[1])+1.1*(u>theta[1])*(u<theta[2])+1*(u>=theta[2])
   }
set.seed(1351)
theta<-c(0.05,0.1,2.0)
xy2<-PIPP.h(100,theta,denom=2^12)
pointmap(xy2,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
u<-0.001*(0:200)
plot(u,h(u,theta),type="l",xlab="u",ylab="h(u)",xlim=c(0,0.2),ylim=c(0,2))
set.seed(1351)
theta<-c(0.01,0.1,2.0)
xy21<-PIPP.h(100,theta,denom=2^12)
pointmap(xy21,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
u<-0.001*(0:200)
plot(u,h(u,theta),type="l",xlab="u",ylab="h(u)",xlim=c(0,0.2),ylim=c(0,2))
#
# thinning of simple inhibition process (cf Figure 6.8)
#
set.seed(1351)
n.initial<-200
delta<-0.05
rho<-20
beta<-0.1
result<-THIN.simple(n.initial,delta,rho,beta)
par(pty="s",mfrow=c(1,1))
pointmap(result$xy,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
#
# two processses indistinguishable by their second-moment properties (cf Figure 6.9)
#
par(pty="s",mfrow=c(2,2))
set.seed(1351)
n<-100
rho<-20
sigma<-0.025
xy<-PCP(n,rho,sigma,poisson=TRUE)
n1<-60
n2<-n-n1
xy1<-cbind(runif(n1),runif(n1))
rho2<-rho*(n2/n)^2
xy2<-rbind(xy1,PCP(n2,rho2,sigma,poisson=TRUE))
pointmap(xy,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
pointmap(xy2,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
h<-0.01
grid.x<-h*(-0.5+(1:round(1/h)))
pts<-as.matrix(expand.grid(grid.x,grid.x))
u<-0.002*(0:125)
F<-Fhat(xy,pts,u)
G<-Ghat(xy,u)
F2<-Fhat(xy2,pts,u)
G2<-Ghat(xy2,u)
plot(u,F,type="l",xlab="x",ylab="F(x)",lty=2)
lines(u,F2)
plot(u,G,type="l",xlab="x",ylab="G(x)",lty=2)
lines(u,G2)
#
# inhomogeneous pairwise interaction point process (cf Figure 6.10)
#
par(pty="s",mfrow=c(1,1))
set.seed(1351)
h<-function(u,theta) {
#
# simple inhibition
#
   1*(u>theta)
   }
lambda<-function(x) {
   exp(-x[1]-2*x[2])
   }
n<-100
theta<-0.03
xy<-IPIPP.h(n,theta)
pointmap(xy,pch=19,cex=0.5,xaxt="n",yaxt="n",bty="n",xlim=c(0,1),ylim=c(0,1))
polymap(square,add=TRUE)
#
# mutually inhibitory pairwise interaction point processes (cf Figure 6.12)
#
h1<-function(u,theta1) {
      1*(u>theta1)
      }
h2<-function(u,theta2) {
      1*(u>theta2)
      }
h12<-function(u,theta12) {
      1*(u>theta12)
      }
set.seed(94681)
par(pty="s",mfrow=c(1,2))
n1<-100
n2<-100
theta1<-0.007
theta2<-0.007
theta12<-0.07
result<-PIPP.bivar.h(n1,n2,theta1,theta2,theta12) 
plot(result$xy1[,1],result$xy1[,2],pch=19,cex=0.5,xlim=c(0,1),ylim=c(0,1),xlab=" ",ylab=" ",
   xaxt="n",yaxt="n",bty="n")
points(result$xy2[,1],result$xy2[,2],pch=1,cex=0.5)
polymap(square,add=TRUE)
theta1<-0.07
theta2<-0.07
theta12<-0.007
result<-PIPP.bivar.h(n1,n2,theta1,theta2,theta12) 
plot(result$xy1[,1],result$xy1[,2],pch=19,cex=0.5,xlim=c(0,1),ylim=c(0,1),xlab=" ",ylab=" ",
   xaxt="n",yaxt="n",bty="n")
points(result$xy2[,1],result$xy2[,2],pch=1,cex=0.5)
polymap(square,add=TRUE)
   

