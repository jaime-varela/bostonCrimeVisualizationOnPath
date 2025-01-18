#
# Code relating to material in Chapter 3. 
#
# load splancs package
#
library(splancs)
#
# code to create postscript versions of Figures 3.1, 3.2, 3.3
#
xy<-c(0.13,0.21,0.36,0.49,0.49,0.86,0.9,0.50,0.72,0.05,1.72,
      0.28,1.65,0.57,1.31,0.5,1.25,0.79,1.68,0.69,1.78,0.92)
xy<-matrix(xy,11,2,byrow=TRUE)
rad<-sqrt(sum((xy[4,]-xy[8,])^2))
xc<-xy[4,1]+rad*cos(2*pi*(0:360)/360)
yc<-xy[4,2]+rad*sin(2*pi*(0:360)/360)
rad2<-sqrt(sum((xy[8,]-xy[9,])^2))
xc2<-xy[8,1]+rad2*cos(2*pi*(0:360)/360)
yc2<-xy[8,2]+rad2*sin(2*pi*(0:360)/360)
postscript("figure3.1.ps",height=8,width=8,paper="special")
par(pty="s")
pointmap(cbind(xy[,1],xy[,2]),pch=19,cex=1.5,xaxt="n",yaxt="n",bty="n",xlab=" ",ylab=" ",
   xlim=c(0,2),ylim=c(0,1))
lines(xc,yc)
lines(xc2,yc2)
lines(c(xy[4,1],xy[8,1]),c(xy[4,2],xy[8,2]),lwd=3)
lines(c(xy[4,1],xc[43]),c(xy[4,2],yc[43]),lwd=3)
lines(c(xy[8,1],xc[43]),c(xy[8,2],yc[43]),lwd=3)
xpoly<-c(xc[319:361],xc[2:43],xc2[112:1],xc2[361:250])
ypoly<-c(yc[319:361],yc[2:43],yc2[112:1],yc2[361:250])
polygon(xpoly,ypoly,density=10)
text(0.9,0.45,"O")
text(1.28,0.45,"P")
text(1.24,0.85,"Q")
text(1.1,0.45,"x")
text(1.22,0.63,"y")
dev.off()
#
rad3<-sqrt(sum((xy[7,]-xy[8,])^2))
xc3<-xy[8,1]+rad3*cos(2*pi*((-90):90)/360)
yc3<-xy[8,2]+rad3*sin(2*pi*((-90):90)/360)
postscript("figure3.2.ps",height=8,width=8,paper="special")
par(pty="s")
pointmap(cbind(xy[,1],xy[,2]),pch=19,cex=1.5,xaxt="n",yaxt="n",bty="n",xlab=" ",ylab=" ",
   xlim=c(0,2),ylim=c(0,1))
lines(xc,yc)
lines(xc3,yc3)
lines(c(xy[8,1],xy[8,1]),c(0,1),lwd=2,lty=2)
lines(c(xy[4,1],xy[8,1]),c(xy[4,2],xy[8,2]),lwd=3)
lines(c(xy[7,1],xy[8,1]),c(xy[7,2],xy[8,2]),lwd=3)
text(0.9,0.45,"O")
text(1.28,0.45,"P")
text(1.1,0.45,"x")
text(1.47,0.58,"z")
lines(xy[8,1]-c(0.08,0.08,0),xy[8,2]+c(0,0.08,0.08))
dev.off()
xy<-c(0.06,0.49,0.53,0.39,0.84,0.35,1.33,0.63,1.82,0.69)
xy<-matrix(xy,5,2,byrow=TRUE)
mask<-cbind(c(0.4,0,0.4),c(-0.4,0,0.4))
xc<-cos(2*pi*((-45):45)/360)
yc<-sin(2*pi*((-45):45)/360)
postscript("figure3.3.ps",height=8,width=8,paper="special")
par(pty="s")
pointmap(xy[2:5,],pch=19,cex=1.5,bty="n",xaxt="n",yaxt="n",xlab=" ",ylab=" ",
xlim=c(0,2),ylim=c(0,1))
lines(xy[,1],xy[,2],lwd=3)
for (i in 1:4) {
   lines(xy[i,1]+mask[,1],xy[i,2]+mask[,2],lwd=2,lty=2)
   rad<-sqrt(sum((xy[i,]-xy[(i+1),])^2))
   lines(xy[i,1]+rad*xc,xy[i,2]+rad*yc)
   }
lines(xy[5,1]+mask[,1],xy[5,2]+mask[,2],lwd=2,lty=2)
xy2<-c(0.13,0.25,0.82,0.04,1.09,0.04,2.01,0.17,
       1.94,1.20,1.16, 0.93,0.46,1.24,0.08,0.93)
xy2<-matrix(xy2,8,2,byrow=TRUE)
pointmap(xy2,add=TRUE,pch=19,cex=1.5)
text(0,0.5,"O")
dev.off()









