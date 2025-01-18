#
# Code relating to material in Chapter 1. 
#
# Data-sets are available from www.lancs.ac.uk/staff/diggle/pointpatternbook
#
# load splancs package
#
library(splancs)
#
# plotting the amacrines data
#
on<-scan("../data/amacrines.on.d")
on<-matrix(on,152,2,byrow=TRUE)
off<-scan("../data/amacrines.off.d")
off<-matrix(off,142,2,byrow=TRUE)
pdf("amacrines.pdf",height=6,width=6,paper="special")
par(pty="s")
plot(on[,1],on[,2],type="n",xaxt="n",yaxt="n",bty="n",xlab=" ",ylab=" ",xlim=c(0,1.6),ylim=c(0,1.6))
points(on[,1],on[,2],pch=19,cex=0.7)
points(off[,1],off[,2],pch=1,cex=0.7)
rectangle<-matrix(c(0,0,1.6,0,1.6,1,0,1),4,2,byrow=TRUE)
polymap(rectangle,add=TRUE)
dev.off()
#
# plotting (some of) the AEGISS data
#
data<-read.table("../data/AEGISS_ixyt.txt",sep=" ",header=TRUE)
x<-data$x[1:100]
y<-data$y[1:100]
day<-data$t[1:100]
range(x); range(y); range(day)
#
#[1] 416250 487950
#[1]  93850 162050
#[1] 1 8
#
border<-read.table("../data/AEGISS_poly.txt",sep=" ",header=FALSE)
xp<-border[,1]
yp<-border[,2]
range(xp); range(yp)
#
#[1] 402843.2 488625.3
#[1]  90723.34 165566.75
#
pdf("AEGISS_first100.pdf",height=6,width=6)
par(pty="s")
plot(c(xp,xp[1]),c(yp,yp[1]),xlim=c(400000,500000),ylim=c(80000,180000),
   xlab="E-W",ylab="N-S",type="l")
intercept<-0.01; scale<-0.05; radii<-intercept+scale*day; maxradius<-0.05
symbols(x,y,circles=radii,inches=maxradius,add=T)
dev.off()
#
# Dirichlet tessellation
#
library(deldir)
set.seed(914)
unitsquare<-matrix(c(0,0,1,0,1,1,0,1),4,2,T)
xy<-csr(unitsquare,10)
pdf("tessellation.pdf",height=6,width=6)
par(pty="s")
plot(c(0,1,1,0,0),c(0,0,1,1,0),type="l",bty="n",xaxt="n",yaxt="n",xlab=" ",ylab=" ")
pointmap(xy,add=T,pch=19)
tess<-deldir(xy[,1],xy[,2],rw=c(0,1,0,1))
plot.deldir(tess,add=T,wlines="both",lty=c(2,1))
dev.off()
