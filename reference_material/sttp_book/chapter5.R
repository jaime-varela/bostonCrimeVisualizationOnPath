#
# Code relating to material in Chapter 5. 
#
# Data-sets are available from www.lancs.ac.uk/staff/diggle/pointpatternbook
#
# load splancs package
#
library(splancs)
#
# add user-defined functions
#
source("functions.R") 
#
#
# Kernel smoothing of hickories and maples in Lansing woods (cf Figure 5.3)
#
xyh<-scan("../data/hickories")
xyh<-matrix(xyh,703,2,byrow=TRUE)
xym<-scan("../data/maples")
xym<-matrix(xym,514,2,byrow=TRUE)
poly<-matrix(c(0,0,1,0,1,1,0,1),4,2,byrow=TRUE)
#
# set bandwidth for splancs kernel smoothing function
#
h<-0.1
smooth.m<-kernel2d(xym,poly,h,nx=40,ny=40)
smooth.h<-kernel2d(xyh,poly,h,nx=40,ny=40)
pdf("lansing_kernel_smooth.pdf",height=6,width=8)
par(pty="s",mfrow=c(1,2))
pointmap(xyh,pch=19,cex=0.25,bty="n",xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1))
polymap(poly,add=T)
contour(smooth.h$x,smooth.h$y,smooth.h$z,add=T)
pointmap(xym,pch=19,cex=0.25,bty="n",xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1))
polymap(poly,add=T)
contour(smooth.m$x,smooth.m$y,smooth.m$z,add=T)
dev.off()
#
# bootstrap analysis of replicated point patterns (cf Figures 5.5 to 5.8, but note
# that data are slightly different to those used for the figures in the book)
#
data<-scan("../data/benes.txt"); data<-matrix(data,1398,2,byrow=TRUE)
poly<-matrix(c(0,0,1,0,1,1,0,1),4,2,byrow=TRUE)
ranges<-c(1,43,44,82,83,148,149,209,210,274,275,306,307,412,413,475, 
476,513,514,569,570,615,616,653,654,689,690,701,702,756,757,768,769,
822,823,901,902,962,963,1038,1039,1059,1060,1106,1107,1108,1109,1165, 
1166,1202,1203,1218,1219,1262,1263,1303,1304,1341,1342,1388,1389,1398)
ranges<-matrix(ranges,31,2,byrow=TRUE)
u<-0.01*(1:25)
khats<-matrix(0,31,25)
for (i in 1:31) {
   take<-ranges[i,]
   xy<-data[(take[1]:take[2]),]
   khats[i,]<-khat(xy,poly,u)
   }
npts<-ranges[,2]-ranges[,1]+1
#
# non-parametric estimation of mean K-function and bootstrap standard errors
#
# within control group 
#
Kmat.control<-khats[1:12,]
npts.control<-npts[1:12]
result<-K.boot(Kmat.control,u,npts.control,nboot=1000)
D.est<-result$mean - pi*u*u
D.SE<-result$SE
yrange<-c(min(D.est-2*D.SE),max(D.est+2*D.SE))
plot(u,D.est,type="l",xlab="u",ylab="Khat-pi*u*u",ylim=yrange) # plotting to screeen rather than to file
lines(u,D.est-2*D.SE,lty=2)
lines(u,D.est+2*D.SE,lty=2)
#
# within schizo-affective group
#
Kmat.affective<-khats[13:21,]
npts.affective<-npts[13:21]
result<-K.boot(Kmat.affective,u,npts.affective,nboot=1000)
D.est<-result$mean - pi*u*u
D.SE<-result$SE
yrange<-c(min(D.est-2*D.SE),max(D.est+2*D.SE))
plot(u,D.est,type="l",xlab="u",ylab="Khat-pi*u*u",ylim=yrange)
lines(u,D.est-2*D.SE,lty=2)
lines(u,D.est+2*D.SE,lty=2)
#
# within schizophrenia group
#
Kmat.schizo<-khats[13:21,]
npts.schizo<-npts[13:21]
result<-K.boot(Kmat.schizo,u,npts.schizo,nboot=1000)
D.est<-result$mean - pi*u*u
D.SE<-result$SE
yrange<-c(min(D.est-2*D.SE),max(D.est+2*D.SE))
plot(u,D.est,type="l",xlab="u",ylab="Khat-pi*u*u",ylim=yrange)
lines(u,D.est-2*D.SE,lty=2)
lines(u,D.est+2*D.SE,lty=2)
#
# between-group comparisons
#  	
Kmat<-khats
group<-c(rep(1,12),rep(2,9),rep(3,10))
nboot<-999
set.seed(43907)
result<-K.boot.anova(Kmat,u,npts,group,nboot=999)
plot(sort(result$boot),1:nboot,type="l")
points(result$test,sum(result$boot<=result$test),pch=19,col="red")
result$p
#
# 0.714
# 
