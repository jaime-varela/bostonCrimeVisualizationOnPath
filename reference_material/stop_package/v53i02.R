library("stpp")

## About stpp
## **********

?stpp

##
## Plotting spatio-temporal point patterns
## ***************************************
##

# Statistic display of the data consisting of locations in the 
# left-hand panel and the cumulative distribution of the times 
# in the right-hand panel


data("fmd")
data("northcumbria")
fmd <- as.3dpoints(fmd)
plot(fmd, s.region = northcumbria, pch = 19)

?as.3dpoints
# Static display in which the time is treated as a quantitative 
# mark attached to each location, and the locations are plotted 
# with the size and/or colour of the plotting symbol determined 
# by the value of the mark.

plot(fmd, s.region = northcumbria, pch = 19, mark = TRUE)

# Animation of a space-time point pattern (2D)

animation(fmd,runtime = 10,cex = 0.5,s.region = northcumbria)

# Dynamic display (3D)
# Require the packages rgl and rpanel

library(rgl)
library(rpanel)
stan(fmd, bgpoly = northcumbria, bgframe = FALSE)


##
## Space-time inhomogeneous K-function
## ***********************************
##

FMD <- as.3dpoints(fmd[,1]/1000, fmd[,2]/1000, fmd[,3])
Northcumbria <- northcumbria/1000

# Estimation of the temporal intensity

Mt <- density(FMD[ ,3], n = 1000)
mut <- Mt$y[findInterval(FMD[ ,3], Mt$x)] * dim(FMD)[1]

# Estimation of the spatial intensity

h <- mse2d(as.points(FMD[,1:2]), Northcumbria, nsmse = 50, range = 4)
h <- h$h[which.min(h$mse)]
Ms <- kernel2d(as.points(FMD[ ,1:2]), Northcumbria, h = h, nx = 2000, ny = 2000)
atx <- findInterval(x = FMD[ ,1], vec = Ms$x)
aty <- findInterval(x = FMD[ ,2], vec = Ms$y)
mhat <- NULL
for(i in 1:length(atx)) mhat <- c(mhat, Ms$z[atx[i],aty[i]])

# Estimation of the STIK function

u <- seq(0,10,by = 1)
v <- seq(0,15,by = 1)
stik <- STIKhat(xyt = FMD, s.region = Northcumbria, t.region = c(1,200),
   lambda = mhat*mut/dim(fmd)[1], dist = u, times = v, infectious = TRUE)

# Plotting the estimation using a contour plot

plotK(stik)
plotK(stik,legend = FALSE)

# Plotting the estimation using a perspective plot

plotK(stik,persp = TRUE, theta = -65, phi = 35)

# Estimation of the pair correlation function

g <- PCFhat(xyt = FMD, lambda = mhat * mut / dim(FMD)[1], dist = 1:20, times = 1:20, s.region = Northcumbria, t.region = c(1,200))

# Plotting the estimation using a contour plot

plotPCF(g)

# Plotting the estimation using a perspective plot

plotPCF(g, persp = TRUE, theta = -65, phi = 35)


##
## Simulation of homogeneous Poisson processes
## *******************************************
##

# Simulation within the unit cube and dynamic display 
# of the resulting pattern

hpp1 <- rpp(lambda = 200, nsim = 5, replace = FALSE)
stan(hpp1$xyt[[2]])

?stan

# Simulation within given spatial and temporal regions and 
# animation of the result pattern

data("northcumbria")
hpp2 <- rpp(npoints = 1000, s.region = northcumbria, t.region = c(1,500),
   discrete.time = TRUE)
animation(hpp2$xyt, s.region = hpp2$s.region)



##
## Simulation of inhomogeneous Poisson processes
## *********************************************
##

# Simulation within the unit cube, using a given density function 
# and dynamic display of the resulting pattern

lbda1 <- function(x,y,t,a){a*exp(-4*y) * exp(-2*t)}
ipp1 <- rpp(lambda = lbda1, npoints = 200, a = 1600/((1-exp(-4))*(1-exp(-2))))
stan(ipp1$xyt)

# Simulation within given spatial and temporal regions, using a 3D-array
# for the density and animation of the result pattern overimposed on the
# the intensity

data("fmd")
data("northcumbria")
h <- mse2d(as.points(fmd[,1:2]), northcumbria, nsmse = 30, range = 3000)
h <- h$h[which.min(h$mse)]
Ls <- kernel2d(as.points(fmd[,1:2]), northcumbria, h, nx = 100, ny = 100)
Lt <- dim(fmd)[1] * density(fmd[,3], n = 200)$y
nx <- 100
ny <- 100
nt <- 200
Lst <- array(0, dim = c(nx,ny,nt))
for(k in 1:nt) Lst[,,k] <- Ls$z*Lt[k]/dim(fmd)[1]
ipp2 <- rpp(lambda = Lst, s.region = northcumbria, t.region = c(1,200), discrete.time = TRUE)
image(Ls$x, Ls$y, Ls$z, col = grey((1000:1)/1000))
polygon(northcumbria)
animation(ipp2$xyt, add = TRUE, cex = 0.5, runtime = 15)

##
## Simulation of Poisson cluster processes
## ***************************************
##

# Simulation within given spatial and temporal regions
# and animation of the result pattern

data("northcumbria")
pcp1 <- rpcp(nparents=50, mc=10, s.region=northcumbria, t.region=c(1,365),
   cluster=c("normal","exponential"), dispersion=c(5000,5))
animation(pcp1$xyt, s.region=pcp1$s.region, t.region=pcp1$t.region, runtime=5)

# Simulation within the unit cube, using a given density function of parents
# and dynamic display of the resulting pattern

lbda <- function(x,y,t,a){a*exp(-4*y) * exp(-2*t)}
pcp2 <- rpcp(nparents=50, npoints=250, cluster="normal", lambda=lbda,
   a=2000/((1-exp(-4))*(1-exp(-2))))
stan(pcp2$xyt)


##
## Simulation of interaction processes
## ***********************************
##

# Inhibition processes
# --------------------

# Simulation within the unit cube and dynamic display of the result pattern

inh1 = rinter(npoints=200, thetas=0, deltas=0.05, thetat=0, deltat=0.001,
   inhibition=TRUE)
stan(inh1$xyt)

# Simulation using "hs" and "ht" functions spefied by the user
# and animation of the result pattern

hs = function(d,theta,delta,mus=0.1){
res=NULL
a=(1-theta)/mus
b=theta-a*delta
for(i in 1:length(d))
{	
 if (d[i]<=delta) res=c(res,theta)
	if (d[i]>(delta+mus)) res=c(res,1)
	if (d[i]>delta & d[i]<=(delta+mus)) res=c(res,a*d[i]+b)
}
return(res)}

ht = function(d,theta,delta,mut=0.3){
res=NULL
a=(1-theta)/mut
b=theta-a*delta
for(i in 1:length(d))
{	
	if (d[i]<=delta) res=c(res,theta)
	if (d[i]>(delta+mut)) res=c(res,1)
	if (d[i]>delta & d[i]<=(delta+mut)) res=c(res,a*d[i]+b)
}
return(res)}

inh2 = rinter(npoints=100, hs=hs, gs="min", thetas=0.2, deltas=0.1, ht=ht,
   gt="min", thetat=0.1, deltat=0.05, inhibition=TRUE)
animation(inh2$xyt,runtime=15,cex=0.8)


# Contagious process
# Simulation within given spatial and temporal regions
# and static display and animation of the result pattern

data("northcumbria")
cont1 = rinter(npoints=250, s.region=northcumbria, t.region=c(1,200), thetas=0,
   deltas=7500, thetat=0, deltat=10, recent=1, inhibition=FALSE)
plot(cont1$xyt, pch=19, s.region=cont1$s.region, mark=TRUE, mark.col=4)
animation(cont1$xyt, s.region=cont1$s.region, t.region=cont1$t.region,
   incident="red", prevalent="lightgreen", runtime=15, cex=0.8)


##
## Simulation of infectious processes
## **********************************
##

# Simulation within a given temporal region
# and animation of the result pattern

inf1 = rinfec(npoints=100, alpha=0.1, beta=0.6, gamma=0.5,
   maxrad=c(0.075,0.5), t.region=c(0,50), s.distr="uniform",
   t.distr="uniform", h="step", g="min", recent="all", inhibition=TRUE)
animation(inf1$xyt, cex=0.8, runtime=10)

# Simulation within given spatial and temporal regions using 
# a given spatial intensity function and animation of the result pattern
# overimosed on the intensity

data("fmd")
data("northcumbria")
h = mse2d(as.points(fmd[,1:2]), northcumbria, nsmse=30, range=3000)
h = h$h[which.min(h$mse)]
Ls = kernel2d(as.points(fmd[,1:2]), northcumbria, h, nx=50, ny=50)
inf2 = rinfec(npoints=100, alpha=4, beta=0.6, gamma=20, maxrad=c(12000,20),
   s.region=northcumbria, t.region=c(1,2000), s.distr="poisson", t.distr="uniform",
   h="step", g="min", recent=1, lambda=Ls$z, inhibition=FALSE)
image(Ls$x, Ls$y, Ls$z, col=grey((1000:1)/1000)); polygon(northcumbria,lwd=2)
animation(inf2$xyt, add=TRUE, cex=0.7, runtime=15)


##
## Simulation of Log-Gaussian Cox processes
## ****************************************
##

# Simulations and animations of the result pattern overimposed 
# on the intensity

lgcp1 <- rlgcp(npoints=200, nx=50, ny=50, nt=50, separable=FALSE,
   model="gneiting", param=c(1,1,1,1,1,2), var.grf=1, mean.grf=0)
N <- lgcp1$Lambda[,,1]
for(j in 2:(dim(lgcp1$Lambda)[3])){N <- N+lgcp1$Lambda[,,j]}
image(N,col=grey((1000:1)/1000));box()
animation(lgcp1$xyt, cex=0.8, runtime=10, add=TRUE, prevalent="orange")

lgcp2 <- rlgcp(npoints=200, nx=50, ny=50, nt=50, separable=TRUE,
   model="exponential",param=c(1,1,1,1,1,2), var.grf=2, mean.grf=-0.5*2)
N <- lgcp2$Lambda[,,1]
for(j in 2:(dim(lgcp2$Lambda)[3])){N <- N+lgcp2$Lambda[,,j]}
image(N,col=grey((1000:1)/1000));box()
animation(lgcp2$xyt, cex=0.8, runtime=10, add=TRUE, prevalent="orange")



