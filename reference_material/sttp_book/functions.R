# 
# Various functions written by PJD...almost certainly inefficiently coded, but required by some of the 
# chapter-specific files (chapter1.R, chapter2.R,...)
#
# splancs includes a function for estimating Ripley's Khat from data on an arbitrary
# polygon. The two functions below are included here to show specifically how the edge-corrections
# are computed, as this might be useful for incorporation 
# into other used-defined functions
#
ripley<-function(x,u,a=1,b=1) {
#
# Ripley's edge-correction for a point pattern on a rectangle, (0,a)X(0,b)
#
# Arguments
#
#     x: (x1,x2), a point in the rectangle
#     u: a vector of positive values, each in the range 0<u<=0.5
#   a,b: side-lengths of the rectangle (default values a=b=1)
#
# Result
#
#   A vector of weights, w, of the same length as u. Each element, w[i], is the 
#   proportion of the circumference of the circle with centre x and radius u[i] 
#   that lies within the rectangle (0,a)X(0,b)
#
   m<-length(u)
   w<-rep(1,m)
   if ((min(x)<0)|(x[1]>a)|(x[2]>b)) {
      print("Error: x not in rectangle")
      stop()
      }
   if ((min(u)<0)|(max(u)>0.5)) {
      print("Error: u outside range 0 to 0.5")
      stop()
      }
   d1<-min(c(x[1],a-x[1]))
   d2<-min(c(x[2],b-x[2]))
   dmin<-min(c(d1,d2))
   dsq<-d1*d1+d2*d2
   take1<-((u*u)<=dsq)&(u>dmin)
   if (sum(take1)>0) {
      ut1<-u[take1]
      arg1<-d1/ut1
      arg1[arg1>1]<-1
      arg2<-d2/ut1
      arg2[arg2>1]<-1
      w[take1]<-1-(acos(arg1)+acos(arg2))/pi
      }
   take2<-((u*u)>dsq)
   if (sum(take2)>0) {
      ut2<-u[take2]
      w[take2]<-0.75-(acos(d1/ut2)+acos(d2/ut2))/(2*pi)
      }
   w
   }

khat.rectangle<-function(u,pts,a=1,b=1) {
#
# Ripley's Khat for a point pattern on a rectangle, (0,a)X(0,b)
#
# Arguments
#
#     pts: an n by 2 matrix of locations in the rectangle
#     u: a vector of positive values, each in the range 0 to 0.5
#   a,b: side-lengths of the rectangle (default values a=b=1)
#
# Result
#
#  A vector of estimates Khat for each value of u 
#
   if ((min(u)<0)|(max(u)>0.5)) {
      print("Error: u outside range 0 to 0.5")
      stop()
      }
   m<-length(u)
   khat<-rep(0,m)
   if (u[1]>0) start<-1 else start<-1
   uvec<-u[start:m] 
   n<-dim(pts)[1]
   A<-a*b
   for (i in 1:n) {
      dvec<-sqrt((pts[i,1]-pts[-i,1])^2 + (pts[i,2]-pts[-i,2])^2)
      Imat<-outer(dvec,uvec,"<=")
      wvec<-rep(1,n-1)
      take<-dvec<=0.5
      wvec[take]<-ripley(pts[i,],dvec[take],a,b)
      khat[start:m]<-khat[start:m]+apply(Imat/matrix(wvec,n-1,m+1-start,byrow=T),2,sum)
      }
   A*khat/(n*(n-1))
   }

rho.hist<-function(pts,poly,nu,hu) {
#
# Histogram-like estimate of the pair correlation function
# for a point pattern on an arbitrary polygon
#
# Requires the splancs library
#
# Arguments
#
#      pts: an n by 2 matrix of locations
#     poly: a polygon containing all of the locations in pts
#       nu: a positve integer
#       hu: a positve real number
# Result
#
#  A list with elements:
#      $u: a vector of values u=0.5+hu*j: j=0,1,...,nu-1
#      $r: estimates of the pair correlation function
#          at distance bins centred on the values u
#
   u<-hu*(0:nu)
   if (max(u)>0.5) {
      print("Error: u outside range 0 to 0.5")
      stop()
      }
   rhat<-khat(pts,poly,u)
   u<-hu*(0.5+(0:(nu-1)))
   rhat<-(2*pi*u)^{-1}*(rhat[-1]-rhat[-length(rhat)])/hu
   list(u=u,r=rhat)
   }

rho.kernel<-function(pts,poly,umax,h,increment=h/10) {
#
# Kernel estimate of the pair correlation function
# for a point pattern on an arbitrary polygon,using
# a quartic kernel, with reflection in the y-axis
#
# Requires the splancs library
#
# Arguments
#
#         pts: an n by 2 matrix of locations
#        poly: a polygon containing all of the locations in pts
#        umax: maximum distance over which estimates are required
#           h: bandwidth for the kernel smoother
#   increment: plotting increment for the estimate (defaults to h/10)

# Result
#
#  A list with elements:
#      $u: a vector of distances, approximately spanning the interval (0,umax),
#          at which estimates are calculated
#      $r: estimates of the pair correlation function
#          at distances u
#
   kernel<-function(x) {
      nk<-length(x)
      kernel<-rep(0,nk)
      take<-abs(x)<1
      kernel[take]<-(15/16)*((1-x[take]^2)^2)
      kernel
      }
   u<-increment*(0:floor(umax/increment))
   if (max(u)>0.5) {
      print("Error: u outside range 0 to 0.5")
      stop()
      }
   nu<-length(u)
   rtilde<-rho.hist(pts,poly,nu-1,increment)
   ur<-rtilde$u
   rr<-rtilde$r
   nr<-length(rr)
   rhat<-rep(0,nr)
   for (i in 1:nr) {
      weights<-(1/h)*(kernel((ur-ur[i])/h)+kernel((-ur-ur[i])/h))
      weights<-weights/sum(weights)
      rhat[i]<-sum(rr*weights)
      }
   list(u=ur,r=rhat)
   }
 
K.boot<-function(Kmat,u,npts,nboot=0) {
#
# Arguments:
#    Kmat: matrix of estimated K-functions, with rows and columns corresponding to
#          distances and replicates, respectively
#       u: vector of distances used for each estimated K-function (not used, returned 
#          as part of result for convenience)
#    npts: vector of numbers of events in each replicate point pattern
#   nboot: number of bootstrap re-samples for standard error estimation
#
# Result:
#   list with components:
#        mean: estimate of K-function (weighted average of estimates frdm each replicate)
#          SE: estimated standard error at each distance
#           u: vector of distances 
#   residuals: matrix of standardised residual K-functions
#
   result<-NULL
   rc<-dim(Kmat); r<-length(npts); c<-rc[2]
   if (r!=rc[1]) print("STOP: incompatible arguments")
   else {
      mat.npts<-matrix(npts,r,c)
      Kmean<-apply(mat.npts*Kmat,2,sum)/sum(npts)
	  rmat<-sqrt(mat.npts)*(Kmat-matrix(Kmean,r,c,byrow=TRUE))
      Kvar<-NULL
      if (nboot>0) {
         boot<-sample(1:r,nboot,replace=TRUE)
         rstar<-rmat[boot,]
	     Kstar<-matrix(Kmean,nboot,c,byrow=TRUE)+rstar/sqrt(matrix(npts[boot],nboot,c))
		 Kvar<-apply(Kstar,2,var)
		 }
      result<-list(mean=Kmean,SE=sqrt(Kvar/r),u=u,residuals=rmat)
	  }
   result
   }

 K.boot.anova<-function(Kmat,u,npts,group,nboot=999,weight.power=-2) {
#
# Arguments:
#           Kmat: matrix of estimated K-functions, with rows and columns corresponding to
#                 distances and point patterns, respectively
#              u: vector of distances used for each estimated K-function (not used, returned 
#                 as part of result for convenience)
#           npts: vector of numbers of events in each  point pattern
#          group: vector of indicators of group membership 
#          nboot: number of bootstrap re-samples for standard error estimation
#   weight.power: power of distance used in integral of weighted squared difference between
#                 overall and group-specific K-functions
#
# Result:
#   list with components:
#   overall.mean: estimate of K-function assuming no group differences
#   group.means: estimates of group-specific K-functions
#   test.statistic: ANOVA-style test statistic
#   bootstrap: bootstrapped versions of test statistic
#   p.value: p-value of test for differences between groups
#        mean: estimate of K-function (weighted average of estimates from each replicate)
#          SE: estimated standard error at each distance
#           u: vector of distances 
#   residuals: matrix of standardised residual K-functions
#
w<-function(u) {
    u^weight.power
    }
    weight<-w(u)
	result<-NULL
    rc<-dim(Kmat); r<-length(npts); c<-rc[2]
    if (r!=rc[1]) print("STOP: incompatible arguments")
    else {
        rmat<-matrix(0,r,c)
		ug<-unique(group); ng<-length(ug); n.per.group<-rep(0,ng)
        Kmean.group<-matrix(0,ng,length(u))
        for (g in 1:ng) {
		    take<-group==ug[g]
		    result<-K.boot(Kmat[take,],u,npts[take])
			Kmean.group[g,]<-result$mean
			n.per.group[g]<-sum(npts[take])
			rmat[take,]<-result$residuals
			}
	    Kmean.all<-apply(matrix(n.per.group,g,c)*Kmean.group,2,sum)/sum(n.per.group)
		stat<-matrix(weight,ng,c,byrow=TRUE)*((Kmean.group-matrix(Kmean.all,ng,c,byrow=TRUE))^2)
		stat<-sum(n.per.group*apply(stat,1,sum))
		for (iboot in 1:nboot) {
            boot<-sample(1:r,r,replace=TRUE)
            rstar<-rmat[boot,]
	        Kstar<-matrix(Kmean.all,r,c,byrow=TRUE)+rstar/sqrt(matrix(npts,r,c))
			Kmean.star.group<-matrix(0,ng,length(u))
            for (g in 1:ng) {
		        take<-group==ug[g]
		        result<-K.boot(Kstar[take,],u,npts[take])
			    Kmean.star.group[g,]<-result$mean
				}
	        Kmean.star.all<-apply(matrix(n.per.group,g,c)*Kmean.star.group,2,sum)/sum(n.per.group)
			stat.boot<-matrix(weight,ng,c,byrow=TRUE)*
			                        ((Kmean.star.group-matrix(Kmean.star.all,ng,c,byrow=TRUE))^2)
		    stat.boot<-sum(n.per.group*apply(stat.boot,1,sum))
			stat<-c(stat,stat.boot)
		    }
	    p<-sum(stat>=stat[1])/(1+nboot)
		}
	result<-list(overall.mean=Kmean.all,group.means=Kmean.group,test.statistic=stat[1],
	                       bootstrap=stat[-1],p.value=p)
	}
	
PCP<-function(n,rho,sigma,a=1,b=1,poisson=TRUE) {
#
# Simulation of Poisson cluster process on rectangle (0,a)X(0,b), 
# with toroidal wrapping
#
# Arguments
#
#          n: number of events 
#        rho: mean number of parents per unit area
#     sigma: standard deviation of Gaussian dispersion of offspring relative 
#            to their parents in either coordinate direction
#      a, b: dimensions of rectangle
#   poisson: if TRUE, offspring are randomly distributed amongst parents,
#            if false the number of offspring per parent is constant (with
#            adjustment to n if n/(rho*a*b) is non-integer)
#
# Result
#
#   A two-column matrix of event locations (x,y)
#
   m<-round(rho*a*b)
   if (poisson==FALSE) {
      k<-round(n/m)
      n<-m*k
      } 
   xp<-a*runif(m)
   yp<-b*runif(m)
   if (poisson==FALSE) parent<-rep(1:m,k) else parent<-sample(1:m,n,replace=TRUE)
   xoff<-xp[parent]+sigma*rnorm(n)
   yoff<-yp[parent]+sigma*rnorm(n)
   take<-(xoff>a)
   if (sum(take)>0) {
      xoff[take]<-xoff[take]-a*floor(xoff[take]/a)
      }
   take<-(yoff>b)
   if (sum(take)>0) {
      yoff[take]<-yoff[take]-b*floor(yoff[take]/b)
      }
   take<-(xoff<0) 
   if (sum(take)>0) {
      xoff[take]<-a+xoff[take]+a*ceiling(xoff[take]/a)
      }
   take<-(yoff<0)
   if (sum(take)>0) {
      yoff[take]<-b+yoff[take]+b*ceiling(yoff[take]/b)
      }
   cbind(xoff,yoff)
   }

IPP<-function(n,bound=1,a=1,b=1) {
#
# Simulation of inhomogeneous Poisson process on rectangle (0,a)X(0,b) 
#
# Arguments
#
#             n: number of events 
#         bound: upper bound for the intensity (see below)
#          a, b: dimensions of rectangle
#  
# Result
#
#   A two-column matrix of event locations (x,y)
#
#
# Note: requires a user-defined function whose syntax must be of the form
#       lambda(x) where x is any location in the rectangle (0,a)X(0,b)
#       and the function returns a non-negative value proportional to the intensity
#       at x and bounded above by the value of bound
   xy<-matrix(0,1,2)
   while (dim(xy)[1]<(n+1)) {
      x<-a*runif(1)
      y<-b*runif(1)
      prob<-lambda(c(x,y))/bound
      if (runif(1)<prob) xy<-rbind(xy,c(x,y))
      }
   xy[-1,]
   }

SSI<-function(n,delta,a=1,b=1) {
#
# Simulation of simple sequential inhibition process on rectangle (0,a)X(0,b), 
#
# Arguments
#
#       n: number of events 
#   delta: minimum permissible distance between any two events
#    a, b: dimensions of rectangle
#  
# Result
#
#   A two-column matrix of event locations (x,y)
#
   xy<-matrix(c(a,b)*runif(2),1,2)
   delsq<-delta*delta
   while (dim(xy)[1]<n) {
      dsq<-0
      while (dsq<delsq) {
         x<-a*runif(1)
         y<-b*runif(1)
         dsq<-min((xy[,1]-x)^2+(xy[,2]-y)^2)
         }
      xy<-rbind(xy,c(x,y))
      }
   xy
   }

PIPP.h<-function(n,theta,denom=1,sweep=4,a=1,b=1) {
#
# Simulation of pairwise interaction point  process on rectangle (0,a)X(0,b), 
#
# Arguments
#
#       n: number of events 
#   theta: parameters of the interaction function (see below)
#   denom: the denominator used for rejection sampling (see below)
#   sweep: number of sweeps of the MCMC algorithm
#    a, b: dimensions of rectangle
#  
# Result
#
#   A two-column matrix of event locations (x,y)
#
# Note: this function calls a user-defined interaction function whose syntax must
#       be of the form h<-function(u,theta), where u is a vector of non-negative 
#       real numbers, and whose result must be a vector of non-negative real numbers
#       with the same length as u. If h(u,theta) can take values greater than 1,
#       denom needs to be at least as large as the maximum achievable value for the
#       product of the interactions between a candidate location and  all current
#       event locations. Computation can then be very slow
#
   xy<-cbind(a*runif(n),b*runif(n))
   for (s in 1:sweep) {
      accept<-FALSE
      for (i in 1:n) {
         while (accept==FALSE) {
            x<-a*runif(1)
            y<-b*runif(1)
            u<-sqrt((x-xy[,1])^2+(y-xy[,2])^2)
            prob<-prod(h(u,theta))/denom
            accept<-runif(1)<prob
            }
         accept<-FALSE
         xy[i,]<-c(x,y)
         }
      }
   xy
   }

PIPP.bivar.h<-function(n1,n2,theta1,theta2,theta12,denom=1,sweep=4,a=1,b=1) {
#
# Simulation of bivariate pairwise interaction point  process on rectangle (0,a)X(0,b), 
#
# Arguments
#
#      n1: number of type 1 events 
#      n2: number of type 2 events
#  theta1: parameters of the interaction function for type 1 events (see below)
#  theta2: parameters of the interaction function for type 2 events (see below)
# theta12: parameters of the cross-interaction function (see below)
#   denom: the denominator used for rejection sampling (see below)
#   sweep: number of sweeps of the MCMC algorithm
#    a, b: dimensions of rectangle
#  
# Result
#
#   A list of two, two-column matrics of event locations (x,y) for type 1 and type 2 events
#
# Note: this function calls three user-defined interaction functions whose syntax must
#       be of the form h1<-function(u,theta1), h2<-function(u,theta2) and
#       h12<-function(u,theta12), where u is a vector of non-negative 
#       real numbers, and whose result must be a vector of non-negative real numbers
#       with the same length as u. If any of h1(), h2() or h12() can return values greater 
#       than 1, denom needs to be at least as large as the maximum achievable value for the
#       product of the interactions between a candidate location and  all current
#       event locations. Computation can then be very slow
#
   xy1<-cbind(a*runif(n1),b*runif(n1))
   xy2<-cbind(a*runif(n2),b*runif(n2))
   for (s in 1:sweep) {
      accept<-FALSE
      for (i in 1:n1) {
         while (accept==FALSE) {
            x<-a*runif(1)
            y<-b*runif(1)
            u11<-sqrt((x-xy1[,1])^2+(y-xy1[,2])^2)
            u12<-sqrt((x-xy2[,1])^2+(y-xy2[,2])^2)
            prob<-prod(h1(u11,theta1))*prod(h12(u12,theta12))/denom
            accept<-runif(1)<prob
            }
         accept<-FALSE
         xy1[i,]<-c(x,y)
         }
      accept<-FALSE
      for (i in 1:n2) {
         while (accept==FALSE) {
            x<-a*runif(1)
            y<-b*runif(1)
            u12<-sqrt((x-xy1[,1])^2+(y-xy1[,2])^2)
            u22<-sqrt((x-xy2[,1])^2+(y-xy2[,2])^2)
            prob<-prod(h2(u22,theta2))*prod(h12(u12,theta12))/denom
            accept<-runif(1)<prob
            }
         accept<-FALSE
         xy2[i,]<-c(x,y)
         }
      }
   list(xy1=xy1,xy2=xy2)
   }

THIN.simple<-function(n.initial,delta,rho,beta,sweep=4,a=1,b=1) {
#
# Simulation of pairwise interaction point  process on rectangle (0,a)X(0,b), 
#
# Arguments
#
#   n.initial: number of events in unthinned process (see below)
#       delta: parameter of simple inhibitory pairwise interaction point process
#         rho: intensity of homogeneous Poisson process on (0,a)X(0,b)
#        beta: radius of thinning field (see below)
#       sweep: number of sweeps of MCMC algorithm for unthinned process
#        a, b: dimensions of rectangle
#  
# Result
#
#   A list three elements:
#        n: the number of events in the thinned process
#      xy: a two-column matrix of event locations
#     xyp: a two-column matrix of centre locations (see below)
#
# Note: the unthinned process is a simple inhibitory pairwise interaction point
#       process with minimum distance delta between any two events; the thinning
#       field takes the value 1 on the union of a set of discs with radius beta
#       and centres a homogeneous Poisson process of intensity rho, zero otherwise
#
   h<-function(u) {
      1*(u>delta)
      }
   np<-round(rho*a*b)
   betasq<-beta*beta
   xyp<-cbind(a*runif(np),b*runif(np))
   xy<-cbind(a*runif(n.initial),b*runif(n.initial))
   for (s in 1:sweep) {
      accept<-F
      for (i in 1:n.initial) {
         while (accept==F) {
            x<-a*runif(1)
            y<-b*runif(1)
            u<-sqrt((x-xy[,1])^2+(y-xy[,2])^2)
            prob<-prod(h(u))
            accept<-(runif(1)<prob)
            }
         accept<-F
         xy[i,]<-c(x,y)
         }
      }
   dsq<-rep(0,n.initial)
   for (i in 1:n.initial) {
      dsq[i]<-min((xy[i,1]-xyp[,1])^2+(xy[i,2]-xyp[,2])^2)
      }
   take<-dsq<betasq
   xy<-xy[take,]
   n<-dim(xy)[1]
   list(n=n,xy=xy,xyp=xyp)
   }

THIN.general<-function(xy.initial,lambda,delta,a=1,b=1) {
#
# Simulation of thinning of a set of points on rectangle (0,a)X(0,b), 
#
# Arguments
#
#   xy.initial: locations of events in unthinned process
#       lambda: intensity of homogeneous Poisson process on (0,a)X(0,b) (see below)
#        delta: radius of thinning field (see below)
#         a, b: dimensions of rectangle
#  
# Result
#
#   A list three elements:
#        n: the number of events in the thinned process
#      xy: a two-column matrix of event locations in the thinned process
#     xyp: a two-column matrix of centre locations (see below)
#
# Note: the thinning field retains locations in the unthinned process that lie
#       within the union of a set of discs with radius delta and centres a 
#       homogeneous Poisson process of intensity lambda
#
   n.initial<-dim(xy.initial)[1]
   np<-round(lambda*a*b)
   deltasq<-delta*delta
   xyp<-cbind(a*runif(np),b*runif(np))
   dsq<-rep(0,n.initial)
   xy<-xy.initial
   for (i in 1:n.initial) {
      dsq[i]<-min((xy[i,1]-xyp[,1])^2+(xy[i,2]-xyp[,2])^2)
      }
   take<-dsq<deltasq
   xy<-xy[take,]
   n<-dim(xy)[1]
   list(n=n,xy=xy,xyp=xyp)
   }

IPIPP.h<-function(n,theta,bound=1,denom=1,sweep=4,a=1,b=1) {
#
# Simulation of imnhomogeneous pairwise interaction point 
# process on rectangle (0,a)X(0,b), 
#
# Arguments
#
#       n: number of events 
#   theta: parameters of the interaction function (see below)
#   bound: upper bound for the intensity (see below)
#   denom: the denominator used for rejection sampling (see below)
#   sweep: number of sweeps of the MCMC algorithm
#    a, b: dimensions of rectangle
#  
# Result
#
#   A two-column matrix of event locations (x,y)
#
# Note: this function calls two user-defined functions:
#
#       1. An intensity function whose syntax must be of the form
#       lambda(x) where x is any location in the rectangle (0,a)X(0,b) and
#       the function returns a non-negative value proportional to the intensity
#       at x and bounded above by the value of bound
#       2. an interaction function whose syntax must be of the form
#       h<-function(u,theta), where u is a vector of non-negative real
#       numbers, and whose result must be a vector of non-negative real numbers
#       with the same length as u. 
#       If h(u,theta) can take values greater than 1,
#       denom needs to be at least as large as the maximum achievable value for the
#       product of the interactions between a candidate location and  all current
#       event locations. Computation can then be very slow.
#
   xy<-IPP(n,bound,a,b)
   for (s in 1:sweep) {
      accept<-F
      for (i in 1:n) {
         while (accept==F) {
            xpyp<-c(IPP(1,bound))
            u<-sqrt((xpyp[1]-xy[,1])^2+(xpyp[2]-xy[,2])^2)
            prob<-prod(h(u,theta))/denom
            accept<-runif(1)<prob
            }
         accept<-F
         xy[i,]<-xpyp
         }
      }
   xy
   }









