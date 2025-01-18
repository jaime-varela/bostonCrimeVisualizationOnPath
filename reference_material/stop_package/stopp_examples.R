install.packages("stopp")
library("stopp")
set.seed(2)
df <- data.frame(runif(100), runif(100), runif(100))
stp1 <- stp(df)
stp1

# plot the stp object
plot(stp1)


# plot networked dataset with non cumulative time points
set.seed(2)
df_net <- data.frame(runif(100, 0, 0.85), runif(100, 0, 0.85), runif(100))
stlp1 <- stp(df_net, L = chicagonet)
stlp1

plot(stlp1,tcum = FALSE)


# Multi-type point patterns
set.seed(2)
dfA <- data.frame(x = runif(100), y = runif(100), t = runif(100), m1 = rnorm(100), m2 = rep(c("C"), times = 100))
dfB <- data.frame(x = runif(50), y = runif(50), t = runif(50), m1 = rnorm(25), m2 = rep(c("D"), times = 50))
stpm2 <- stpm(rbind(dfA, dfB), names = c("continuous", "dichotomous"))
plot(stpm2)


# some covariate stuff
set.seed(2)
df <- data.frame(runif(100), runif(100), runif(100), rpois(100, 15))
sim_cov <- stcov(df, interp = FALSE, names = "SimulatedCovariate")
interp_cov <- stcov(df, mult = 20, names = "InterpolatedCovariate")
plot(sim_cov)
plot(interp_cov)


?rpois
?stcov
?stppm

# Load data
data("greececatalog", package = "stopp")
plot(greececatalog)


# valencia
data("valenciacrimes", package = "stopp")
plot(valenciacrimes)
data("chicagonet", package = "stopp")
data("valencianet", package = "stopp")
plot(chicagonet)
plot(valencianet)

# section 4 simulations
?stp
?rstpp
rstpp(lambda = 500)
exp_pp = rstpp(lambda = function(x, y, t, a) {exp(a[1] + a[2] * x)}, par = c(2, 6))
plot(exp_pp,tcum=FALSE)




# section six fitting
?stppm

# fit a homogenous (constant) poisson process
set.seed(2)
ph <- rstpp(lambda = 200)
hom1 <- stppm(ph, formula = ~ 1, seed = 2)
hom1

# fit a \lambda = exp(\theta_0 + x \theta_1) model
# note that the formula is a log of what is normally expected

set.seed(2)
# note par is the parameters
pin <- rstpp(lambda = function(x, y, t, a) {exp(a[1] + a[2] * x)}, par = c(2, 6))
?rstpp
pin

inh1 <- stppm(pin, formula = ~ x, seed = 2)
inh1

# spline based fitting
inh2 <- stppm(pin, formula = ~ s(x, y, bs = "tp", k = 30), seed = 2)
plot(inh2)


# adding external 'covariates'
set.seed(2)
df1 <- data.frame(runif(100), runif(100), runif(100), rpois(100, 15))
df2 <- data.frame(runif(100), runif(100), runif(100), rpois(100, 15))
?rpois


obj1 <- stcov(df1, names = "cov1")
obj2 <- stcov(df2, names = "cov2")

# store the covariates
covariates <- list(cov1 = obj1, cov2 = obj2)

inh3 <- stppm(pin, formula = ~ x + cov2, covs = covariates, spatial.cov = TRUE, seed = 2)
inh3


set.seed(2)
dfA <- data.frame(x = runif(100), y = runif(100), t = runif(100), m1 = rep(c("A"), times = 100))
dfB <- data.frame(x = runif(50), y = runif(50), t = runif(50), m1 = rep(c("B"), each = 50))
stpm1 <- stpm(rbind(dfA, dfB))
plot(stpm1)



# fit the model
inh4 <- stppm(stpm1, formula = ~ x + s(m1, bs = "re"), marked = TRUE, seed = 2)
plot(inh4)


# 6.4. Spatio-temporal Poisson point processes with separable intensity
crimesub <- stpm(valenciacrimes$df[101:200, ], names = colnames(valenciacrimes$df)[-c(1:3)], L = valencianet)
mod1 <- sepstlppm(crimesub, spaceformula = ~x , timeformula = ~ day)
plot(mod1)

?sepstlppm

# 6.5. Spatio-temporal Poisson point processes with non-separable intensity

nonsepmod <- stppm(greececatalog, formula = ~ x + y + t + x:y + y:t + I(x^2) + I(y^2) + I(t^2) + I(x^2):I(y^2), seed = 2)

summary(nonsepmod)
plot(nonsepmod)


summary(nonsepmod$mod_global)


# 6.6. Log-Gaussian Cox processes
catsub <- stp(greececatalog$df[1:200, ])
lgcp1 <- stlgcppm(catsub, seed = 2)
lgcp1


# 6.7. Local models
set.seed(2)
inh <- rstpp(lambda = function(x, y, t, a) {exp(a[1] + a[2] * x)},par = c(0.005, 5))
inh_local <- locstppm(inh, formula = ~ x, seed = 2)
inh_local



localplot(inh_local)

localsummary(inh_local)


lgcp2 <- stlgcppm(catsub, second = "local", seed = 2)
lgcp2


plot(lgcp2)
localplot(lgcp2)



# section 7 global diagnostics

?rstpp
?stppm
set.seed(2)
inh <- rstpp(lambda = function(x, y, t, a) {exp(a[1] + a[2]*x)}, par = c(.3, 6))
mod1 <- stppm(inh, formula = ~ 1)
mod2 <- stppm(inh, formula = ~ x)
(g1 <- globaldiag(inh, mod1$l))
(g2 <- globaldiag(inh, mod2$l))
?globaldiag
plot(g1)

(g2 <- globaldiag(inh,mod2$l))
plot(g2)


# section 7.2 local diagnostics
?localdiag
res <- localdiag(inh, mod1$l, p = .9)
res

plot(res)


infl(res)


