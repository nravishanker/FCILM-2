library(mvtnorm)
library(MASS)
library(LaplacesDemon)

## Bivariate normal distribution
library(car)
# simulate bivariate normal with rho=0.5
set.seed(1)
n <- 200
rho <- 0.5
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2)
bvn1 <- mvtnorm::rmvnorm(n, mu, Sigma, method = "svd")
# simulate bivariate normal with rho=-0.5
set.seed(1)
n <- 200
rho <- -0.5
mu <- c(0, 0)
Sigma <- matrix(c(1,-0.5,-0.5, 1), 2)
bvn2 <- mvtnorm::rmvnorm(n, mu, Sigma, method = "svd")
#plot contours
par(mfrow = c(1, 2))
dataEllipse(
  bvn1[, 1],
  bvn1[, 2],
  levels = c(0.5, 0.95),
  xlab = "x1",
  ylab = "x2",
  sub = "rho=0.5"
)
dataEllipse(
  bvn2[, 1],
  bvn2[, 2],
  levels = c(0.5, 0.95),
  xlab = "x1",
  ylab = "x2",
  sub = "rho=-0.5"
)
library(MASS)
# contours based on kde
par(mfrow = c(1, 2))
kde1 <- kde2d(bvn1[, 1], bvn1[, 2], n = n)
contour(kde1,
        xlab = "x1",
        ylab = "x2",
        sub = "rho=0.5")
kde2 <- kde2d(bvn2[, 1], bvn2[, 2], n = n)
contour(kde2,
        xlab = "x1",
        ylab = "x2",
        sub = "rho=-0.5")


## Trivariate normal distribution
set.seed(1)
n <- 200
rho <- 0.5
mu1 <- 1
mu2 <- 1
mu3 <- 1
s1 <- 2
s2 <- 4
s3 <- 6
mu <- c(mu1, mu2, mu3)
mu
Sigma <- matrix(
  c(
    s1 ^ 2,
    s1 * s2 * rho,
    s1 * s3 * rho,
    s2 * s1 * rho,
    s2 ^ 2,
    s2 * s3 * rho,
    s3 * s1 * rho,
    s3 * s2 * rho,
    s3 ^ 2
  ),
  3
)
Sigma
tvn <- mvtnorm::rmvnorm(n, mu, Sigma, method = "svd")
colnames(tvn) <- c("x1", "x2", "x3")
scatterplotMatrix(tvn[, 1:3])


## Chi-square distributions
n <- 200 #sample size
df1 <- 6
df2 <- 10 #d.f.
ncp1 <- 6
ncp2 <- 2 #non-centrality parameter
#Central chi-square
sim.1 <- rchisq(n, df1)
sim.2 <- rchisq(n, df2)
#Noncentral chi-square
sim.3 <- rchisq(n, df1, ncp = ncp1)
sim.4 <- rchisq(n, df1, ncp = ncp2)
#plot(density(sim.1),col="red",main="Chi-Square",ylim=c(0,0.16))
plot(
  density(sim.1),
  lty = 1,
  main = " ",
  ylim = c(0, 0.16)
)
#lines(density(sim.2),col="blue")
lines(density(sim.2), lty = 2)
#lines(density(sim.3),col="green")
lines(density(sim.3), lty = 3)
#lines(density(sim.4),col="black")
lines(density(sim.4), lty = 4)
#legend("topright",c("df=6,ncp=0","df=10,ncp=0","df=6,ncp=6","df=6,ncp=2"),
# col=c("red","blue","green","black"),pch=19)
legend(
  "topright",
  c("df=6,ncp=0", "df=10,ncp=0", "df=6,ncp=6", "df=6,ncp=2"),
  lty = c(1, 2, 3, 4),
  pch = 19
)


## Simulate from F-distributions
n <- 100
df1 <- 4
df2 <- 9
ncp <- 7
Sim.cf <- rf(n, df1, df2)
Sim.ncf <- rf(n, df1, df2, ncp = ncp)


## Simulate from t-distributions
n <- 100
df <- 8
ncp <- 4
Sim.ct <- rt(n, df)
Sim.nct <- rt(n, df, ncp = ncp)


## Mixture of Multivariate normal (MVN) distributions
mixture.mvn.simul <-
  function(n,
           prob,
           mu1,
           sigma1,
           mu2,
           sigma2,
           mu3,
           sigma3) {
    set.seed(1)
    m = rmultinom(1, n, prob)
    x1 = MASS::mvrnorm(m[1], mu1, sigma1)
    x2 = MASS::mvrnorm(m[2], mu2, sigma2)
    x3 = MASS::mvrnorm(m[3], mu3, sigma3)
    X = rbind(x1, x2, x3)  #combine the samples
    X = X[sample(1:n),]  # permute the rows
    return(X)
  }
L = 3 # number of mixands
n = 200   # number of samples
k = 2 # MVN dimension
p1 = 0.3
mu1 = c(0, 0)
sigma1 = matrix(c(1, 0.5, 0.5, 1), 2, 2)
p2 = 0.5
mu2 = c(10, 8)
sigma2 = matrix(c(10, 1, 1, 10), 2, 2)
p3 = 1 - p1 - p2
mu3 = c(3, 1)
sigma3 = matrix(c(0.1, 0.005, 0.005, 0.4), 2, 2)
prob = c(p1, p2, p3)
X = mixture.mvn.simul(n, prob, mu1, sigma1, mu2, sigma2, mu3, sigma3)


## Spherical distributions
## Example: Standard bivariate normal
# Radial distribution is sqrt of a chi-square distribution
library("distrEllipse")
k <- 2
n <- 1000
loc <- c(0, 0) # origin
c <- 4
scale <- c * diag(length(loc)) #constant multiple of I
RL1 <- Chisq(df = length(loc))# radial dist.sqrt of a chi-square(k)
z1_f <- EllipticalDistribution(radDistr = sqrt(RL1), loc, scale)
z1 <- t(r(z1_f)(n)) # samples
k1 <-
  MASS::kde2d(z1[, 1], z1[, 2])
contour(k1,
        asp = 1,
        xlim = c(-10, 10),
        ylim = c(-10, 10))
MVN::mvn(z1, multivariatePlot = "qq") #check MVN
## Standard bivariate double exponential (Laplace)
k <- 2
n <- 1000
loc <- c(0, 0)
RL2 <-
  (DExp() ^ 2 + DExp() ^ 2) #radial dist.x'x=x1^2+x2^2,r=sqrt(x'x)
z2_f <- EllipticalDistribution(radDistr = sqrt(RL2), loc, scale)
z2 <- t(r(z2_f)(n))
k2 <-
  MASS::kde2d(z2[, 1], z2[, 2])
contour(k2,
        asp = 1,
        xlim = c(-10, 10),
        ylim = c(-10, 10))
# 3-d density plot - bivariate standard Laplace
plotly::plot_ly(
  x = k2$x,
  y = k2$y,
  z = k2$z,
  type = "surface"
)
MVN::mvn(z2, multivariatePlot = "qq") #Check MVN


## Elliptical distributions
## Example: bivariate normal
k <- 2
n <- 1000
loc <- c(1, 2)
scale <-
  matrix(c(2, 0, 0, 3), nrow = 2, byrow = TRUE) # P where PP'=V
x1_f <-
  EllipticalDistribution(radDistr = sqrt(Chisq(df = length(loc))), loc, scale)
x1 <- t(r(x1_f)(n))
k1 <- MASS::kde2d(x1[, 1], x1[, 2])
contour(k1, asp = 1)
MVN::mvn(x1, multivariatePlot = "qq")
## Example: bivariate Laplace
k <- 2
n <- 1000
loc <- c(1, 2)
scale <-
  matrix(c(2, 0, 0, 3), nrow = 2, byrow = TRUE) # P where PP'=V
RL <- (DExp() ^ 2 + DExp() ^ 2)
z2_f <- EllipticalDistribution(radDistr = sqrt(RL))
x2 <- t(r(z2_f)(n)) %*% t(scale) + loc
k2 <- MASS::kde2d(x2[, 1], x2[, 2])
contour(k2,
        asp = 1,
        xlim = c(-10, 10),
        ylim = c(-10, 10))
MVN::mvn(x2, multivariatePlot = "qq")
