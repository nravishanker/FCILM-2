## MLE of mean and covariance of a MVN sample
library(Rfast)
n <- 200
k <- 3
tvn1 <- matrnorm(n, k)
mvnorm.mle(tvn1)


## Sample from Wishart Distributions
library(matrixsampling)
n <- 200
df <- 7
sig <- matrix(c(4, -1, 0, -1, 4, 2, 0, 2, 9), nrow = 3)
# Central Wishart distribution
sample <- rwishart(n = n, nu = df, Sigma = sig)
# Non-central Wishart distribution
ncp <- diag(rep(1, 3))
sample <- rwishart(
  n = n,
  nu = df,
  Sigma = sig,
  Theta = ncp
)
# Inverse non-central Wishart distribution
sig <- matrix(c(4, -1, 0, -1, 4, 2, 0, 2, 9), nrow = 3)
sample <- rinvwishart(n = n, nu = df, Omega = sig)


## Estimate correlations
X <- iris
str(X)
# Simple correlations
# corr(X1,X3)
cor(X[, 1], X[, 3])
cor.test(X[, 1], X[, 3])
# correlation matrix
cor(X[, -5])
# Partial correlations
library(ppcor)
# Partial correlation of X1 and X2 given X3,X4
pcor.test(X[, 1], X[, 2], X[, c(3, 4)])
# all partial correlations
pcor(X[-5])$estimate
# Multiple correlation
library(mro)
mcr.test(X[, -5], 1, c(2, 3, 4))


## Q-Q plot for Multivariate normal (MVN) samples
k <- 4
n <- 200
mu <- c(1, 2, 3, 4)
V  <- matrix(
  c(1, 0.2, 0.3, 0.1, 0.2, 4, 0.6,
    0.2, 0.3, 0.6, 9, 0.3, 0.1, 0.2, 0.3, 1),
  nrow = 4,
  byrow = TRUE
)
x <- rmvnorm(n, mu, V)  # samples
library(MVN)
mvn(x, multivariatePlot = "qq")


## Skewness and kurtosis measures based on MVN samples
library(psych)
M <- mardia(x, na.rm = TRUE, plot = TRUE)
M$b1p # sample skewness
M$b2p # Sample kurtosis
#
M$skew  # test statistic
M$p.skew # p-value
#
M$kurtosis # test statistic
M$p.kurt   # p-value
