## M-regression
# Dataset: stackloss from the R library "MASS"
library(MASS)
data(stackloss)
summary(rlm(stack.loss ~ ., stackloss))
rlm(stack.loss ~ ., stackloss, psi = psi.hampel, init = "lts")
rlm(stack.loss ~ ., stackloss, psi = psi.bisquare)


## Spline fitting
# Dataset: Wage from the R library "ISLR"
library(ISLR)
data(Wage)
attach(Wage)
library(splines)
(agelims <- range(age))
(age.grid <- seq(from = agelims[1], to = agelims[2]))
# Cubic spline with 3 knots/cutpoints at ages 30,45,65
cuts <- c(30, 45, 65)
summary(fit <- lm(wage ~ bs(age, knots = cuts), data = Wage))
# Smoothing spline
fit1 <- smooth.spline(age, wage, df = 16)
# Plot cubic and smoothing splines
plot(age, wage, xlab = "age", ylab = "wage")
points(
  age.grid,
  predict(fit, newdata = list(age = age.grid)),
  col = "blue",
  lwd = 2,
  type = "l"
)
abline(v = cuts, lty = 2, col = "blue") #add cutpoints
lines(fit1, col = "red", lwd = 2)
legend(
  "topright",
  c("smoothing spline", "cubic spline"),
  col = c("red", "blue"),
  lwd = 2
)


## Additive model
# Dataset: gam.data from the R library "gam"
library(gam)
data(gam.data)
gam(y ~ s(x) + z, data = gam.data)


## Generalized additive model
# Dataset: kyphosis from the R library "gam"
library(gam)
data(kyphosis)
Mod <- gam(Kyphosis ~ poly(Age, 2) + s(Start),
           data = kyphosis,
           family = binomial)
summary(Mod)


## Projection pursuit regression
# Dataset: rock from the R library "datasets"
data(rock)
attach(rock)
area1 <- area / 10000
peri1 <- peri / 10000
rock.ppr <- ppr(
  log(perm) ~ area1 + peri1 + shape,
  data = rock,
  nterms = 2,
  max.terms = 5
)
summary(rock.ppr)


## Multivariate adaptive regression splines
# Dataset: trees from the R library "mda"
library(mda)
data(trees)
diameter <- trees[, 1]
height <- trees[, 2]
volume <- trees[, 3]
fit1 <- mars(cbind(diameter, height), volume)
fit1$coefficients
fit1$cuts
showcuts <- function(obj)
{
  tmp <- obj$cuts[obj$sel, ]
  dimnames(tmp) <- list(NULL, names(trees)[-3])
  tmp
}
showcuts(fit1)


## Neural networks regression
# Dataset: marketing from the R library "datarium"
library(datarium)
data(marketing)
# scale features to be in (0,1)
scale01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
marketing <- marketing %>%  mutate_all(scale01)
# split into training and test
marketing_train <- sample_frac(tbl = marketing,
                               replace = FALSE,
                               size = 0.80)
marketing_test <- anti_join(marketing, marketing_train)
#Fit a simple 1-hidden layer neural net  with 1 neuron.
library(neuralnet)
set.seed(123457)
nn.1 <- neuralnet(sales ~ youtube + facebook + newspaper,
                  data = marketing_train)
plot(nn.1, rep = "best")
#SSE of the test data
test_nn.1_out <- compute(nn.1, marketing_test[, 1:3])$net.result
nn.1_test_SSE <- sum((test_nn.1_out - marketing_test[, 4]) ^ 2) / 2
nn.1_test_SSE


## Regularized regression
# Dataset: Hitters from the R library "ISLR"
# see https://uc-r.github.io/regularized_regression
library(ISLR)
data(Hitters, package = "ISLR")
Hitters <- na.omit(Hitters)
X <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary
# OLS fit
fit <- lm(Salary ~ ., Hitters)
c.ls <- coef(fit)
library(glmnet)
lasso    <- glmnet(X, y, alpha = 1.0)
elastic.1 <- glmnet(X, y, alpha = 0.25)
elastic.2 <- glmnet(X, y, alpha = 0.75)
ridge    <- glmnet(X, y, alpha = 0.0)
par(mfrow = c(2, 2))
plot(lasso, xvar = "lambda", main = "Lasso (Alpha = 1)")
plot(elastic.1, xvar = "lambda", main = "Elastic Net (Alpha = .25)")
plot(elastic.2, xvar = "lambda", main = "Elastic Net (Alpha = .75)")
plot(ridge, xvar = "lambda", main = "Ridge (Alpha = 0)")
# Cross-validation is used to select the ridge parameter using the cv.glmnet()
# function with (the default) 10 folds. The code also prints the
# penalty term using the minimum $\lambda$ value.
fit_ridge_cv <- cv.glmnet(X, y, alpha = 0)
# Df: number of nonzero coefficients;
# %dev: percent (of null) deviance explained;
# Lambda: value of alpha
# Ridge fit
fit_ridge_cv
c.ridge <- coef(fit_ridge_cv)
sum(coef(fit_ridge_cv, s = "lambda.min")[-1] ^ 2)
# Lasso fit
fit_lasso_cv <- cv.glmnet(X, y, alpha = 1)
fit_lasso_cv
c.lasso <- coef(fit_lasso_cv, s = "lambda.min")
sum(coef(fit_lasso_cv, s = "lambda.min")[-1] ^ 2)
# Elastic net fit
fit_enet_cv <- cv.glmnet(X, y, alpha = 0.25)
fit_enet_cv
c.enet <- coef(fit_enet_cv, s = "lambda.min")
(c <- cbind(c.ls, c.ridge, c.lasso, c.enet))
