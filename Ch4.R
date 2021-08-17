## Simple Linear Regression (SLR) Model
# Dataset: cars in the R library "car"
library(car)
data("cars")
attach(cars)
plot(dist, speed)
# Simple correlation
cor(dist, speed)
corr.test <- cor(dist, speed)
cor.test
# OLS estimation in an SLR model
mod.slr <- lm(speed ~ dist, data = cars)
smod <- summary(mod.slr)
# Pull out items from the output list
smod$sigma #standard error
smod$coefficients[, 1] #least square estimates
smod$coefficients[, 2] #standard error of least square estimates
smod$r.squared #R-square
smod$adj.r.squared # Adjusted R-square


## Multiple Linear Regression (MLR) Model
# Dataset: stackloss in the R library "datasets"
library(datasets)
library(car)
data(stackloss)
attach(stackloss)
scatterplotMatrix(stack.loss)
data <- cbind(stack.loss, Air.Flow, Water.Temp, Acid.Conc.)
(cor.mat <- cor(data))
# OLS estimation in an MLR model
mod <- lm(stack.loss ~ Air.Flow + Water.Temp
          + Acid.Conc., data = stackloss)
(smod <- summary(mod))
smod$sigma #standard error
smod$coefficients[, 1] #least square estimates
smod$coefficients[, 2] #standard error of least square estimates
smod$r.squared #R-square
smod$adj.r.squared # Adjusted R-square


## One-way fixed-effects ANOVA  model
# Dataset: PlantGrowth in the R library "stats"
data(PlantGrowth)
fit.lm <- lm(weight ~ group, data = PlantGrowth)
summary(fit.lm)
#or
summary(fit <- lm(weight ~ group, data = PlantGrowth))
#or
summary(fit1 <- aov(weight ~ group, data = PlantGrowth))
coef(fit1)
