## Partial residual and added variable plots
# Dataset: stackloss in the R library "datasets"
library(datasets)
data(stackloss)
attach(stackloss)
mod <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.)
car::crPlots(mod)
car::avPlots(mod)
## Sequential and partial F-tests
anova(mod) #Sequential F-tests
car::Anova(mod)#Partial F-tests


## Variable selection
# Dataset: cement in the R library "MASS"
library(MASS)
data(cement)
str(cement)
full.model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
summary(full.model)
library(olsrr)
## All possible regressions
# Rsq and Cp based selection
ols_step_all_possible(full.model)
# Forward selection
ols_step_forward_p(full.model, prem = 0.10)
summary(forward.model <- lm(y ~ x1 + x2 + x4, data = cement))
# Backward elimination
ols_step_backward_p(full.model, prem = 0.10)
summary(backward.model <- lm(y ~ x3 + x4, data = cement))
# Stepwise selection
ols_step_both_p(full.model, prem = 0.10)
summary(stepwise.model <- lm(y ~ x1 + x2, data = cement))


## Multicollinearity
# Dataset: Boston in the R library "MASS"
library(MASS)
data(Boston)
library(olsrr)
model <- lm(medv ~ ., data = Boston)
car::vif(model)   # VIF
# Refit a model excluding the variable tax
model2 <- lm(medv ~ . - tax, data = Boston)
car::vif(model2)    #all the VIF's < 5
# Ridge regression
x <- Boston[, 1:13]
y <- Boston[, 14]
xs <- scale(x, TRUE, TRUE)
ys <- scale(y, TRUE, TRUE)
lm.coef <- lm.ridge(ys ~ xs - 1)
lm.coef
plot(lm.ridge(ys ~ xs - 1, lambda = seq(0, 100, 0.1)))
lambda.est <-
  select(lm.ridge(ys ~ xs - 1, lambda = seq(0, 100, 0.1)))
fit <-
  lm.ridge(ys ~ xs - 1, lambda = 4.3) # at optimal ridge coefficient
coef(fit)
# Principal components regression
summary(pcr_model <-
          pls::pcr(medv ~ ., data = Boston, scale = TRUE))


## Dummy variables in regression
# Dataset: salaries from the R library "carData"
library(tidyverse) #used for manipulating data
data("Salaries", package = "carData")
## Categorical variable gender, with 2 levels
# Model for salary difference between males and females
model <- lm(salary ~ sex, data = Salaries)
summary(model)$coef # male appears in the model results
# Dummy variable created by R for gender used in the model
contrasts(Salaries$sex)
# We can use the function relevel() to set the baseline category to males:
Salaries <- Salaries %>% mutate(sex = relevel(sex, ref = "Male"))
model <- lm(salary ~ sex, data = Salaries)
summary(model)$coef # now, female appears in the model results
## Categorical variables with more than two levels
res <- model.matrix(~ rank, data = Salaries)
head(res[, -1])
summary(model2 <-
          lm(salary ~ yrs.service + rank + discipline + sex,
             data = Salaries))
# Build model using 4 variables in which three variables are dummy coded
library(car) #to use the anova function
model2 <-
  lm(salary ~ yrs.service + rank + discipline + sex, data = Salaries)
Anova(model2)
summary(model2)
vcov(model2)
