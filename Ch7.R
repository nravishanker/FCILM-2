## One-way fixed-effects model (continued from Ch. 4)
# Dataset: PlantGrowth from R library "stats"
attach(PlantGrowth)
fit <- aov(weight ~ group, data = PlantGrowth)
summary(fit)
coef(fit)
#Contrasts
contrasts(group)
library(multcomp)
glht(fit, linfct = mcp(group = "Tukey"))


## Two-way additive ANOVA model
# Dataset: ToothGrowth from R library "stats"
attach(ToothGrowth)
dose <-
  factor(dose,
         levels = c(0.5, 1, 2),
         labels = c("D0.5", "D1", "D2"))
fit <- aov(len ~ supp + dose, data = ToothGrowth)
summary(fit)
coef(fit)


## Two-way ANOVA model with interaction
# Datset: wightgain from R library "HSAUR"
library(HSAUR)
data(weightgain)
fit <- aov(weightgain ~ source * type, data = weightgain)
summary(fit)
summary(lm(fit))$coefficients
