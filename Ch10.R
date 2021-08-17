## Unbalanced fixed effects (FE) one-factor ANOVA
# Dataset: schizophrenia in the R library "HSAUR"
library(HSAUR)
data("schizophrenia")
fit <- lm(age ~ gender, schizophrenia)
car::Anova(fit, type = "III")  # type III SS
summary(lm(fit))$coefficients
contrasts(schizophrenia$gender)


## Unbalanced fixed effects (FE) two-factor ANOVA with interaction
# Dataset: foster in the R library "HSAUR"
library(HSAUR)
data(foster)
fit <- lm(weight ~ litgen * motgen, foster)
fit$coefficients
car::Anova(fit, type = "III") # type III SS
summary(lm(fit))$coefficients
contrasts(foster$litgen)
contrasts(foster$motgen)


## Kruskal-Wallis procedure
# Dataset: airquality in the R library "datasets"
data(airquality)
kruskal.test(Ozone ~ Month, data = airquality)


## Analysis of covariance
# Dataset: anxiety in the R library "datarium"
library(datarium)
data("anxiety")
anxiety$group <-
  factor(anxiety$group, levels = c("grp1", "grp2", "grp3"))
ancova <-
  lm(t3 ~ t1 + group,
     data = anxiety,
     contrasts = list(group = "contr.SAS"))
summary(ancova)$coefficients   # least squares coefficients
anova(ancova)  # unadjusted sum of squares
car::Anova(ancova, type = "III")    # adjusted sum of squares
summary(ancova)$r.squared
summary(ancova)$sigma


## Multiple Comparisons
# Dataset: PlantGrowth in the R library "datasets"
data(PlantGrowth)
fit <- aov(weight ~ group, data = PlantGrowth)
library(multcomp)
# Tukey's procedure
summary(glht(fit, linfct = mcp(group = "Tukey")))
# Dunnett's procedure
summary(glht(fit, linfct = mcp(group = "Dunnett")))
library(agricolae)
#LSD procedure
LSD.test(fit, "group", p.adj = "bonferroni", console = TRUE)
#Duncan multiple range procedure
duncan.test(fit, "group", console = TRUE)
#Newman-Keuls procedure
SNK.test(fit, "group", console = TRUE)
