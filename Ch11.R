## Random-effects one-factor model
# Dataset: Dyestuff in the R library "lme4"
data(Dyestuff, package = "lme4")
# Anova estimates
Dyestuff$Batch <- factor(Dyestuff$Batch)
fit <- aov(Yield ~ Batch, Dyestuff)
summary(fit)
coef(fit)
# Calculate estimate of sigma2mu from the ANOVA table
table(Dyestuff$Batch)
sigma2mu <- (11272 -  2451) / 5
sigma2mu
# REML estimates
r <- lme4::lmer(Yield ~ 1 | Batch, data = Dyestuff)
summary(r)
lme4::ranef(r) #estimated RE
# Maximum likelihood (ML) estimates
m <- lme4::lmer(Yield ~ 1 | Batch, data = Dyestuff, REML = FALSE)
summary(m)
lme4::ranef(m)
# Minque estimates
library(minque)
(res = lmm(Yield ~ 1 | Batch, Dyestuff))
res$Var
res$RandomEffect

## Nested Model
# Dataset: curdies from the R library "GFD"
library(GFD)
data(curdies)
curdies$site <- as.factor(curdies$site)
curdies.aov <- aov(dugesia ~ season + Error(site), data = curdies)
summary(lm(curdies.aov))$coefficients # least squares coefficients
