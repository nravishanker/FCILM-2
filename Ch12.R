## Binary response models
# Dataset: heart from the R library "catdata"
library(catdata)
data(heart, package = "catdata")
head(heart)
## Logit modeling
# Full model
full_logit <- glm(
  y ~ sbp + tobacco + ldl + adiposity
  + famhist + typea + obesity + alcohol
  + age,
  data = data.frame(heart),
  family = binomial(link = "logit")
)
summary(full_logit)
# Reduced model
red_logit <-  glm(
  y ~ tobacco + ldl + famhist + typea + age ,
  data = data.frame(heart),
  family = binomial(link = "logit")
)
summary(red_logit)
(a.out <- anova(red_logit, full_logit))
aval <- a.out$Deviance[2]
adf <- a.out$DF[2]
1 - pchisq(aval, 4) # p-value
## Probit modeling
full_probit <- glm(
  y ~ sbp + tobacco + ldl + adiposity
  + famhist + typea + obesity + alcohol
  + age,
  data = data.frame(heart),
  family = binomial(link = "probit")
)
summary(full_probit)
red_probit <-  glm(
  y ~ tobacco + ldl + famhist + typea + age ,
  data = data.frame(heart),
  family = binomial(link = "probit")
)
summary(red_probit)
anova(red_probit, full_probit)
1 - pchisq(3.8368, 4)  # p-value


## Count models
# Dataset: aids from the R library "catdata"
data("aids", package = "catdata")
## Poisson regression
full_pois <-
  glm(cd4 ~ time + drugs + partners + packs + cesd + age,
      data = data.frame(aids),
      family = poisson)
summary(full_pois)
red_pois <- glm(cd4 ~ time + drugs + packs + cesd,
                data = data.frame(aids))
summary(red_pois)
anova(full_pois, red_pois)
## Negative binomial regression
full_nb <- glm.nb(cd4 ~ time + drugs + partners + packs
                  + cesd + age, data = data.frame(aids))
summary(full_nb)
# reduced negative binomial regression model
red_nb <- glm.nb(cd4 ~ time + drugs + packs + cesd,
                 data = data.frame(aids))
summary(red_nb)
anova(full_nb, red_nb)
