## Multivariate linear model
# Datset: mtcars from the R library "datasets"
data(mtcars)
Y <- as.matrix(mtcars[, c("mpg", "disp", "hp", "wt")])
mvmod <- lm(Y ~ cyl + am + carb, data = mtcars)
summary(mvmod)
#estimated coefficient matrix B_hat
coef(mvmod)


## Multivariate ANOVA (MANOVA) model
# Dataset: Plastic from the R library "heplots"
library(heplots)
data(Plastic)
model <- lm(cbind(tear, gloss, opacity) ~ rate, data = Plastic)
Anova(model)
pairs(model)
fit <- manova(cbind(tear, gloss, opacity) ~ rate, data = Plastic)
summary(fit, test = "Pillai")


## Repeated measures analysis
# Dataset: emotion from the R library "psycho"
library(psycho)
data(emotion)
library(tidyverse)
df <- psycho::emotion %>%
  select(Participant_ID,
         Participant_Sex,
         Emotion_Condition,
         Subjective_Valence,
         Recall)
# ANOVA
summary(aov(
  Subjective_Valence ~ Emotion_Condition +
    Error(Participant_ID / Emotion_Condition),
  data = df
))
# ANOVA from a linear mixed model
library(lmerTest)
fit <-
  lmer(Subjective_Valence ~ Emotion_Condition + (1 |
                                                   Participant_ID),
       data = df)
anova(fit)


## Elliptical linear models
# Dataset: luzdat from the R library gwer
library(gwer)
data(luzdat)
attach(luzdat)
z1 <- C(factor(x1), treatment)
z2 <- x2
z3 <- x2 ^ 2
luz <- data.frame(y, z1, z2, z3)
# Normal fit
elliptical.fitn <-
  elliptical(y ~ z1 + z2 + z3, family = Normal(), data = luz)
summary(elliptical.fitn)
family(elliptical.fitn)
# Student-t fit
elliptical.fitt <-
  elliptical(y ~ z1 + z2 + z3, family = Student(df = 5),
             data = luz)
summary(elliptical.fitt)
family(elliptical.fitt)
# power exponential fit
elliptical.fitpe <- elliptical(y ~ z1 + z2 + z3, family =
                                 Powerexp(k = 0.5), data = luz)
summary(elliptical.fitpe)
family(elliptical.fitpe)


## Dynamic linear model
# Dataset: jj from the R library "astsa"
library(astsa)
data(jj)
# set up as a DLM
num = length(jj)
A = cbind(1, 1, 0, 0)
# Function to calculate the innovations likelihood
# kf: filter estimates of the state
Linn = function(para) {
  Phi = diag(0, 4)
  Phi[1, 1] = para[1]
  Phi[2,] = c(0,-1,-1,-1)
  Phi[3,] = c(0, 1, 0, 0)
  Phi[4,] = c(0, 0, 1, 0)
  cQ1 = para[2]
  cQ2 = para[3]     # sqrt q11 and sqrt q22
  cQ = diag(0, 4)
  cQ[1, 1] = cQ1
  cQ[2, 2] = cQ2
  cR = para[4]       # sqrt r11
  kf = Kfilter0(num, jj, A, mu0, Sigma0, Phi, cQ, cR)
  return(kf$like)
}
# Initial parameters
mu0 = c(.7, 0, 0, 0)
Sigma0 = diag(.04, 4)
init.par = c(1.03, .1, .1, .5)  # Phi[1,1], the 2 Qs and R
# Numerical MLEs
est = optim(
  init.par,
  Linn,
  NULL,
  method = "BFGS",
  hessian = TRUE,
  control = list(trace = 1, REPORT = 1)
)
SE = sqrt(diag(solve(est$hessian)))
u = cbind(estimate = est$par, SE)
rownames(u) = c("Phi11", "sigw1",
                "sigw2", "sigv")
u
# Smoothed estimates of the state vector.
Phi = diag(0, 4)
Phi[1, 1] = est$par[1]
Phi[2,] = c(0,-1,-1,-1)
Phi[3,] = c(0, 1, 0, 0)
Phi[4,] = c(0, 0, 1, 0)
cQ1 = est$par[2]
cQ2 = est$par[3]
cQ = diag(1, 4)
cQ[1, 1] = cQ1

cQ[2, 2] = cQ2
cR = est$par[4]
ks = Ksmooth0(num, jj, A, mu0, Sigma0, Phi, cQ, cR)
# Plot filter and smoothed estimates and the observed time series
Tsm = ts(ks$xs[1, ,], start = 1960, freq = 4)
Ssm = ts(ks$xs[2, ,], start = 1960, freq = 4)
p1 = 2 * sqrt(ks$Ps[1, 1,])
p2 = 2 * sqrt(ks$Ps[2, 2,])
par(mfrow = c(3, 1))
plot(Tsm, main = "Trend", ylab = "Trend")
lines(Tsm + p1, lty = 2, col = 4)
lines(Tsm - p1, lty = 2, col = 4)
plot(Ssm,
     main = "Seasonal",
     ylim = c(-5, 4),
     ylab = "Season")
lines(Ssm + p2, lty = 2, col = 4)
lines(Ssm - p2, lty = 2, col = 4)
plot(jj, type = "p", main = "Data (points) and Trend+Season (line)")
lines(Tsm + Ssm)
# Forecast 12 steps ahead (3 years) into the future and plot
# them together with their upper and lower confident limits.
n.ahead = 12

y = ts(append(jj, rep(0, n.ahead)), start = 1960, freq = 4)
rmspe = rep(0, n.ahead)
x00 = ks$xf[, , num]
P00 = ks$Pf[, , num]
Q = t(cQ) %*% cQ
R = t(cR) %*% (cR)
for (m in 1:n.ahead) {
  xp = Phi %*% x00
  Pp = Phi %*% P00 %*% t(Phi) + Q
  sig = A %*% Pp %*% t(A) + R
  K = Pp %*% t(A) %*% (1 / sig)
  x00 = xp
  P00 = Pp - K %*% A %*% Pp
  y[num + m] = A %*% xp
  rmspe[m] = sqrt(sig)
}
plot(
  y,
  type = "o",
  main = "",
  ylab = "",
  ylim = c(5, 30),
  xlim = c(1975, 1984)
)
upp = ts(y[(num + 1):(num + n.ahead)] + 2 * rmspe, start = 1981, freq =
           4)
low = ts(y[(num + 1):(num + n.ahead)] - 2 * rmspe, start = 1981, freq =
           4)
lines(upp, lty = 2)
lines(low, lty = 2)
abline(v = 1980.75, lty = 3)
