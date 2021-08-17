## Heteroscedasticity in the MLR model
# Dataset: compasst from the R library "HoRM"
library(HoRM)
data(compasst)
attach(compasst)
ols <- lm(cost ~ num.responses, data = compasst)
plot(ols$fitted.values,
     rstandard(ols),
     xlab = "OLS fits",
     ylab = "OLS residuals")
# White's test
lmtest::bptest(ols, ~ I(num.responses ^ 2), data = compasst)
# Bartlett's test
library(groupdata2)
# create n = 10 groups sorted by x
# grouping is done if there are no replicates
group <- group(compasst$num.responses, 4)
library(lmtest)
bartlett.test(ols$residual, g = group$.groups)
#Goldfeld-Quandt test
gqtest(cost ~ num.responses, data = compasst)
# Breusch-Pagan Test
bptest(cost ~ num.responses, data = compasst)  # requires normality of residuals
## Weighted least squares (WLS)
# To get weights, run an OLS regression of the
# squares of the OLS residuals (or the absolute values of OLS residuals) of the
# OLS fitted values. Get the fitted values from this regression and square them
# to get the weights that are needed for the WLS fit.
new.fits = lm(abs(ols$residuals) ~ ols$fitted)$fitted ^ 2
lmtest::bptest(ols, ~ I(new.fits), data = compasst)
# Obtain WLS estimates of betas and sigma^2 as follows:
wls <- lm(cost ~ num.responses,
          weights = 1 / new.fits,
          data = compasst)
lmtest::bptest(wls, ~ I(num.responses ^ 2), data = compasst)
summary(wls)$coefficients # WLS estimation
summary(wls)$r.squared    # WLS R-square
summary(wls)$sigma ^ 2    # estimate of sigma^2 WLS
plot(wls$fitted.values,
     wls$residuals,
     xlab = "WLS fits",
     ylab = "WLS residuals")


## Serial correlation in the MLR model
# Dataset: Air.df in the R library EnvStats
library(EnvStats)
data(Air.df)
mod <-
  lm(
    ozone ~ radiation + temperature + wind + I(temperature ^ 2) +
      I(wind ^ 2),
    data = Air.df,
    na.action = na.exclude
  )
summary(mod)$coef
lmtest::dwtest(mod)
#GLS fit
library(nlme)
mod.gls <- gls(
  ozone ~ radiation + temperature + wind + I(temperature ^ 2)
  + I(wind ^ 2),
  data = Air.df,
  na.action = na.exclude,
  correlation = corARMA(p = 1),
  method = "ML"
)
summary(mod.gls)


## Regression diagnostics
# Datset: waste in the R library "RobStatTM"
library(RobStatTM)
data(waste)
model <-
  lm(SolidWaste ~ Land + Metals + Trucking + Retail + Restaurants, data = waste)
summary(model)$r.squared
summary(model)$fstatistic
summary(model)$sigma ^ 2
rstandard(model)
# Boxplots of various diagnostics
i <- influence(model)
par(mfrow = c(2, 2))
boxplot(rstudent(model), sub = "Stud. res.")
boxplot(i$hat, sub = "leverages")
boxplot(cooks.distance(model), sub = "Cook's D")
boxplot(dffits(model), sub = "DFFITS")
# L-R plot
plot(
  rstandard(model) ^ 2,
  i$hat,
  pch = 19,
  xlab = "res**2",
  ylab = "leverage"
)
i0 <- which(i$hat > 0.5)
text(
  rstandard(model)[i0] ^ 2,
  i$hat[i0],
  labels = i0,
  cex = 0.9,
  font = 2,
  pos = 1
)


## Inverse simple linear regression(SLR)
# Datset: steamUse in the R package "robustbase"
data("steamUse", package = "robustbase")
attach(steamUse)
# Calibrate
library(EnvStats)
calibrate <- calibrate(Steam ~ temperature, data = steamUse)
summary(calibrate)
anova(calibrate)
newdata <-
  data.frame(temperature = seq(min(temperature),
                               max(temperature), length.out = 100))
# Predicted values
pred <-
  predict(calibrate, newdata = newdata, se.fit = TRUE)
pointwise <- pointwise(pred, coverage = 0.99, individual = TRUE)
inversePredictCalibrate(
  calibrate,
  obs.y = 10,
  intervals = TRUE,
  coverage = 0.95,
  individual = TRUE
)
