library(singleRcapture)


# Example 1, P. G. v. d. Heijden et al. 2003:
model <- estimatePopsize(
  formula = capture ~ gender + age + nation, 
  data    = netherlandsimmigrant,
  popVar  = "analytic",
  model   = ztpoisson,
  method  = "IRLS"
)
summary(model)

# Example 2, a more complicated call
# with inflation parameter depending on gender
modelInflated <- estimatePopsize(
  formula       = capture ~ age,
  controlModel  = controlModel(omegaFormula = ~ gender),
  data          = netherlandsimmigrant, 
  popVar        = "bootstrap",
  model         = oiztgeom(omegaLink = "cloglog"), 
  method        = "IRLS",
  controlPopVar = controlPopVar(
    B        = 600, 
    alpha    = .01,
    bootType = "semiparametric",
    bootstrapFitcontrol = controlMethod(
      epsilon  = 1e-6, 
      silent   = TRUE, 
      stepsize = 2
    ),
    bootstrapVisualTrace = TRUE
  )
)

summary(modelInflated)

# X_vlm matrix

AA1 <- model.matrix(model, type = "vlm")
AA2 <- model.matrix(modelInflated, type = "vlm")

head(AA1)
tail(AA1)

head(AA2)
tail(AA2)

# multiple linear predictors

head(predict(model))
head(predict(modelInflated))

# predict for singleRStaticCountData objects
# has some other nice perks
head(predict(model, type = "mean", se.fit = TRUE))
head(predict(modelInflated, type = "mean", se.fit = TRUE))

# "Weights" for each unit in the data
head(weights(modelInflated, type = "working"))
# in the matrix form
matrix(weights(modelInflated, type = "working")[1, ], ncol = 2)

# Marginal frequencies

mm1 <- marginalFreq(model)
mm2 <- marginalFreq(modelInflated)

mm1
mm2

summary(mm1)
summary(mm1, dropl5 = "group", df = 1)

summary(mm2)
summary(mm2, dropl5 = "group")

# Plot showcase

plot(model, plotType = "qq")
plot(model, plotType = "marginal")
plot(model, plotType = "fitresid")
plot(model, plotType = "cooks")
plot(model, plotType = "hatplot")
plot(model, plotType = "scaleLoc")

stratifyPopsize(model)
stratifyPopsize(model, stratas = ~ nation * gender * age)
# fails, because nation wasn't used in the model
stratifyPopsize(modelInflated, stratas = ~ nation)
# an alternative
target_list <- lapply(
  unique(netherlandsimmigrant$nation),
  FUN = function (x) {
    netherlandsimmigrant$nation == x
  }
)
names(target_list) <- unique(netherlandsimmigrant$nation)
stratifyPopsize(modelInflated, stratas = target_list)

plot(model, plotType = "strata")
plot(modelInflated, plotType = "strata")
# specifying stratas argument works
plot(modelInflated, plotType = "strata", stratas = target_list)

plot(model, plotType = "rootogram")
plot(modelInflated, plotType = "rootogram")

system.time(dfp <- dfpopsize(model, cores = 7))
system.time(dfp_no_multicore <- dfpopsize(model))

identical(dfp, dfp_no_multicore)

# Negative value indicates that removing an observation 
# leads to a greater population size estimate
plot(model, plotType = "dfpopContr", dfpop = dfp)
plot(model, plotType = "dfpopBox", dfpop = dfp)

# this will also work
#plot(modelInflated, plotType = "dfpopContr", cores = 7)
system.time(dfp <- dfpopsize(modelInflated, cores = 7))

plot(modelInflated, plotType = "dfpopContr", dfpop = dfp)
plot(modelInflated, plotType = "bootHist", histKernels = FALSE)
plot(modelInflated, plotType = "bootHist", ylim = c(0, 150), breaks = 30)

# weights example

df <- netherlandsimmigrant[, c(1:3,5)]
df$ww <- 0
### this is dplyr::count without dependencies
df <- aggregate(ww ~ ., df, FUN = length)
modelWeights <- estimatePopsize(
  formula = capture ~ nation + age + gender, 
  data = df, 
  model = ztpoisson, 
  weights = df$ww,
  controlModel = controlModel(weightsAsCounts = TRUE)
)

summary(modelWeights)

table(dfp_no_multicore)
table(dfpopsize(modelWeights))

# Troubleshooting example
# fails
model_neg <- estimatePopsize(
  formula = capture ~ reason,
  model   = ztoinegbin(),
  data    = netherlandsimmigrant,
  controlModel = controlModel(
    alphaFormula = ~ age,
    omegaFormula = ~ gender
  )
)

# checking out
model_neg <- estimatePopsize(
  formula = capture ~ reason,
  model   = ztoinegbin(),
  data    = netherlandsimmigrant,
  controlMethod = controlMethod(
    verbose = 5
  ),
  controlModel = controlModel(
    alphaFormula = ~ age,
    omegaFormula = ~ gender
  )
)

# the culprit is alpha parameter tending to -inf
# fixing:
model_neg <- estimatePopsize(
  formula = capture ~ reason,
  model   = ztoinegbin(),
  data    = netherlandsimmigrant,
  controlMethod = controlMethod(
    verbose = 5, stepsize = .06,
    momentumFactor = .5
  ),
  controlModel = controlModel(
    alphaFormula = ~ age,
    omegaFormula = ~ gender
  )
)

# countreg
library(countreg)
library(singleRcaptureExtra)

modelCountreg <- zerotrunc(
  formula = capture ~ gender + age + nation, 
  data = netherlandsimmigrant, 
  dist = "poisson"
)

estPopCountreg <- estimatePopsize(modelCountreg)
summary(estPopCountreg)
summary(modelWeights)

## VGAM
library(VGAM)
modelVglm <- vglm(
  formula = capture ~ gender + age + nation, 
  data    = netherlandsimmigrant, 
  family  = pospoisson
)

estPopVglm <- estimatePopsize(modelVglm)
summary(estPopVglm)

set.seed(123)
x <- rnorm(n = 1000)
y <- rpois(n = 1000, lambda = exp(-1 + cos(x)))
data <- data.frame(y = y, x = x)
data <- data[data$y > 0, ]

additiveModel <- vgam(
  formula = y ~ sm.os(x, df = 3), 
  data    = data, 
  family  = pospoisson()
)

summary(estimatePopsize(additiveModel))

fn <- function(x) {-1 + cos(x)}

plot(additiveModel)
curve(fn, from = min(x), to = max(x))

plot(additiveModel@predictors, -1 + cos(data$x), 
     xlab = "predictors",
     ylab = "reality")


# function showcase

mm <- estimatePopsize(
  formula = capture ~ 1,
  controlModel = controlModel(omegaFormula = ~ 1),
  data = netherlandsimmigrant,
  model = oiztgeom(omegaLink = "cloglog")
)

ff <- logLik(mm, type = "func")
values <- seq(from = -2, to = -.8, by = .12)
values <- expand.grid(values, values)

z <- sapply(1:nrow(values), function (x) ff(values[x, ] |> as.numeric()))

library(plotly)
plot_ly(x = values$Var1, 
        y = values$Var2, 
        z = z, 
        type = "scatter3d") |> 
  add_trace(x = mm$coefficients[1], 
            y = mm$coefficients[2], 
            z = - logLik(mm), 
            color = "red", 
            name = "minimizer")
