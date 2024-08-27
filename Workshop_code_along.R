install.packages("singleRcapture")
remotes::install_github("https://github.com/ncn-foreigners/singleRcaptureExtra")

library(singleRcapture)
library(singleRcaptureExtra)

df_drunk <- read.csv("~/Desktop/Workshop-on-Survey-Statistics-2024-singleRcapture/df_drunk")
head(df_drunk)

tab <- table(df_drunk$citizenship)
nmm <- names(tab[tab > 200])

df_drunk$citizenship <- ifelse(
  df_drunk$citizenship %in% nmm,
  df_drunk$citizenship,
  "OTHER"
)

#library(countreg)
model <- estimatePopsize(
  formula = counts ~ gender + citizenship,
  data = df_drunk,
  model = singleRcapture::ztpoisson()
)
summary(model)

model_2 <- estimatePopsize(
  formula = counts ~ gender + citizenship,
  data = df_drunk,
  model = ztoipoisson(omegaLink = "cloglog"),
  controlModel = controlModel(
    omegaFormula = ~ gender + citizenship
  ),
  controlMethod = controlMethod(
    verbose = 5
  )
)
summary(model_2)

model_2_reduced <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk,
  model = ztoipoisson(omegaLink = "cloglog"),
  controlModel = controlModel(
    omegaFormula = ~ gender + poly(age, degree = 3)
  ),
  controlMethod = controlMethod(
    verbose = 5
  )
)
summary(model_2_reduced)

AIC(model_2)
AIC(model_2_reduced)

library(lmtest)
lrtest(model_2, model_2_reduced)

lrtest(model_2_reduced, model)

model_3 <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk,
  model = ztoinegbin(
    omegaLink = "cloglog"
  ),
  controlModel = controlModel(
    omegaFormula = ~ gender + poly(age, degree = 3)
  ),
  controlMethod = controlMethod(
    verbose = 5, stepsize = .25
  )
)
summary(model_3)

summary(marginalFreq(model_2_reduced), dropl5 = "group", df = 1)
summary(marginalFreq(model_3), dropl5 = "group", df = 1)

lrtest(model_2_reduced, model_3)

library(VGAM)
library(VGAMdata)

additive_model <- vgam(
  formula = counts ~ gender + sm.os(age, df = 3),
  data = df_drunk,
  family = VGAMdata::oipospoisson(lpstr1 = "clogloglink"),
  trace = TRUE
)

model_4 <- estimatePopsize(additive_model)

plot(additive_model, which.term = 2)

summary(model_4)

AIC(model_3)
AIC(model_4)

# model_5 <- estimatePopsize(
#   formula = counts ~ gender + age + citizenship,
#   data = df_drunk,
#   model = singleRcapture::ztpoisson,
#   popVar = "bootstrap",
#   controlPopVar = controlPopVar(
#     bootType = "semiparametric",
#     confType = "basic",
#     alpha = .001,
#     traceBootstrapSize = TRUE,
#     bootstrapVisualTrace = TRUE
#   )
# )

model_5 <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk,
  model = Hurdleztgeom(),
  controlModel = controlModel(
    piFormula = ~ gender + poly(age, degree = 3)
  )
)

model_6 <- estimatePopsize(
  formula = counts ~ 1,
  data = df_drunk,
  model = Hurdleztnegbin(),
  controlModel = controlModel(
    piFormula = ~ gender + poly(age, degree = 3)
  ),
  controlMethod = controlMethod(
    verbose = 4,
    stepsize = .35,
    momentumFactor = 1
  )
)
?controlMethod

library(ggplot2)

fitt_data <- data.frame(
  "point" = c(model$populationSize$pointEstimate,
              model_2_reduced$populationSize$pointEstimate,
              model_3$populationSize$pointEstimate,
              model_4$populationSize$pointEstimate,
              model_5$populationSize$pointEstimate,
              model_6$populationSize$pointEstimate),
  "lower" = c(model$populationSize$confidenceInterval[1, 1],
              model_2_reduced$populationSize$confidenceInterval[1, 1],
              model_3$populationSize$confidenceInterval[1, 1],
              model_4$populationSize$confidenceInterval[1, 1],
              model_5$populationSize$confidenceInterval[1, 1],
              model_6$populationSize$confidenceInterval[1, 1]),
  "upper" = c(model$populationSize$confidenceInterval[1, 2],
              model_2_reduced$populationSize$confidenceInterval[1, 2],
              model_3$populationSize$confidenceInterval[1, 2],
              model_4$populationSize$confidenceInterval[1, 2],
              model_5$populationSize$confidenceInterval[1, 2],
              model_6$populationSize$confidenceInterval[1, 2]),
  "name" = c("ztpoisson", "ztoipoisson", "ztoinegbin", "additive",
             "Hurdleztgeom", "Hurdleztnegbin")
)

library(ggplot2)
ggplot(fitt_data) +
  geom_point(aes(y = name, x = point)) +
  geom_errorbar(aes(y = name, x = point, xmin = lower, xmax = upper))
  

# dpop <- dfpopsize(model_3, cores = 5)
# plot(model_3, plotType = "dfpopContr", )

plot(model_3, plotType = "scaleLoc")
?singleRcapture:::plot.singleRStaticCountData
