library(singleRcapture)
library(singleRcaptureExtra)

## Basic usage ####

system.time(
  mm <- estimatePopsize(
    formula = capture ~ 1,
    controlModel = controlModel(omegaFormula = ~ 1),
    data = netherlandsimmigrant,
    model = oiztgeom(omegaLink = "cloglog")
    )
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


##
# won't converge
pseudoHurdleModel <- estimatePopsize(
  formula = capture ~ nation + gender,
  data = netherlandsimmigrant,
  model = ztHurdlegeom,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5),
  controlModel = controlModel(piFormula = ~ age * gender)
)

# will work
pseudoHurdleModel <- estimatePopsize(
  formula = capture ~ nation + gender,
  data = netherlandsimmigrant,
  model = Hurdleztgeom,
  method = "IRLS",
  controlMethod = controlMethod(verbose = 5, stepsize = 2),
  controlModel = controlModel(piFormula = ~ age * gender)
)


library(VGAM)
library(VGAMdata)
model <- vgam(
  counts ~ gender + sm.ps(age, o.order = 4),
  data = df_drunk,
  family = oipospoisson(),
  control = vgam.control(
    trace = TRUE,
    save.weights = TRUE,
    maxit = 30,
    Maxit.outer = 100
  )
)
plot(model, which.term = "sm.ps(age, o.order = 4)")

mm <- estimatePopsize(model)
summary(mm)
