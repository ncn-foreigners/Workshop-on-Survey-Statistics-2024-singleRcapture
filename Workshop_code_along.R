library(singleRcapture)
library(singleRcaptureExtra)
library(tidyverse)
police_drunk_or_no_licence <- readRDS("policja-alko-bezpraw.rds")

df_drunk <- police_drunk_or_no_licence |> 
  filter(rok_wpis == 2022) |>
  filter(str_detect(kwalifikacja, "178")) |>
  count(id, gender = plec, 
        age = 2022 - rok_ur,
        name = "counts", 
        citizenship = obyw,
        woj, pow, gmi, 
        karalnosc,
        date = data, 
        kpp, kpw,
        kwalifikacja) |>
  filter(age >= 18) |>
  arrange(id)

ids_unique <- unique(df_drunk$id)

df_helper <- df_drunk[FALSE, ]

# This takes a lot of time to complete
for (k in ids_unique) {
  df_helper_1 <- df_drunk[df_drunk$id == k,]
  df_helper_2 <- df_helper_1[1,]
  
  df_helper_2 <- do.call("rbind", 
                         replicate(NROW(df_helper_1), 
                                   df_helper_2, 
                                   simplify = FALSE))
  df_helper <- rbind(df_helper, df_helper_1 == df_helper_2)
}

mark_delete <- function(k) {
  ddf <- df_helper[k, ]
  id <- df_drunk$id[k]
  ss <- sum(ddf == FALSE)
  
  if (k == 1) {
    return(FALSE)
  } else if (df_drunk$id[k-1] != df_drunk$id[k]) {
    return(FALSE)
  } else if (ddf$date == FALSE) {
    return(FALSE)
  } else if (all(ddf == TRUE)) {
    return(TRUE)
  } else if (ss == 1) {
    ddf_names <- colnames(ddf[, ddf==FALSE, drop = FALSE])
    if (ddf_names == "gmi") {
      TRUE
    } else if (ddf_names == "citizenship") {
      cits <- df_drunk[df_drunk$id == id, "citizenship"] |> unlist() |> paste(collapse = "|")
      if (grepl("POLSKA", x = cits)) {
        df_drunk[df_drunk$id == id, "citizenship"] <- "POLSKA"
        return(TRUE)
        # Rest I went thourgh manually
      } else if (cits == "UKRAINA|WĘGRY") {
        df_drunk[df_drunk$id == id, "citizenship"] <- "Inne"
        return(TRUE)
      } else if (cits == "BEZPAŃSTWOWIEC|ŁOTWA") {
        df_drunk[df_drunk$id == id, "citizenship"] <- "ŁOTWA"
        return(TRUE)
      }
    } else if (grepl(x = ddf_names, pattern = "kwalifikacja")) {
      # Myśle że to można na luzie wywalić
      return(TRUE)
    } else {
      return(ddf_names)
    }
  } else if (ss >= 2) {
    ddf_names <- colnames(ddf[, ddf==FALSE, drop = FALSE])
    ddf_names <- paste(ddf_names, collapse = "|")
    if (ddf_names == "pow|gmi" | ddf_names == "woj|gmi" | ddf_names == "woj|pow") {
      return(TRUE)
    } else if (ddf_names == "woj|pow|gmi") {
      return(TRUE)
    } else if (grepl(ddf_names, "citizenship")) {
      cits <- df_drunk[df_drunk$id == id, "citizenship"] |> unlist() |> paste(collapse = "|")
      if (grepl("POLSKA", x = cits)) {
        df_drunk[df_drunk$id == id, "citizenship"] <- "POLSKA"
        return(TRUE)
        # Rest I went through manually
      } else if (grepl(x = cits, pattern, "UKRAINA")) {
        df_drunk[df_drunk$id == id, "citizenship"] <- "UKRAINA"
        return(TRUE)
      }
    } else if (grepl(x = ddf_names, pattern = "kwalifikacja")) {
      # Myśle że to można na luzie wywalić
      return(TRUE)
    }
    return(ddf_names)
  } else {
    return("Error")
  } 
}

xx <- sapply(1:NROW(df_drunk), mark_delete)
table(xx)
#View(df_drunk |> filter(id %in% unique(df_drunk[which(xx == "gmi"), "id"])))
#View(df_drunk |> filter(id %in% unique(df_drunk[which(sapply(xx, function (x) grepl(x, "kwalifikacja"))),"id"])))

df_drunk_no_location <- df_drunk |>
  filter(xx == FALSE) |> 
  mutate(previous_offences = ifelse(str_sub(karalnosc, end = 1L) == "1", "No cirminal history",
                                    ifelse(str_sub(karalnosc, end = 1L) == "2", "Simmilar offences",
                                           ifelse(str_sub(karalnosc, end = 1L) == "3", "Different offences",
                                                  "No knowledge")))) |>
  count(name = "counts",
        id, gender, age, citizenship,
        previous_offences)

df_drunk <- df_drunk |>
  filter(xx == FALSE) |> 
  mutate(previous_offences = ifelse(str_sub(karalnosc, end = 1L) == "1", "No cirminal history",
                                    ifelse(str_sub(karalnosc, end = 1L) == "2", "Simmilar offences",
                                           ifelse(str_sub(karalnosc, end = 1L) == "3", "Different offences",
                                                  "No knowledge")))) |>
  count(name = "counts",
        id, gender, age, citizenship,
        previous_offences, kpp, kpw)

write.csv(x = df_drunk, file = "df_drunk")
write.csv(x = df_drunk_no_location, file = "df_drunk_no_location")

## Basic usage ####

#
library(plotly)

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
