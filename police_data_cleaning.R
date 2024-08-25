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


df_drunk <- df_drunk |>
  filter(xx == FALSE) |> 
  mutate(previous_offences = ifelse(
           str_sub(karalnosc, end = 1L) == "1", "No cirminal history",
    ifelse(str_sub(karalnosc, end = 1L) == "2", "Simmilar offences",
    ifelse(str_sub(karalnosc, end = 1L) == "3", "Different offences",
                  "No knowledge")))
  ) |>
  count(name = "counts",
        id, gender, age, citizenship,
        previous_offences, kpp, kpw)

# translating from ploish to english
df_drunk$citizenship <- sapply(df_drunk$citizenship, FUN = function (x) {
    xx <- switch (x,
      "ARABIA SAUDYJSKA"                = "SAUDI ARABIA",
      "ARGENTYNA"                       = "ARGENTINA",
      "AZERBEJDŻAN"                     = "AZERBAIJAN",
      "BANGLADESZ"                      = "BANGLADESH",
      "BELGIA"                          = "BELGIUM",
      "BEZPAŃSTWOWIEC"                  = "STATELESS",
      "BIAŁORUŚ"                        = "BELARUS",
      "BOŚNIA I HERCEGOWINA"            = "BOSNIA AND HERZEGOVINA",
      "BRAZYLIA"                        = "BRAZIL",
      "BUŁGARIA"                        = "BULGARIA",
      "CHINY"                           = "CHINA",
      "CHORWACJA"                       = "CROATIA",
      "CZECHY"                          = "CZECH REPUBLIC",
      "DANIA"                           = "DENMARK",
      "DOMINIKANA"                      = "DOMINICAN REPUBLIC",
      "EGIPT"                           = "EGYPT",
      "FILIPINY"                        = "PHILIPINES",
      "FINLANDIA"                       = "FINLAND",
      "FRANCJA"                         = "FRANCE",
      "GRECJA"                          = "GREECE",
      "GRUZJA"                          = "GEORGIA",
      "GWATEMALA"                       = "GUATEMALA",
      "HISZPANIA"                       = "SPAIN",
      "HOLANDIA"                        = "NETHERLANDS",
      "INDIE"                           = "INDIA",
      "IRLANDIA"                        = "IRELAND",
      "IZRAEL"                          = "ISRAEL",
      "JAPONIA"                         = "JAPAN",
      "KAZACHSTAN"                      = "KAZAKHSTAN",
      "KENIA"                           = "KENYA",
      "KIRGISTAN (KIRGIZJA)"            = "KYRGYZSTAN",
      "KONGO"                           = "CONGO", # WHICH CONGO!!
      "KOREA POŁUDNIOWA"                = "SOUTH KOREA",
      "KUWEJT"                          = "KUWAIT",
      "LITWA"                           = "LITHUANIA",
      "ŁOTWA"                           = "LATVIA",
      "MAROKO"                          = "MOROCO",
      "MEKSYK"                          = "MEXICO",
      "MOŁDAWIA"                        = "MOLDOVA",
      "NIEMCY"                          = "GERMANY",
      "NORWEGIA"                        = "NORWAY",
      "PALESTYNA"                       = "PALESTINE",
      "POLSKA"                          = "POLAND",
      "PORTUGALIA"                      = "PORTUGAL",
      "ROSJA"                           = "RUSSIA",
      "RUMUNIA"                         = "ROMANIA",
      "SINGAPUR"                        = "SINGAPORE",
      "SŁOWACJA"                        = "SLOVAKIA",
      "SZWECJA"                         = "SWEDEN",
      "TADŻYKISTAN"                     = "TAJIKISTAN",
      "TUNEZJA"                         = "TUNISIA",
      "TURCJA"                          = "TURKEY",
      "TURKMENISTAN (TURKMENIA)"        = "TURKMENISTAN",
      "UKRAINA"                         = "UKRAINE",
      "USA (STANY ZJEDNOCZONE AMERYKI)" = "USA",
      "UZBEKISTAN"                      = "UZBEKISTAN",
      "WĘGRY"                           = "HUNGARY",
      "WENEZUELA"                       = "VENEZUELA",
      "WIELKA BRYTANIA"                 = "GREAT BRITAIN",
      "WIETNAM"                         = "VIETNAM",
      "WŁOCHY"                          = "ITALY",
      "ZIMBABWE (RODEZJA)"              = "ZIMBABWE"
    )
    
    if (is.null(xx)) xx <- x
    
    xx
  }
) |> as.vector()

write.csv(x = df_drunk, file = "df_drunk", row.names = FALSE)
