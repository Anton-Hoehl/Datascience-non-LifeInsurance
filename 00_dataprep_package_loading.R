##----package loading-----------------------------------------------------------------------
packages <- c("readr",
              "readxl",
              "gridExtra",
              "mgcv",
              "MASS",
              "poissonreg",
              "sf",
              "tidymodels",
              "tidyr",
              "fitdistrplus",
              "classInt",
              "RColorBrewer",
              "Metrics",
              "caret",
              "evtree")

suppressMessages(packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}))

##----data import-----------------------------------------------------------------------

mtpl <- read_delim(file = "Assignment.csv",
                   delim = ",",
                   col_names = T)

inspost <- read_xls("inspost.xls",
                    sheet = "inspost",
                    col_names = T)

str(mtpl)
str(inspost)

mtpl <- mtpl %>% left_join(inspost, by = "CODPOSS")

mtpl <- as_tibble(mtpl) %>%
  dplyr::select(-COMMUNE) %>%
  rename("ageph" = AGEPH,
         "codposs" = CODPOSS,
         "ins" = INS,
         "lat" = LAT,
         "long" = LONG) %>%
  mutate_if(is.character, as.factor)
