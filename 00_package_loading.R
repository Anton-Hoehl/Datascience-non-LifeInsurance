##----package loading-----------------------------------------------------------------------
packages <- c("readr",
              "readxl",
              "gridExtra",
              "mgcv",
              "gam",
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
              "evtree", 
              "xgboost",
              "rpart.plot",
              "kableExtra",
              "factoextra",
              "insight")

suppressMessages(packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}))
