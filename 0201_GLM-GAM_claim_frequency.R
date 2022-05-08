##----package_loading-----------------------------------------------------------------------
packages <- c("tidyverse",
              "readr",
              "tidymodels",
              "readxl",
              "gridExtra",
              "mgcv",
              "MASS",
              "poissonreg")
suppressMessages(packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}))

##----dist_assumptions-----------------------------------------------------------------
mean(mtpl$nbrtotc)  
var(mtpl$nbrtotc)   #mean and variance aproximately the same
                    #we can assume the claim frequency follows a poisson distribution

##----data_resampling------------------------------------------------------------------
mtpl_split <- initial_split(mtpl, prop = 0.75, strata = nbrtotc)
mtpl_training <- training(mtpl_split)
mtpl_test <- testing(mtpl_split)

set.seed(1234)
mtpl_folds <- vfold_cv(mtpl_training, v = 5, strata = nbrtotc)

##----glm_spec--------------------------------------------------------------------------
poi.glm <- poisson_reg() %>%
           set_engine("glm") %>%
           set_mode("regression")

##----ModelSearch_AIC_AllVars--------------------------------------------------------------------
mtpl_df_train <- mtpl_training %>% dplyr::select(-sev,-duree,-ins,-nbrtotan,-chargtot)

poi.fit1 <- poi.glm %>%
            fit(nbrtotc ~ .,
                offset = lnexpo,
                data = mtpl_df_train)
tidy(poi.fit1)           

poi.fit <- glm(nbrtotc ~ .,
               offset = lnexpo,
               family = "poisson",
               data = mtpl_df_train)
summary(poi.fit)
