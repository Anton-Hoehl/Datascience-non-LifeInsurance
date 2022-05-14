##----package loading-----------------------------------------------------------------------
packages <- c("tidyverse",
              "readr",
              "tidymodels",
              "readxl",
              "gridExtra",
              "mgcv",
              "MASS",
              "poissonreg",
              "fitdistrplus",
              "mgcv")
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

#----data prep--------------------------------------------------------------------------------

mtpl <- mtpl %>% left_join(inspost, by = "CODPOSS")

mtpl <- mtpl %>%
  dplyr::select(-COMMUNE) %>%
  rename("ageph" = AGEPH,
         "codposs" = CODPOSS,
         "ins" = INS,
         "lat" = LAT,
         "long" = LONG) %>%
  mutate_if(is.character, as.factor)

mtpl <- mtpl %>%
  mutate(sev = ifelse(nbrtotc == 0, NA, chargtot / nbrtotc))

mtpl_sev <- mtpl %>% 
            filter(sev != is.na(sev)& sev < 80000) %>%
            dplyr::select(sev,ageph,lnexpo,agecar,sexp,fuelc,split,usec,fleetc,sportc,coverp,powerc,lat,long)

##----dist assumptions-----------------------------------------------------------------

set.seed(456)
gamma_fit <- fitdist(mtpl_sev$sev, distr = "gamma", method = "mle", lower = c(0,0))
lnorm_fit <- fitdist(mtpl_sev$sev, distr = "lnorm", method = "mle", lower = c(0,0))
  
g_gamma_vs_lnorm <- 
  gg.dens(mtpl_sev, "sev") +
  geom_function(aes(col = "gamma"), 
                fun = dgamma, 
                args = list(shape = gamma_fit$estimate[1],
                             rate = gamma_fit$estimate[2]),
                lwd = 1) +
  geom_function(aes(col = "log normal"), 
                fun = dlnorm, 
                args = list(meanlog = lnorm_fit$estimate[1],
                            sdlog = lnorm_fit$estimate[2]),
                lwd = 1) +
  scale_colour_manual(values = c("red","blue"))
  

##----data resampling------------------------------------------------------------------

set.seed(1234)
mtpl_sev_split <- initial_split(mtpl_sev, prop = 0.75, strata = sev)
mtpl_sev_training <- training(mtpl_sev_split)
mtpl_sev_test <- testing(mtpl_sev_split)

##----exhaustive gam search---------------------------------

glmfit1 <- glm(formula = sev ~ agecar + fuelc + usec + fleetc + sportc + coverp + powerc,
               family = Gamma(),
               data = mtpl_sev_training)


gamfit1 <- gam(formula = sev ~ s(ageph, k = 10),
               family = Gamma(),
               data = mtpl_sev_training) 

summary(gamfit1)
plot(gamfit1)
