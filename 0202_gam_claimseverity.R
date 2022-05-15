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
            dplyr::select(sev,ageph,lnexpo,agecar,sexp,fuelc,split,usec,fleetc,sportc,coverp,powerc,lat,long,nbrtotc)

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

##----gam search for severity-----------------------------------------------------------

sev_glm_low <- glm(formula = log(sev) ~ 1,
                      family = gaussian(),
                      weights = nbrtotc,
                      data = mtpl_sev_training)

sev_glm_high <- glm(formula = log(sev) ~ . -lnexpo -long -lat -nbrtotc -ageph,
                       family = gaussian(),
                       weights = nbrtotc,
                       data = mtpl_sev_training)

summary(sevfit_glm_low)
summary(sevfit_glm_high)

step_sev <- stepAIC(sev_glm_low, 
            scope = list(upper = sev_glm_high, lower = sev_glm_low),
            method = "forward")

sev_glm <- glm(formula = log(sev) ~ split + coverp + agecar + fleetc + powerc,
                  family = gaussian(),
                  weights = nbrtotc,
                  data = mtpl_sev_training)

summary(sev_glm)

sev_gam1 <- gam(formula = 
                log(sev) ~ split + coverp + agecar + fleetc + powerc + s(ageph) + s(lat,long),
               family = gaussian(),
               weights = nbrtotc,
               data = mtpl_sev_training) 


summary(sev_gam1)
plot(sev_gam1)
par(mfrow = c(2,2))
gam.check(sev_gam1)

tuning_grid <- expand.grid(sp = seq(1, 3, by = 0.01), k = 1:10)


fun <- function(x, sp, k) { 
  paste("s(",x,", sp =",as.character(sp),", k =",as.character(k),")")
}


gam.tune <- function(response,
                     variables,
                     smooth,
                     grid,
                     family = gaussian(),
                     data,
                     weights = NULL,
                     subset = NULL,
                     offset = NULL,
                     method = "GCV.Cp") 
{
  for(i in 1:length(grid)) {
    
    grid_crit <- cbind(grid, deviance = matrix(0,length(grid),1))
    
    sp <- grid[1,i]
    k <- grid[2,i]
    
    smoothers <- sapply(FUN = fun, smooth, sp, k)
    
    glm_predictor <- paste(variables, collapse = " + ")
    smooth_fun <- paste(smoothers, collapse = " + ")
    predictor <- paste(glm_predictor, smooth_fun, sep = " + ")
    f <- as.formula(paste(response, predictor, sep = " ~ "))
    
    model <- gam(formula = f,
                 family = family,
                 data = data,
                 weights = weights)
    
    grid_crit$deviance <- model$deviance
    return(grid_crit[which.min(grid_crit$deviance),])
    
    
  }
}

gam.tune(response = "log(sev)",
         variables = c("split","coverp","agecar","fleetc","powerc"),
         smooth = c("ageph","lat,long"),
         grid = tuning_grid,
         data = mtpl_sev_training,
         weights = mtpl_sev_training$nbrtotc)


