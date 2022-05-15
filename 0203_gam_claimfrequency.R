# Run '00_dataprep_package_loading.R' script before running this script

##----data resampling------------------------------------------------------------------
set.seed(1234)

mtpl_n <- mtpl %>% dplyr::select(-duree, -ins, -nbrtotan, -chargtot, -codposs)

mtpl_split <- initial_split(mtpl_n, prop = 0.75, strata = nbrtotc)
mtpl_training <- training(mtpl_split)
mtpl_test <- testing(mtpl_split)

##----dist assumptions-------------------------------------------------------------
#we assume the number of claims to follow a poisson distribution
mean(mtpl_training$nbrtotc)
var(mtpl_training$nbrtotc)

##----GAM - model building - claim frequency-----------------------------------------------------------
#glm - exhaustive search (AIC)

freq_glm_low <- glm(formula = nbrtotc ~ 1,
                   family = poisson(link = "log"),
                   offset = lnexpo,
                   data = mtpl_training) #fitting the null model

freq_glm_high <- glm(formula = nbrtotc ~ . -lnexpo -lat -long -ageph,
                    family = poisson(link = "log"),
                    offset = lnexpo,
                    data = mtpl_training) #fitting the most complex model from candidate features, excluding interactions

summary(freq_glm_low)
summary(freq_glm_high)

step_freq <- stepAIC(freq_glm_high, 
                     scope = list(upper = freq_glm_high, lower = freq_glm_low),
                     method = "backward") #model forward step search based on lowest AIC value

freq_glm <- glm(formula = nbrtotc ~ agecar + sexp + fuelc + split + fleetc + coverp + powerc,
               family = poisson(link = "log"),
               offset = lnexpo,
               data = mtpl_training) #glm with the lowest AIC (we won't use this model)

summary(freq_glm)

#the smoothed univariate effect of ageph
freq_ageph_gam <- gam(formula = nbrtotc ~ s(ageph),
                      family = poisson(link = "log"),
                      offset = lnexpo,
                      data = mtpl_training) 

summary(freq_ageph_gam)
plot(freq_ageph_gam, scheme = 1)
g_ageph_nbrtotc
par(mfrow = c(2,2))
gam.check(freq_ageph_gam)

#the smoothed spatial effect of latitude and longitude
freq_spatial_gam <- gam(formula = nbrtotc ~ s(long,lat, bs = "tp"),
                       family = poisson(link = "log"),
                       offset = lnexpo,
                       data = mtpl_training) 

summary(freq_spatial_gam)
plot(freq_spatial_gam, scheme = 2)
par(mfrow = c(2,2))
gam.check(freq_spatial_gam)

#first gam spec - glm from previous step + smoother functions of non-linear effects of ageph and lat,long 
freq_gam <- gam(formula = 
                 nbrtotc ~ agecar + sexp + fuelc + split + 
                 fleetc + coverp + powerc + s(ageph) + s(long,lat, bs = "tp"),
                family = poisson(link = "log"),
                offset = lnexpo,
                data = mtpl_training)


summary(freq_gam)
plot(freq_gam)
par(mfrow = c(2,2))
gam.check(freq_gam)






