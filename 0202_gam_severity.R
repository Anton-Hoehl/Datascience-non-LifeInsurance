# Run '00_dataprep_package_loading.R' script before running this script

##----Severity_calculation--------------------------------------------------------------------------------

mtpl <- mtpl %>%
  mutate(sev = ifelse(nbrtotc == 0, NA, chargtot / nbrtotc)) #average claim amount calculation

mtpl_sev <- mtpl %>% 
            filter(sev != is.na(sev)& sev < 60000) %>%
            dplyr::select(sev,ageph,lnexpo,agecar,sexp,fuelc,split,usec,fleetc,sportc,coverp,powerc,lat,long,nbrtotc)

##----dist_assumptions - Severity-----------------------------------------------------------------

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

##----GAM - model building - claim severity-----------------------------------------------------------
#glm - exhaustive search (AIC)
sev_glm_low <- glm(formula = log(sev) ~ 1,
                      family = gaussian(),
                      weights = nbrtotc,
                      data = mtpl_sev_training) #fitting the null model

sev_glm_high <- glm(formula = log(sev) ~ . -lnexpo -long -lat -nbrtotc -ageph,
                       family = gaussian(),
                       weights = nbrtotc,
                       data = mtpl_sev_training) #fitting the most complex model from candidate features, excluding interactions

summary(sev_glm_low)
summary(sev_glm_high)

step_sev <- stepAIC(sev_glm_low, 
            scope = list(upper = sev_glm_high, lower = sev_glm_low),
            method = "forward") #model forward step search based on lowest AIC value

sev_glm <- glm(formula = log(sev) ~ split + coverp + agecar + fleetc + powerc,
               family = gaussian(),
               weights = nbrtotc,
               data = mtpl_sev_training) #glm with the lowest AIC (we won't use this model)

summary(sev_glm)

#the smoothed univariate effect of ageph
sev_ageph_gam1 <- gam(formula = log(sev) ~ s(ageph),
                      family = gaussian(),
                      weights = nbrtotc,
                      data = mtpl_sev_training) 

summary(sev_ageph_gam1)
plot(sev_ageph_gam1, scheme = 1)
par(mfrow = c(2,2))
gam.check(sev_ageph_gam1)


#the smoothed spatial effect of latitude and longitude
sev_spatial_gam <- gam(formula = log(sev) ~ s(long,lat, bs = "tp"),
                      family = gaussian(),
                      weights = nbrtotc,
                      data = mtpl_sev_training) 

summary(sev_spatial_gam)
plot(sev_spatial_gam, scheme = 2)
par(mfrow = c(2,2))
gam.check(sev_spatial_gam)

#first gam spec - glm from previous step + smoother functions of non-linear effects of ageph and lat,long 
sev_gam <- gam(formula = 
                log(sev) ~ split + coverp + agecar + 
                  fleetc + powerc + s(ageph) + s(long,lat, bs = "tp"),
                family = gaussian(),
                weights = nbrtotc,
                data = mtpl_sev_training)


summary(sev_gam)
plot(sev_gam)
par(mfrow = c(2,2))
gam.check(sev_gam)

##---Belgium map with fitted spatial effects----------------------------------------------------

post_be <- st_centroid(be_shape_sf) #this part doesn't work, without this I can't plot the fitted spatial values in the belgium map shape file


be_sevfit_spatial <- ggplot(be_shape_sf) +
  geom_sf(aes(fill = sev_spatial_gam$fitted.values)) +
  ggtitle("Belgium - MTPL severity data") +
  labs(fill = "level of severity") +
  scale_fill_gradient() +
  theme_bw()

