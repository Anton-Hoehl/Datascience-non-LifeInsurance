# Run '00_dataprep_package_loading.R' script before running this script

##----Severity_calculation--------------------------------------------------------------------------------

mtpl <- mtpl %>%
  mutate(sev = ifelse(nbrtotc == 0, NA, chargtot / nbrtotc)) #average claim amount calculation

mtpl_sev <- mtpl %>% 
            filter(sev != is.na(sev)& sev < 60000) %>%
            select(-lnexpo,-codposs,-chargtot,-nbrtotc,-nbrtotan)


str(mtpl_sev)
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

##---clustering the spatial effect-------------------------------------------------------------
sf::sf_use_s2(FALSE)
post_be_sev <- st_centroid(be_shape_sf)

post_be_sev$long <- do.call(rbind, post_be_sev$geometry)[,1]
post_be_sev$lat <- do.call(rbind, post_be_sev$geometry)[,2]

vars <- c("agecar","sexp","fuelc","split","fleetc","coverp","powerc","lnexpo","ageph")

for ( i in 1:length(vars)) {
  post_be[vars[i]] <- sample(pull(mtpl[vars[i]][1]), 1146, replace = T)
}

spatial_pred <- predict(freq_gam, newdata = post_be,
                        type = "terms", terms = "s(long,lat)")


be_pred <- tibble(postcode = post_be$POSTCODE,
                  long = post_be$long,
                  lat = post_be$lat,
                  spatial_pred)

#apply fisher natural breaks to create homogeneous clusters     
n_classes <- 5
fisher_classes <- classIntervals(var = be_pred$spatial_pred, n = n_classes, style = "fisher")
g_fisher_freq <- plot(fisher_classes, pal = c("#F1EEF6","#980043"),
                      xlab = expression(hat(f)(long,lat)),
                      main = "Fisher natural breaks - claim frequency")

be_shape_sf <- be_shape_sf %>%
  left_join(be_pred, by = c("POSTCODE" = "postcode")) %>%
  mutate(spatial_cluster = cut(
    x = spatial_pred,
    breaks = fisher_classes$brks,
    right = F, include.lowest = T,
    dig.lab = 2)
  )

g_geo_freq_risk <- ggplot(be_shape_sf) +
  geom_sf(aes(fill = spatial_cluster), colour = NA) +
  ggtitle("Belgium - MTPL claim frequency") +
  labs(fill = "Geographical riskiness") +
  scale_fill_brewer(palette = "PuRd",
                    na.value = "White") +
  theme_bw()

mtpl_n_geo <- mtpl_n %>% 
  left_join(select(be_pred, -long, -lat), by = c("codposs" = "postcode"))

mtpl_n_geo <- mtpl_n_geo %>%
  mutate(geo = cut(
    spatial_pred,
    breaks = fisher_classes$brks,
    right = F,
    include.lowest = T,
    dig.lab = 2
  ))


set.seed(1234)
mtpl_split_geo <- initial_split(mtpl_n_geo, prop = 0.75, strata = nbrtotc)
mtpl_training_geo <- training(mtpl_split_geo)
mtpl_test_geo <- testing(mtpl_split_geo)

set.seed(567)
freq_gam_geo <- gam(formula = 
                      nbrtotc ~ agecar + sexp + fuelc + split + 
                      fleetc + coverp + powerc + s(ageph) + geo,
                    family = poisson(link = "log"),
                    offset = lnexpo,
                    data = mtpl_training_geo)

summary(freq_gam_geo)
par(mfrow = c(2,2))
gam.check(freq_gam_geo)

