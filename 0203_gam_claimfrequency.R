# Run '00_dataprep_package_loading.R' script before running this script

##----data resampling------------------------------------------------------------------
set.seed(1234)

mtpl_n <- mtpl %>% dplyr::select(-duree, -ins, -nbrtotan, -chargtot)

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

freq_glm_high <- glm(formula = nbrtotc ~ . -lnexpo -lat -long -ageph -codposs,
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
freq_spatial_gam <- gam(formula = nbrtotc ~ s(long,lat),
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
                 fleetc + coverp + powerc + s(ageph) + s(long,lat),
                family = poisson(link = "log"),
                offset = lnexpo,
                data = mtpl_training)


summary(freq_gam)
plot(freq_gam)
par(mfrow = c(2,2))
gam.check(freq_gam)

##---clustering latitude, longitude-------------------------------------------------------------
sf::sf_use_s2(FALSE)
post_be <- st_centroid(be_shape_sf)
post_be$long <- do.call(rbind, post_be$geometry)[,1]
post_be$lat <- do.call(rbind, post_be$geometry)[,2]

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

n_classes <- 2:5
AIC_nclasses_freq <- data.frame(n_classes = n_classes,
                                AIC = vector(mode = "numeric", length = length(n_classes)))
mtpl_n_geo <- mtpl_n %>% 
  left_join(select(be_pred, -long, -lat), by = c("codposs" = "postcode"))

for (i in 1:length(n_classes)) {
  n <- n_classes[i]
  
  fisher_classes <- classIntervals(var = be_pred$spatial_pred, n = n, style = "fisher")
          
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

AIC_nclasses_freq$AIC[i] <- freq_gam_geo$aic
}

n_optim <- AIC_nclasses_freq[which.min(AIC_nclasses_freq$AIC),1]

fisher_classes <- classIntervals(var = be_pred$spatial_pred, n = n_optim, style = "fisher")

g_fisher_freq <- plot(fisher_classes, pal = c("#F1EEF6","#980043"),
                      xlab = expression(hat(f)(long,lat)),
                      main = "Fisher natural breaks - claim frequency")

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

##----clustering policyholder age-------------------------------------------------------

#extracting unique predicted values using the smoothed effect of ageph
#count the number of each policyholder age in the dataset
#filter 0 exposures

pred_ageph <- predict(freq_ageph_gam, type = "terms", terms = "s(ageph)")

dt_pred_ageph_expo <- tibble("ageph" = mtpl_training_geo$ageph, pred_ageph) %>%
                      group_by(ageph,pred_ageph) %>%
                      summarize(n_ageph = n()) %>%
                      arrange(ageph) %>%
                      filter(n_ageph != 0)

#we use the evtree to create splits for the continuous variable ageph
#the control parameters can be tuned

control_pars <- 
  evtree.control(
                 minbucket = 0.05*nrow(mtpl_training), 
                 alpha = 550,
                 maxdepth = 5
                )
  
freq_ageph_evtree <- 
  evtree(
         pred_ageph ~ ageph,
         data = dt_pred_ageph_expo,
         weights = n_ageph,
         control = control_pars
        )

freq_ageph_evtree
plot(freq_ageph_evtree)

#compare with rpart decision tree
