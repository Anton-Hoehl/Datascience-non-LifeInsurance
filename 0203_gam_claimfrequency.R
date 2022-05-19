# Run '00_dataprep_package_loading.R' script before running this script

##----data resampling------------------------------------------------------------------

set.seed(1234)

mtpl_n <- mtpl %>% dplyr::select(-duree, -ins, -nbrtotan, -chargtot, -sev)

mtpl_split <- initial_split(mtpl_n, prop = 0.75, strata = nbrtotc)
mtpl_training <- training(mtpl_split)
mtpl_test <- testing(mtpl_split)

##----dist assumptions-------------------------------------------------------------
#we assume the number of claims to follow a poisson distribution

mean(mtpl_training$nbrtotc)
var(mtpl_training$nbrtotc)

##---the smoothed univariate effect of ageph---------------------------------------------

freq_ageph_gam <- mgcv::gam(formula = nbrtotc ~ s(ageph),
                            family = poisson(link = "log"),
                            offset = lnexpo,
                            data = mtpl_training,
                            method = "REML") 


summary(freq_ageph_gam)
plot(freq_ageph_gam, scheme = 1)

##---searching for the number of basis functions for s(ageph)---------------------------------------
k <- 1:15
freq_ageph_gam_list <- list()
ageph_gam_aic <- vector(mode = "numeric", length = length(k))

for (i in k) {
  ageph_form <- formula(paste("nbrtotc ~ s(ageph, k =",as.character(i),")"))
  
  
  freq_ageph_gam_list[[i]] <- mgcv::gam(formula = ageph_form,
                                        family = poisson(link = "log"),
                                        offset = lnexpo,
                                        data = mtpl_training,
                                        method = "REML")
  
  ageph_gam_aic[i] <- freq_ageph_gam_list[[i]]$aic
}

k_ageph <- which.min(ageph_gam_aic)

##----the smoothed spatial effect of latitude and longitude------------------------------------------

freq_spatial_gam <- mgcv::gam(formula = nbrtotc ~ s(long,lat),
                              family = poisson(link = "log"),
                              offset = lnexpo,
                              data = mtpl_training,
                              method = "REML") 

summary(freq_spatial_gam)
plot(freq_spatial_gam, scheme = 2)

##---searching for the number of basis functions for s(long,lat)---------------------------------------
k2 <- 1:25
freq_spatial_gam_list <- list()
spatial_gam_aic <- vector(mode = "numeric", length = length(k))

for (i in k2) {
  spatial_form <- formula(paste("nbrtotc ~ s(long,lat, k =",as.character(i),")"))
  
  
  freq_spatial_gam_list[[i]] <- mgcv::gam(formula = spatial_form,
                                        family = poisson(link = "log"),
                                        offset = lnexpo,
                                        data = mtpl_training,
                                        method = "REML")
  
  spatial_gam_aic[i] <- freq_spatial_gam_list[[i]]$aic
}

k_spatial <- which.min(spatial_gam_aic)
plot(freq_spatial_gam_list[[k_spatial]], scheme = 2)

##----step gam search for frequency-------------------------------------------------------------

freq_scope_init <- gam.scope(mtpl_training,
                             response = mtpl_training$nbrtotc,
                             smoother = "s")

freq_scope_init <- within(freq_scope_init, rm(lat,long))

freq_scope_add <- list("agecar" = ~ 1 + agecar,
                       "ageph" = ~ 1 + ageph + s(ageph, k = 6),
                      "latlong" = ~ 1 + s(long,lat, k = 25),
                      "sexp" = ~ 1 + sexp)


freq_scope <- c(freq_scope_init,freq_scope_add)
                  

gam_freq_lower <- gam(formula = nbrtotc ~ 1,
                      family = poisson(link = "log"),
                      offset = lnexpo,
                      data = mtpl_training,
                      method = "REML")

step_gam_freq <- step.Gam(gam_freq_lower,
                          scope = freq_scope,
                          direction = "both")

step_gam_freq$formula

##---gam spec for claim frequency--------------------------------------------------------------

freq_gam <- mgcv::gam(
                  formula = 
                  nbrtotc ~ fuelc + split + usec + coverp + 
                  powerc + agecar + s(ageph, k = 6) + s(long,lat, k = 25),
                  family = poisson(link = "log"),
                  offset = lnexpo,
                  data = mtpl_training,
                  method = "REML")


summary(freq_gam)
par(mfrow = c(2,2))
gam.check(freq_gam)

##---clustering the spatial effect-------------------------------------------------------------
sf::sf_use_s2(FALSE)
post_be <- st_centroid(be_shape_sf)
post_be$long <- do.call(rbind, post_be$geometry)[,1]
post_be$lat <- do.call(rbind, post_be$geometry)[,2]

vars <- c("agecar","usec","fuelc","split","fleetc","coverp","powerc","lnexpo","ageph")

for ( i in 1:length(vars)) {
  post_be[vars[i]] <- sample(pull(mtpl[vars[i]][1]), 1146, replace = T)
}

spatial_pred <- predict(freq_gam, newdata = post_be,
                        type = "terms", terms = "s(long,lat)")


be_pred <- tibble(postcode = post_be$POSTCODE,
                  long = post_be$long,
                  lat = post_be$lat,
                  spatial_pred)

##----apply fisher natural breaks to create homogeneous clusters of s(lat,long)---------------------------------------------   

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
  freq_gam_geo <- mgcv::gam(formula = 
                            nbrtotc ~ fuelc + split + usec + coverp + 
                            powerc + agecar + s(ageph, k = 6) + geo,
                            family = poisson(link = "log"),
                            offset = lnexpo,
                            data = mtpl_training_geo,
                            method = "REML")
  
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
freq_gam_geo <- mgcv::gam(formula = 
                          nbrtotc ~ fuelc + split + usec + coverp + 
                          powerc + agecar + s(ageph, k = 6) + geo,
                          family = poisson(link = "log"),
                          offset = lnexpo,
                          data = mtpl_training_geo,
                          method = "REML")


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

pred_ageph <- predict(freq_ageph_gam_list[[k_ageph]], type = "terms", terms = "s(ageph)")

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

freq_tree_spec <- decision_tree(
                  min_n = 0.1*nrow(dt_pred_ageph_expo),
                  tree_depth = 3,
                  cost_complexity = 0.013) %>%
                  set_engine("rpart") %>%
                  set_mode("regression")


freq_ageph_tree <- freq_tree_spec %>%
                   fit(pred_ageph ~ ageph,
                       data = dt_pred_ageph_expo)


freq_ageph_tree$fit
freq_ageph_tree$fit$splits
rpart.plot(freq_ageph_tree$fit, roundint = F)

##----tune the tree_depth and cost complexity----------------------------

freq_tune_spec <- decision_tree(
                  tree_depth = tune(),
                  cost_complexity = tune()) %>%
                  set_mode("regression") %>%
                  set_engine("rpart")

freq_tree_grid <- expand.grid(cost_complexity = seq(0.001,0.5, by = 0.02),
                              tree_depth = 2:3)

set.seed(1234)
freq_folds <- vfold_cv(dt_pred_ageph_expo, v = 6)

freq_tune_results <- tune_grid(freq_tune_spec,
                               pred_ageph ~ ageph,
                               resamples = freq_folds,
                               grid = freq_tree_grid,
                               metrics = metric_set(yardstick::rmse))

autoplot(freq_tune_results)


