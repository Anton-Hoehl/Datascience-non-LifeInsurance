source("01_dataprep.R")

##---the smoothed univariate effect of ageph---------------------------------------------

sev_ageph_gam <- mgcv::gam(formula = lnsev ~ s(ageph),
                            family = gaussian(),
                            weights = nbrtotc,
                            data = mtpl_sev_training,
                            method = "REML") 


summary(sev_ageph_gam)
plot(sev_ageph_gam, scheme = 1)

##---searching for the number of basis functions for s(ageph)---------------------------------------
k <- 1:15
sev_ageph_gam_list <- list()
sev_ageph_gam_aic <- vector(mode = "numeric", length = length(k))

for (i in k) {
  sev_ageph_form <- formula(paste("lnsev ~ s(ageph, k =",as.character(i),")"))
  
  
  sev_ageph_gam_list[[i]] <- mgcv::gam(formula = sev_ageph_form,
                                        family = gaussian(),
                                        weights = nbrtotc,
                                        data = mtpl_sev_training,
                                        method = "REML")
  
  sev_ageph_gam_aic[i] <- sev_ageph_gam_list[[i]]$aic
}

k_sev_ageph <- which.min(sev_ageph_gam_aic)

##----the smoothed spatial effect of latitude and longitude------------------------------------------

sev_spatial_gam <- mgcv::gam(formula = lnsev ~ s(long,lat),
                              family = gaussian(),
                              weights = nbrtotc,
                              data = mtpl_sev_training,
                              method = "REML") 

summary(sev_spatial_gam)
plot(sev_spatial_gam, scheme = 2)

##---searching for the number of basis functions for s(long,lat)---------------------------------------
k2 <- 1:25
sev_spatial_gam_list <- list()
sev_spatial_gam_aic <- vector(mode = "numeric", length = length(k))

for (i in k2) {
  sev_spatial_form <- formula(paste("lnsev ~ s(long,lat, k =",as.character(i),")"))
  
  
  sev_spatial_gam_list[[i]] <- mgcv::gam(formula = sev_spatial_form,
                                          family = gaussian(),
                                          weights = nbrtotc,
                                          data = mtpl_sev_training,
                                          method = "REML")
  
  sev_spatial_gam_aic[i] <- sev_spatial_gam_list[[i]]$aic
}

k_sev_spatial <- which.min(sev_spatial_gam_aic)
plot(sev_spatial_gam_list[[k_sev_spatial]], scheme = 2)

##----step gam search for log(severity)-------------------------------------------------------------

#source("0205_sev_gam_step_search.R")

##---gam spec for claim severity--------------------------------------------------------------

sev_gam <- mgcv::gam(
  formula = lnsev ~ agecar + coverp + split + s(ageph, k = 6),
  family = gaussian(),
  weights = nbrtotc,
  data = mtpl_sev_training,
  method = "REML")


summary(sev_gam)
par(mfrow = c(2,2))
gam.check(sev_gam)
plot(sev_gam, scheme = 1)

# ##---clustering the spatial effect-------------------------------------------------------------
# sf::sf_use_s2(FALSE)
# post_be <- st_centroid(be_shape_sf)
# post_be$long <- do.call(rbind, post_be$geometry)[,1]
# post_be$lat <- do.call(rbind, post_be$geometry)[,2]
# 
# vars <- c("coverp","ageph","agecar")
# 
# for ( i in 1:length(vars)) {
#   post_be[vars[i]] <- sample(pull(mtpl[vars[i]][1]), 1146, replace = T)
# }
# 
# spatial_pred_sev <- predict(sev_gam, newdata = post_be,
#                         type = "terms", terms = "s(long,lat)")
# 
# 
# be_pred_sev <- tibble(postcode = post_be$POSTCODE,
#                   long = post_be$long,
#                   lat = post_be$lat,
#                   spatial_pred_sev)

##----apply fisher natural breaks to create homogeneous clusters of s(lat,long)---------------------------------------------   

# n_classes <- 2:15
# AIC_nclasses_sev <- data.frame(n_classes = n_classes,
#                                 AIC = vector(mode = "numeric", length = length(n_classes)))
# mtpl_sev_geo <- mtpl_sev %>% 
#   left_join(select(be_pred_sev, -long, -lat), by = c("codposs" = "postcode"))
# 
# for (i in 1:length(n_classes)) {
#   
#   n <- n_classes[i]
#   
#   fisher_classes_sev <- classIntervals(var = be_pred_sev$spatial_pred_sev, n = n, style = "fisher")
#   
#   mtpl_sev_geo <- mtpl_sev_geo %>%
#     mutate(geo = cut(
#       spatial_pred_sev,
#       breaks = fisher_classes_sev$brks,
#       right = F,
#       include.lowest = T,
#       dig.lab = 2
#     ))
#   
#   set.seed(1234)
#   mtpl_sev_geo_split <- initial_split(mtpl_sev_geo, prop = 0.75, strata = lnsev)
#   mtpl_sev_training_geo <- training(mtpl_sev_geo_split)
#   
#   sev_gam_geo <- mgcv::gam(formula = lnsev ~ coverp + 
#                              agecar + s(ageph, k = 6) + geo,
#                             weights = nbrtotc,
#                             data = mtpl_sev_training_geo,
#                             method = "REML")
#   
#   AIC_nclasses_sev$AIC[i] <- sev_gam_geo$aic
# }
# 
# n_optim_sev <- AIC_nclasses_sev[which.min(AIC_nclasses_sev$AIC),1]
# 
# fisher_classes_sev <- classIntervals(var = be_pred_sev$spatial_pred_sev, n = n_optim_sev, style = "fisher")
# 
# g_fisher_sev <- plot(fisher_classes_sev, pal = c("#F1EEF6","#980043"),
#                       xlab = expression(hat(f)(long,lat)),
#                       main = "Fisher natural breaks - claim severity")
# 
# mtpl_sev_geo <- mtpl_sev_geo %>%
#   mutate(geo = cut(
#     spatial_pred_sev,
#     breaks = fisher_classes_sev$brks,
#     right = F,
#     include.lowest = T,
#     dig.lab = 2
#   ))
# 
# set.seed(1234)
# mtpl_sev_geo_split <- initial_split(mtpl_sev_geo, prop = 0.75, strata = lnsev)
# mtpl_sev_training_geo <- training(mtpl_sev_geo_split)
# 
# sev_gam_geo <- mgcv::gam(formula = lnsev ~ coverp + 
#                            agecar + s(ageph, k = 6) + geo,
#                          weights = nbrtotc,
#                          data = mtpl_sev_training_geo,
#                          method = "REML")
# 
# 
# 
# be_shape_sf <- be_shape_sf %>%
#   left_join(be_pred_sev, by = c("POSTCODE" = "postcode")) %>%
#   mutate(spatial_cluster_sev = cut(
#     x = spatial_pred_sev,
#     breaks = fisher_classes_sev$brks,
#     right = F, include.lowest = T,
#     dig.lab = 2)
#   )
# 
# g_geo_sev_risk <- ggplot(be_shape_sf) +
#   geom_sf(aes(fill = spatial_cluster_sev), colour = NA) +
#   ggtitle("Belgium - MTPL claim severity") +
#   labs(fill = "Geographical riskiness") +
#   scale_fill_brewer(palette = "PuRd",
#                     na.value = "White") +
#   theme_bw()

##----clustering policyholder age-------------------------------------------------------

#extracting predicted values using the smoothed effect of ageph

pred_ageph <- predict(sev_gam, type = "terms", terms = "s(ageph)")

dt_pred_ageph_expo <- tibble("ageph" = mtpl_sev_training$ageph, pred_ageph) %>%
  group_by(ageph,pred_ageph) %>%
  summarize(n_ageph = n()) %>%
  arrange(ageph) %>%
  filter(n_ageph != 0)

#we use the evtree to create splits for the continuous variable ageph
#the control parameters can be tuned

control_pars <- 
  evtree.control(
    minbucket = 0.05*nrow(mtpl_sev_training), 
    alpha = 70,
    maxdepth = 4,
    minsplit = 1400,
    ntrees = 500
  )

sev_ageph_evtree <- 
  evtree(
    pred_ageph ~ ageph,
    data = dt_pred_ageph_expo,
    weights = n_ageph,
    control = control_pars
  )

sev_ageph_evtree

sev_ageph_evtree$node$split

plot(sev_ageph_evtree)

g_ageph_sevfreq +
  geom_vline(xintercept = c(25,29,45,65,72))


##----extracting splits created by the tree and adding them to the data set used for modeling------------------------------

sev_ageph_evtree_pred <- predict(sev_ageph_evtree, type = "node")

sev_ageph_evtree_nodes <- tibble("ageph" = dt_pred_ageph_expo$ageph, 
                                  "nodes" = sev_ageph_evtree_pred) %>%
                          mutate(change = c(0, pmin(1, diff(nodes))))

sev_ageph_splits <- unique(c(min(mtpl_sev$ageph),
                              sev_ageph_evtree_nodes$ageph[which(sev_ageph_evtree_nodes$change ==1)],
                              max(mtpl_sev$ageph)))


mtpl_sev <- mtpl_sev %>%
            mutate(ageph_class_s = 
                    cut(ageph,
                         breaks = sev_ageph_splits,
                         right = F,
                         include.lowest = T))

##----specifying and calibrating the final glm for claim severity using only factor variables-------------------------------------

set.seed(1234)
mtpl_sev_split <- initial_split(mtpl_sev, prop = 0.75, strata = lnsev)
mtpl_sev_training <- training(mtpl_sev_split)
mtpl_sev_test <- testing(mtpl_sev_split)

sev_glm_classic <- glm(lnsev ~ agecar + coverp + split + ageph_class_s,
                       weights = nbrtotc,
                       family = gaussian(),
                       data = mtpl_sev_training)

sev_formula_final <- sev_glm_classic$formula
