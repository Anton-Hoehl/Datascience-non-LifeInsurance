#----run source codes to get input data for tarrif structure build---------------------------------------------------------------
source("0203_gam_claimfrequency.R")
source("0203_gam_severity_v2.R")
source("model_comp.R")
source("0201_xgboost_claimfrequency.R")

# add the ageph_class_s groups

source("00_package_loading.R")

mtpl_tariff <- mtpl_final %>%
                  mutate(ageph_class_s = 
                           cut(ageph,
                               breaks = sev_ageph_splits,
                               right = F,
                               include.lowest = T))


# calculate fitted values 

sigma <- get_sigma(sev_glm_classic)


mtpl_tariff <- mtpl_tariff %>% 
                  mutate(freq_pred = exp(unname(stats::predict(freq_glm_classic, 
                                                               newdata = mtpl_tariff))),
                         sev_pred = exp(unname(stats::predict(sev_glm_classic, 
                                                          newdata = mtpl_tariff)) + (sigma^2)/2))

meaned <- as.data.frame(lapply(mtpl_tariff %>% select(c(freq_pred, sev_pred)), scale))



# function to compute total within-cluster sum of square
wss <- function(df, k, nstart=25) {
  kmeans(df, k, nstart = nstart)$tot.withinss
}

# Compute and plot wss for k = 2 to k = 20
k_values <- 2:20
set.seed(42)
# extract wss for 2-20 clusters
wss_values <- vapply(k_values, wss, df = meaned, FUN.VALUE = numeric(1))


ggplot(data.frame(k_values, wss_values), aes(k_values, wss_values)) +
  geom_line() + geom_point() +
  theme_bw() +
  labs(title="Optimal number of clusters",
       x = "Number of clusters K",
       y = "Total within-clusters sum of squares")



apllied_means <- kmeans(meaned, 15, nstart = 25)

clusterplot <- fviz_cluster(apllied_means, data = meaned,
             geom = "point",
             shape = ".",
             pointsize = 0.1,
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


mtpl_tariff$cluster <- apllied_means$cluster

clustering <- mtpl_tariff %>% 
  mutate(Tariff_Name = paste("Tariff ", cluster)) %>% 
  mutate(pure_premium = sev_pred * freq_pred) %>% 
  select(c(Tariff_Name, pure_premium, cluster, chargtot, freq_pred, sev_pred)) %>%
  group_by(cluster, Tariff_Name) %>% 
  summarise(num_ph = n(),
            avg_freq_pred = mean(freq_pred),
            avg_lnsev_pred = mean(log(sev_pred)),
            Pure_Premium_ind = mean(pure_premium),
            claims_obs = sum(chargtot),
            Pure_Premium_tot = sum(pure_premium),
            diff = sum(pure_premium) - sum(chargtot))
            



ggplot(mtpl_tariff %>% mutate(Pure_Premium = sev_pred * freq_pred), aes(x = Pure_Premium)) + 
  facet_wrap(.~ cluster, ncol=4) +
  theme_bw() +
  geom_histogram(binwidth = 5, fill =  "steelblue")


summary(clustering$Pure_Premium_ind)

# ==============================================================================


ggplot(clustering, aes(fill=nbrtotc, y=count, x=cluster)) + 
  geom_bar(position="fill", stat="identity")



cluster_params <- mtpl_tariff %>% 
                    select(sev_pred, freq_pred, cluster) %>% 
                    group_by(cluster) %>% 
                    summarise(mean_sev = mean(sev_pred),
                              mean_freq = mean(freq_pred))


#----risk premium calculation for tariff groups----------------------------------
#the standard deviation approach
riskP <- function(PP,num_ph,lambda,mu,sigma,theta) {

sigma_n <- sigma / sqrt(num_ph)
EX <- exp(mu + (sigma_n^2)/2)
DX <- (exp(sigma_n^2)-1) * exp(2*mu + sigma_n^2)
DS <- lambda*DX+lambda*(EX^2)
RP <- PP + theta*sqrt(DS)
return(RP)

}

riskP <- Vectorize(riskP, vectorize.args = c("PP","num_ph","lambda","mu","theta"))

#----final tariff tables for different theta values--------------------------------------
tariff_structure <- list()

for (i in 1:3) {

theta <- 0.02*i
  
tariff_structure[[i]] <- tibble("Tariff_Name" = clustering$Tariff_Name) %>%
                           mutate(PP = clustering$Pure_Premium_ind,
                                  RP_phi1 = riskP(
                                    clustering$Pure_Premium_ind,
                                    clustering$num_ph,
                                    clustering$avg_freq_pred,
                                    clustering$avg_lnsev_pred,
                                    sigma,
                                    theta = theta)) %>%
                          mutate(RP_phi1_tot = RP_phi1*clustering$num_ph,
                                 claims_obs = clustering$claims_obs,
                                 diff = RP_phi1_tot - claims_obs)

}

