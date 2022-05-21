
# add the ageph_class_s groups
mtpl_tariff <- mtpl_final %>%
                  mutate(ageph_class_s = 
                           cut(ageph,
                               breaks = sev_ageph_splits,
                               right = F,
                               include.lowest = T))


# calculate fitted values 
mtpl_tariff <- mtpl_tariff %>% 
                  mutate(freq_pred = exp(unname(stats::predict(freq_glm_classic, 
                                                               newdata = mtpl_tariff))),
                         sev_pred = exp(unname(stats::predict(sev_glm_classic, 
                                                          newdata = mtpl_tariff)) + ((get_sigma(sev_glm_classic))^2)/2
                         )) 

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



mtpl_tariff$cluster <- apllied_means$cluster

clustering <- mtpl_tariff %>% 
  mutate(Tariff_Name = paste("Tariff ", cluster)) %>% 
  mutate(pure_premium = sev_pred * freq_pred) %>% 
  select(c(Tariff_Name, pure_premium, cluster, chargtot)) %>%
  group_by(cluster, Tariff_Name) %>% 
  summarise(count = n(),
            Pure_Premium = mean(pure_premium),
            charggrp = sum(chargtot),
            p70_rp = stats::quantile(pure_premium, probs = 0.7),
            p70_tot = count * p70_rp, 
            diff_70 = p70_tot - charggrp,
            p80_rp = stats::quantile(pure_premium, probs = 0.8),
            p80_tot = n() * p80_rp, 
            diff_80 = p80_tot - charggrp,
            p90_rp = stats::quantile(pure_premium, probs = 0.9),
            p90_tot = n() * p90_rp, 
            diff_90 = p90_tot - charggrp,
            p95_rp = stats::quantile(pure_premium, probs = 0.95),
            p95_tot = n() * p95_rp,
            diff_95 = p95_tot - charggrp,
            p99_rp = stats::quantile(pure_premium, probs = 0.99),
            p99_tot = n() * p99_rp,
            diff_99 = p99_tot - charggrp)



# total 
  
quantile(wts$pure_premium, probs = 0.99)

163647 * 124.5689 - sum(wts$chargtot)


cluster_summary <- clustering %>% 
  group_by(cluster) %>% 
  summarise(n = n()) %>% 
  mutate(lab = paste("N = ", n))


ggplot(mtpl_tariff %>% mutate(Pure_Premium = sev_pred * freq_pred), aes(x = Pure_Premium)) + 
  facet_wrap(.~ cluster, ncol=4) +
  theme_bw() +
  geom_histogram(binwidth = 5, fill =  "steelblue")



ggplot(clustering, aes(x = Pure_Premium)) + 
  geom_histogram(binwidth = 5,)


ggplot(clustering, aes(x = pure_premium)) + geom_histogram(binwidth = 5)

summary(clustering$pure_premium)

# ==============================================================================


clustering <- mtpl_tariff %>% 
  select(c(nbrtotc, cluster)) %>% 
  filter(nbrtotc >= 1) %>%
  group_by(cluster, nbrtotc) %>% 
  summarise(count = n())



ggplot(clustering, aes(fill=nbrtotc, y=count, x=cluster)) + 
  geom_bar(position="fill", stat="identity")


clustering <- mtpl_tariff %>% 
  mutate(pure_premium = sev_pred * freq_pred) %>% 
  select(c(pure_premium, cluster)) %>% 
  group_by(cluster) %>% 
  dplyr::summarise(avg = mean(pure_premium))












cluster_params <- mtpl_tariff %>% 
                    select(sev_pred, freq_pred, cluster) %>% 
                    group_by(cluster) %>% 
                    summarise(mean_sev = mean(sev_pred),
                              mean_freq = mean(freq_pred))

