
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
                                                          newdata = mtpl_tariff)))
                         ) 

meaned <- as.data.frame(lapply(mtpl_tariff %>% select(c(freq_pred, sev_pred)), scale))



# function to compute total within-cluster sum of square
wss <- function(df, k, nstart=25) {
  kmeans(df, k, nstart = nstart)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 10
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


mtpl_tariff <- mtpl_tariff %>% 
                  replace_na(list(sev = 0))



cluster_params <- mtpl_tariff %>% 
                    select(sev_pred, freq_pred, cluster) %>% 
                    group_by(cluster) %>% 
                    summarise(mean_sev = mean(sev_pred),
                              mean_freq = mean(freq_pred))

