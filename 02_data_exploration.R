##----call package loading source-------------------------------------
source("00_package_loading.R")
##---------------------------------------------------------------------------
sum(is.na(mtpl))

min(mtpl$ageph)
max(mtpl$ageph)

mtpl <- mtpl %>%
  mutate(sev = ifelse(nbrtotc == 0, NA, chargtot / nbrtotc))   #average claim amount (severity) calculation


##----data_viz_prep--------------------------------------------------------------
pantone <- "#D0417E"
col <- pantone
fill <- pantone
ylab <- "Relative frequency"

gg.bar <- function(data, variable) {
  ggplot(data = data, aes_string(x = variable)) +
    theme_bw() +
    geom_bar(aes(y = (..count..) / sum(..count..)), 
             col = col, fill = fill, alpha = 0.5) +
    labs(x = variable, y = ylab)
}

gg.hist <- function(data, variable, binwidth = 30) {
  ggplot(data = data, aes_string(x = variable)) +
    theme_bw() +
    geom_histogram(aes(y = (..count..) / sum(..count..)),
                   col = col, binwidth = binwidth, fill = fill, alpha = 0.5) +
    labs(x = variable, y = ylab)
}

gg.dens <- function(data, variable) {
  ggplot(data = data, aes_string(x = variable)) +
    theme_bw() +
    geom_density(col = col, fill = fill, alpha = 0.5) +
    xlab(variable) +
    ylab("density")
}

##----bar_plots--------------------------------------------------------------
char_cols <- mtpl %>% select_if(is.factor) %>% names()
g_bars <- lapply(char_cols, gg.bar, data = mtpl)

##----hist_plots-------------------------------------------------------------------------------
g_duree <- gg.hist(mtpl, "duree", binwidth = 0.05)
g_nbrtotc <- gg.hist(mtpl, "nbrtotc", binwidth = 1)
g_nbrtotan <- gg.hist(mtpl %>% filter(nbrtotan < 20), "nbrtotan", binwidth = 3)
g_ageph <- gg.hist(mtpl, "ageph", binwidth = 2)
g_hists <- list(g_duree, g_nbrtotan, g_nbrtotc, g_ageph)

##----dens_plots-------------------------------------------------------------------------------
 
g_sev <- gg.dens(mtpl %>% filter(nbrtotc > 0), "sev") + xlim(0,10000)    #we can see the severity distribution is very skewed
g_sevs <- list(g_sev)

#cutoff <- quantile(mtpl$sev, probs = 0.999, na.rm = T)

#mtpl <- mtpl %>%
        #filter(is.na(sev) | (sev < cutoff))   #we excluded claim severities higher than 82153.51 (99.9 percentile)

##----data_viz------------------------------------------------------------------------------
g_list <- c(g_hists,g_sevs,g_bars)
#grid.arrange(grobs = g_list)

g_ageph_nbrtotc <-  mtpl %>% 
                    group_by(ageph) %>% 
                    summarize(n_claims = sum(nbrtotc))  %>%
                    mutate(claim_freq = n_claims / sum(n_claims)) %>%
                    ggplot(aes(x = ageph, y = claim_freq)) +
                    geom_point(col = col) +
                    theme_bw()

g_ageph_sevfreq <-  mtpl %>% 
                    filter(nbrtotc > 0) %>%
                    group_by(ageph) %>% 
                    summarize(sev_agg = sum(sev))  %>%
                    ggplot(aes(x = ageph, y = sev_agg)) +
                    geom_point(col = col) +
                    theme_bw()

g_ageph_logsev <- mtpl %>%
               filter(nbrtotc > 0, sev <= 60000) %>%
               ggplot(aes(x = ageph, y = log(sev))) +
               geom_point(col = col) +
               theme_bw()


g_codposs_nbrtotc <- mtpl %>% 
                     group_by(codposs) %>% 
                     summarize(n_claims = sum(nbrtotc))  %>%
                     mutate(claim_freq = n_claims / sum(n_claims)) %>%
                     ggplot(aes(x = codposs, y = claim_freq)) +
                     geom_point(col = col) +
                     theme_bw()

##----distibutional asumptions for claim frequency-----------------------------------

mean(mtpl$nbrtotc) #we assume the number of claims to follow a poisson distribution
var(mtpl$nbrtotc)

##----distributional asumptions for claim severity-----------------------------------------------------------------

mtpl_sev <- mtpl %>% filter(sev != is.na(sev)) 

set.seed(456)
gamma_fit <- fitdist(mtpl_sev$sev, distr = "gamma", method = "mle", lower = c(0,0))
lnorm_fit <- fitdist(mtpl_sev$sev, distr = "lnorm", method = "mle", lower = c(0,0))

g_gamma_vs_lnorm <- 
  gg.dens(mtpl_sev, "sev") +
  xlim(0,5000) +
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

mtpl_sev <- mtpl_sev %>%
            mutate(lnsev = log(sev))


##----spatial_data_prep----------------------------------------------------------------------
be_shape_sf <- st_read("./shape file Belgie postcodes/npc96_region_Project1.shp", quiet = T)
be_shape_sf <- st_transform(be_shape_sf, "+proj=longlat", "+datum=WGS84")

codposs_expo <- mtpl %>% 
                group_by(codposs) %>% 
                summarize(numph = n(), expo = sum(duree))

be_shape_sf <- be_shape_sf %>%
               left_join(codposs_expo,
                         by = c("POSTCODE" = "codposs"))

be_shape_sf <- be_shape_sf %>%
               mutate(expo_pau = expo / Shape_Area)

be_shape_sf <- be_shape_sf %>%
               mutate(expo_abin = cut(be_shape_sf$expo_pau, 
                                      breaks = quantile(be_shape_sf$expo_pau, c(0,0.2,0.8,1), na.rm = T),
                                      right = F, include.lowest = T,
                                      labels = c("low","medium","high")))


##----spatial_data_viz-----------------------------------------------------------------------

be_expo_reg <- ggplot(be_shape_sf) +
  geom_sf(aes(fill = expo_abin)) +
  ggtitle("Belgium - relative exposure per region") +
  labs(fill = "relative exposure") +
  scale_fill_brewer(palette = "PuRd",
                    na.value = "White") +
  theme_bw()
  
##----relative_frequency_plots-----------------------------------------------------------------------

# dataset for explanatory stuff
my_ex <- mtpl %>% 
  select(c(nbrtotc, split, coverp, powerc)) %>% 
  filter(nbrtotc >= 1)


# barplot for claim frequency in powerc classes 
geogroup <- my_ex %>% 
  select(c(nbrtotc, powerc)) %>% 
  group_by(powerc, nbrtotc) %>% 
  summarise(count = n())

powerpl <- ggplot(geogroup, aes(fill=nbrtotc, y=count, x=powerc)) + 
  geom_bar(position="fill", stat="identity")


# barplot for claim frequency in coverp classes 
geogroup <- my_ex %>% 
  select(c(nbrtotc, coverp)) %>% 
  group_by(coverp, nbrtotc) %>% 
  summarise(count = n())

coverpl <- ggplot(geogroup, aes(fill=nbrtotc, y=count, x=coverp)) + 
  geom_bar(position="fill", stat="identity")
  


# barplot for claim frequency in split classes 
geogroup <- my_ex %>% 
  select(c(nbrtotc, split)) %>% 
  group_by(split, nbrtotc) %>% 
  summarise(count = n())

splitpl <- ggplot(geogroup, aes(fill=nbrtotc, y=count, x=split)) + 
  geom_bar(position="fill", stat="identity")


