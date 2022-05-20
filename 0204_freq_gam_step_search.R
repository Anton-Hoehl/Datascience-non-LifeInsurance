source("01_dataprep.R")

##----step gam search for frequency-------------------------------------------------------------

freq_scope_init <- gam.scope(mtpl_training,
                             response = mtpl_training$nbrtotc,
                             smoother = "s")

freq_scope_init <- within(freq_scope_init, rm(lat,long,sev,chargtot,nbrtotan,idph))

freq_scope_add <- list("agecar" = ~ 1 + agecar,
                       "ageph" = ~ 1 + ageph + s(ageph, k = 6),
                       "latlong" = ~ 1 + s(long,lat, k = 25))


freq_scope <- c(freq_scope_init,freq_scope_add)


gam_freq_lower <- mgcv::gam(formula = nbrtotc ~ 1,
                            family = poisson(link = "log"),
                            offset = lnexpo,
                            data = mtpl_training,
                            method = "REML")

step_gam_freq <- step.Gam(gam_freq_lower,
                          scope = freq_scope,
                          direction = "both")

step_gam_freq$formula