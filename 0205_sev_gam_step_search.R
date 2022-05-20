source("01_dataprep.R")

##----step gam search for log(severity)-------------------------------------------------------------


sev_scope_init <- gam.scope(mtpl_sev_training,
                            response = mtpl_sev_training$nbrtotc,
                            smoother = "s")

sev_scope_init <- within(sev_scope_init, rm(lat,long,lnsev,sev,nbrtotan,chargtot,idph))

sev_scope_add <- list("agecar" = ~ 1 + agecar,
                      "ageph" = ~ 1 + ageph + s(ageph, k = 6))


sev_scope <- c(sev_scope_init,sev_scope_add)


gam_sev_lower <- mgcv::gam(formula = lnsev ~ 1,
                           family = gaussian(),
                           weights = nbrtotc,
                           data = mtpl_sev_training,
                           method = "REML")

step_gam_sev <- step.Gam(gam_sev_lower,
                         scope = sev_scope,
                         direction = "both")

step_gam_sev$formula