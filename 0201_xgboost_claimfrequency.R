
# xgb frequency procedure 

mtpl_tr <- xgb.DMatrix(data = mtpl %>% 
                         select(-c(sev, duree,nbrtotc, lnexpo, nbrtotan, chargtot, idph)) %>%
                         data.matrix,
                       info = list(
                         'label' = mtpl$nbrtotc,
                         'base_margin' = mtpl$lnexpo))


m1_xgb <-
  xgboost::xgboost(
    data =  mtpl_tr,
    nrounds = 500,
    objective = "count:poisson",
    eval_metric = "poisson-nloglik",
    early_stopping_rounds = 3,
    max_depth = 2,
    eta = .15,
    verbose = 0
  )

m1_pl <- xgb.ggplot.importance(
  xgb.importance(model = m1_xgb)) +
  theme(legend.position="none") + 
  ggtitle("Variable Importance Frequency")

  

# xgb severity procedure 

pl_tr <- xgb.DMatrix(data =  mtpl_sev_training %>% 
                       select(-c(lnsev, sev, lnexpo, chargtot, nbrtotan, duree,
                                 idph, nbrtotc)) %>% 
                         data.matrix,
                       info = list(
                         'label' = mtpl_sev_training$lnsev))

m2_xgb <-
  xgboost::xgboost(
    data =  pl_tr,
    nrounds = 500,
    objective = "reg:squarederror",
    weight = mtpl_sev_training$nbrtotc,
    early_stopping_rounds = 3,
    max_depth = 2,
    eta = .15,
    verbose = 0
  )



m2_pl <- xgb.ggplot.importance(
  xgb.importance(model = m2_xgb))  +
  theme(legend.position="none") + 
  ggtitle("Variable Importance Severity")





