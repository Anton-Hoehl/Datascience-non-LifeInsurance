source('00_dataprep_package_loading.R')
source('01_data_exploration.R')
source('0203_gam_claimfrequency.R')


mfolds <- function(dataset, K = 6) {
  
  fold_name <- vector("integer", K)
  performance_glm <-vector("integer", K)
  performance_xgb <-vector("integer", K)
  ins_glm <-vector("integer", K)
  ins_xgb <-vector("integer", K)
  
  # enumerate the rows and sample 1/K parts
  idx <- seq(1, nrow(dataset))
  
  # move through the rows such that every entry is only used once
  idx_k <- sample(idx, 1 / K * nrow(dataset))
  idx <- setdiff(idx, idx_k)
  
  for (k in seq(2, K)) {
    idx_k <- cbind(idx_k, sample(idx, 1 / K * nrow(dataset)))
    idx <- setdiff(idx, idx_k[, k])
  }
  
  # use then the idx_k object to get different test and train sets
  for (k in seq(1, K)) {
    # for debugging and progress demonstration
    print(paste("Fold Number: ", k, sep = ""))
    
    # idx1 train and idx2 test
    idx2 <- idx_k[, k]
    idx1 <- setdiff(seq(1, nrow(dataset)), idx2)
    
    #===========================================================================
    freq_glm_classic <- glm(nbrtotc ~ fuelc + split + coverp + 
                              powerc + agecar + ageph_class + geo,
                            offset = lnexpo,
                            family = poisson(link = "log"),
                            data = dataset[idx1, ])

    yhat_test <- exp(stats::predict(freq_glm_classic, newdata = dataset[idx2, ]))
    ytrue_test <- dataset[idx2, ]$nbrtotc
    name <- paste("Fold Number: ", k, sep = "")
    fold_name[k] <- k
    perf <- dev_poiss(ytrue_test, yhat_test)
    print(perf)
    performance_glm[k] <- perf
    
    yhat_train <- exp(stats::predict(freq_glm_classic, newdata = dataset[idx1, ]))
    ytrue_train <- dataset[idx1, ]$nbrtotc
    
    ins_glm[k] <- dev_poiss(ytrue_train, yhat_train)
    
    #===========================================================================
    
    mtpl_tr <- xgb.DMatrix(data = dataset[idx1, ] %>% 
                             select(ageph, split, usec, agecar, coverp, powerc, 
                                    fuelc, sexp, fleetc, sportc, lat, long) %>%
                             data.matrix,
                           info = list(
                             'label' = dataset[idx1, ]$nbrtotc,
                             'base_margin' = dataset[idx1, ]$lnexpo))
    
    mtpl_ts <- xgb.DMatrix(data = dataset[idx2, ] %>% 
                             select(ageph, split, usec, agecar, coverp, powerc, 
                                    fuelc, sexp, fleetc, sportc, lat, long) %>%
                             data.matrix,
                           info = list(
                             'label' = dataset[idx2, ]$nbrtotc,
                             'base_margin' = dataset[idx2, ]$lnexpo))
    
    m1_xgb <-
      xgboost::xgboost(
        data =  mtpl_tr,
        nrounds = 1000,
        objective = "count:poisson",
        eval_metric = "poisson-nloglik",
        early_stopping_rounds = 3,
        max_depth = 2,
        eta = .15,
        verbose = 0
      )
    yhat_test <- m1_xgb %>% predict(newdata = mtpl_ts)
    perf <- dev_poiss(ytrue_test, yhat_test)
    print(perf)
    performance_xgb[k] <- perf
    
    yhat_train <- m1_xgb %>% predict(newdata = mtpl_tr)
    ins_xgb[k] <- dev_poiss(ytrue_train, yhat_train)
    #===========================================================================
    
  }

  return(data.frame(fold_name, performance_glm, ins_glm, performance_xgb, ins_xgb))
}

results <- mfolds(dataset = mtpl_n_training) %>% 
              pivot_longer(!fold_name, 
                           names_to = "Model", 
                           values_to = "Performance")
# out of sample
oos <- results %>% filter(Model == "performance_glm" | Model == "performance_xgb")

# plotting of the poisson deviance 
fpl <- ggplot(data = oos, aes(x = fold_name, y = Performance, 
                             col = Model, group = Model)) + 
          scale_color_manual(values= c("performance_glm"="#f9b300" ,
                                       "performance_xgb"="#1f973d")) +
          geom_line() +
          labs(x = "Fold number:", y = "Poisson Deviance", 
               title = "6-Fold Model Cross Validation") +
          theme_bw()

plot(fpl)


