source('00_dataprep_package_loading.R')

mfolds <- function(dataset_freq, dataset_sev, K = 6) {
  
  fold_name_freq <- vector("integer", K)
  performance_glm_freq <-vector("integer", K)
  performance_xgb_freq <-vector("integer", K)
  ins_glm_freq <-vector("integer", K)
  ins_xgb_freq <-vector("integer", K)
  
  # enumerate the rows and sample 1/K parts
  idx <- seq(1, nrow(dataset_freq))
  
  # move through the rows such that every entry is only used once
  idx_k <- sample(idx, 1 / K * nrow(dataset_freq))
  idx <- setdiff(idx, idx_k)
  
  for (k in seq(2, K)) {
    idx_k <- cbind(idx_k, sample(idx, 1 / K * nrow(dataset_freq)))
    idx <- setdiff(idx, idx_k[, k])
  }
  
  # use then the idx_k object to get different test and train sets
  for (k in seq(1, K)) {
    # for debugging and progress demonstration
    print(paste("Fold Number: ", k, sep = ""))
    
    # idx1 train and idx2 test
    idx2 <- idx_k[, k]
    idx1 <- setdiff(seq(1, nrow(dataset_freq)), idx2)
    
    # glm frequency procedure
    #===========================================================================
    freq_glm_classic <- glm(nbrtotc ~ fuelc + split + coverp + 
                              powerc + agecar + ageph_class + geo,
                            offset = lnexpo,
                            family = poisson(link = "log"),
                            data = dataset_freq[idx1, ])

    yhat_test <- exp(stats::predict(freq_glm_classic, newdata = dataset_freq[idx2, ]))
    ytrue_test <- dataset_freq[idx2, ]$nbrtotc
    name <- paste("Fold Number: ", k, sep = "")
    fold_name_freq[k] <- k
    perf <- dev_poiss(ytrue_test, yhat_test)
    print(perf)
    performance_glm_freq[k] <- perf
    
    yhat_train <- exp(stats::predict(freq_glm_classic, newdata = dataset_freq[idx1, ]))
    ytrue_train <- dataset_freq[idx1, ]$nbrtotc
    
    ins_glm_freq[k] <- dev_poiss(ytrue_train, yhat_train)
    
    # xgboost frequency procedure
    #===========================================================================
    
    mtpl_tr <- xgb.DMatrix(data = dataset_freq[idx1, ] %>% 
                             select(fuelc, split, coverp, powerc, agecar, 
                                    ageph_class, geo) %>%
                             data.matrix,
                           info = list(
                             'label' = dataset_freq[idx1, ]$nbrtotc,
                             'base_margin' = dataset_freq[idx1, ]$lnexpo))
    
    mtpl_ts <- xgb.DMatrix(data = dataset_freq[idx2, ] %>% 
                             select(fuelc, split, coverp, powerc, agecar, 
                                    ageph_class, geo) %>%
                             data.matrix,
                           info = list(
                             'label' = dataset_freq[idx2, ]$nbrtotc,
                             'base_margin' = dataset_freq[idx2, ]$lnexpo))
    
    m1_xgb <-
      xgboost::xgboost(
        data =  mtpl_tr,
        nrounds = 100,
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
    performance_xgb_freq[k] <- perf
    
    yhat_train <- m1_xgb %>% predict(newdata = mtpl_tr)
    ins_xgb_freq[k] <- dev_poiss(ytrue_train, yhat_train)
    
  }
  
  # glm severity model
  #=============================================================================
  fold_name_sev <- vector("integer", K)
  performance_glm_sev <-vector("integer", K)
  performance_xgb_sev <-vector("integer", K)
  ins_glm_sev <-vector("integer", K)
  ins_xgb_sev <-vector("integer", K)
  
  # enumerate the rows and sample 1/K parts
  idx <- seq(1, nrow(dataset_sev))
  
  # move through the rows such that every entry is only used once
  idx_k <- sample(idx, 1 / K * nrow(dataset_sev))
  idx <- setdiff(idx, idx_k)
  
  for (k in seq(2, K)) {
    idx_k <- cbind(idx_k, sample(idx, 1 / K * nrow(dataset_sev)))
    idx <- setdiff(idx, idx_k[, k])
  }
  
  # use then the idx_k object to get different test and train sets
  for (k in seq(1, K)) {
    # for debugging and progress demonstration
    print(paste("Fold Number: ", k, sep = ""))
    fold_name_sev[k] <- k
    
    # idx1 train and idx2 test
    idx2 <- idx_k[, k]
    idx1 <- setdiff(seq(1, nrow(dataset_sev)), idx2)
    
    
    sev_glm_classic <- glm(lnsev ~ agecar + coverp + split + ageph_class,
                           weights = nbrtotc,
                           family = gaussian(),
                           data = dataset_sev[idx1, ])
    
    yhat_test <- exp(stats::predict(sev_glm_classic, newdata = dataset_sev[idx2, ]))
    ytrue_test <- dataset_sev[idx2, ]$nbrtotc
    name <- paste("Fold Number: ", k, sep = "")

    perf <- RMSE(ytrue_test, yhat_test)
    print(perf)
    performance_glm_sev[k] <- perf
    
    yhat_train <- exp(stats::predict(sev_glm_classic, newdata = dataset_sev[idx1, ]))
    ytrue_train <- dataset_sev[idx1, ]$lnsev
    
    ins_glm_sev[k] <- RMSE(ytrue_train, yhat_train)
    
    # xgboost severity model
    #===========================================================================
    mtpl_tr <- xgb.DMatrix(data = dataset_sev[idx1, ] %>% 
                             select(agecar, coverp, split, ageph_class) %>%
                             data.matrix,
                           info = list(
                             'label' = dataset_sev[idx1, ]$lnsev))
    
    mtpl_ts <- xgb.DMatrix(data = dataset_freq[idx2, ] %>% 
                             select(agecar, coverp, split, ageph_class) %>%
                             data.matrix,
                           info = list(
                             'label' = dataset_freq[idx2, ]$lnsev))
    
    m1_xgb <-
      xgboost::xgboost(
        data =  mtpl_tr,
        nrounds = 100,
        objective = "reg:squarederror",
        weight = dataset_sev[idx1, ]$nbrtotc,
        early_stopping_rounds = 3,
        max_depth = 2,
        eta = .15,
        verbose = 0
      )
    
    yhat_test <- m1_xgb %>% predict(newdata = mtpl_ts)
    perf <- RMSE(ytrue_test, yhat_test)
    print(perf)
    performance_xgb_sev[k] <- perf
    
    yhat_train <- m1_xgb %>% predict(newdata = mtpl_tr)
    ins_xgb_sev[k] <- RMSE(ytrue_train, yhat_train)
    
  }

  return(data.frame(fold_name_freq, performance_glm_freq, ins_glm_freq, 
                    performance_xgb_freq, ins_xgb_freq, fold_name_sev,
                    performance_glm_sev, performance_xgb_sev, 
                    ins_glm_sev, ins_xgb_sev))
}


# actual step of model calibration 
results <- mfolds(dataset_freq = mtpl_n_training, dataset_sev = mtpl_n_training) %>% 
              pivot_longer(!c(fold_name_freq, fold_name_sev), 
                           names_to = "Model", 
                           values_to = "Performance")

# out of sample
oos <- results %>% filter(Model == "performance_glm_freq" | Model == "performance_xgb_freq")

# plotting of the poisson deviance 
fpl <- ggplot(data = oos, aes(x = fold_name_freq, y = Performance, 
                             col = Model, group = Model)) + 
          scale_color_manual(values= c("performance_glm_freq"="#f9b300" ,
                                       "performance_xgb_freq"="#1f973d")) +
          geom_line() +
          labs(x = "Fold number:", y = "Poisson Deviance", 
               title = "6-Fold Claim Frequency Cross Validation") +
          theme_bw()

plot(fpl)

# This approach needs to be replicated for the Severity
#===========================================================================

# out of sample
oos <- results %>% filter(Model == "performance_glm_sev" | Model == "performance_xgb_sev")

fpl <- ggplot(data = oos, aes(x = fold_name_sev, y = Performance, 
                              col = Model, group = Model)) + 
  scale_color_manual(values= c("performance_glm_sev"="#f9b300" ,
                               "performance_xgb_sev"="#1f973d")) +
  geom_line() +
  labs(x = "Fold number:", y = "RMSE", 
       title = "6-Fold Severity Cross Validation") +
  theme_bw()

plot(fpl)



