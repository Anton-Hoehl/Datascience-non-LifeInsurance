
# function from github: https://github.com/henckr/treeML/blob/master/treeML.Rmd

# Poisson deviance
dev_poiss <- function(ytrue, yhat) {
  -2 * mean(dpois(ytrue, yhat, log = TRUE) - dpois(ytrue, ytrue, log = TRUE), na.rm = TRUE)
}

##----data_import-----------------------------------------------------------------------

set.seed(1234)

#summary(mtpl)

##----data_resampling------------------------------------------------------------------

str(mtpl)

mtpl_n <- mtpl %>% dplyr::select(-duree, -ins, -nbrtotan, -chargtot)

mtpl_split <- initial_split(mtpl_n, prop = 0.75, strata = nbrtotc)
mtpl_training <- training(mtpl_split)
mtpl_test <- testing(mtpl_split)


# do the folds manually
# 6 because it as denominator of nrows(mtpl)
mfolds <- function(dataset, K = 6) {
  
  models <- c()
  insample <- c()
  outofsample <- c()
  pred_ins <- c()
  pred_oos <- c()
  
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
    
    # fit the model
    X <- data.matrix(dataset[idx1,] %>% dplyr::select(-nbrtotc))
    y <- data.matrix(dataset$nbrtotc[idx1])
    
    m1_xgb <-
      xgboost::xgboost(
        data =  X,
        label = y,
        nrounds = 1000,
        objective = "count:poisson",
        eval_metric = "poisson-nloglik",
        early_stopping_rounds = 3,
        max_depth = 2,
        eta = .15,
        verbose = 0
      )
    
    paste("Fold Number: ", k, sep = "")
    
    print(varImp(m1_xgb,scale=FALSE))
    
    models <- c(m1_xgb, m1_xgb)
    
    # in sample performance metrics
    Yhat <- as.numeric(stats::predict(m1_xgb, new = X))
    pred_ins <- c(pred_ins, Yhat)
    
    insample <- c(insample, dev_poiss(dataset[idx1,]$nbrtotc, Yhat))
    print(dev_poiss(dataset[idx1,]$nbrtotc, Yhat))
    
    # out of sample performance metrics
    X_test <-
      data.matrix(dataset[idx2,] %>% dplyr::select(-nbrtotc))
    y_test <- data.matrix(dataset$nbrtotc[idx2])
    
    Yhat_test <- as.numeric(stats::predict(m1_xgb, new = X_test))
    pred_oos <- c(pred_oos, Yhat_test)
    
    outofsample <- c(outofsample, dev_poiss(dataset[idx2,]$nbrtotc, Yhat_test))
    print(dev_poiss(dataset[idx2,]$nbrtotc, Yhat_test))

  }
  #final <- data.frame(models, insample, outofsample)
  #colnames(final) <- c("model", "deviance_insample", "deviance_outsample")
  return(list(data.frame(insample, outofsample),
              data.frame(pred_ins, pred_oos)))
}

test <- mfolds(dataset = mtpl_training)

ggplot(data = test[[1]]) + 
    geom_line(aes(x = as.numeric(row.names(test[[1]])), y = insample, col = "red")) +
    geom_line(aes(x = as.numeric(row.names(test[[1]])), y = outofsample, col = "green")) +
    scale_colour_manual(name = "Data",
                        labels = c("Out of Sample", "In Sample"), 
                        values = c("red", "green")) + 
    scale_x_continuous(labels=as.character(as.numeric(row.names(test[[1]]))),
                       breaks=as.numeric(row.names(test[[1]]))) +
    labs(x = "Fold Number", y = "Poisson Deviance", 
         title = "6-Fold XGBoost: Claim Frequency", subtitle = "excluding variable community") +
    theme_bw()

