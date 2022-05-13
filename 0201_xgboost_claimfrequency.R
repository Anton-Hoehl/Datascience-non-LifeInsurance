
packages <- c("data.table",
              "dplyr",
              "ggplot2",
              "caret",
              "xgboost",
              "e1071",
              "cowplot",
              "Matrix",
              "magrittr")
suppressMessages(packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}))
##----data-----------------------------------------------------------------------

mtpl <- read_delim(file = "Assignment.csv",
                   delim = ",",
                   col_names = T)

inspost <- read_xls("inspost.xls",
                    sheet = "inspost",
                    col_names = T)

mtpl <- mtpl %>% left_join(inspost, by = "CODPOSS")

dplyr::select()

mtpl <- mtpl %>%
  dplyr::select(-COMMUNE) %>%
  rename(
    "ageph" = AGEPH,
    "codposs" = CODPOSS,
    "ins" = INS,
    "lat" = LAT,
    "long" = LONG
  ) %>%
  mutate_if(is.character, as.factor)

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
        objective = "reg:squarederror",
        early_stopping_rounds = 3,
        max_depth = 3,
        eta = .15,
        verbose = 0
      )
    
    # in sample performance metrics
    Yhat <- as.numeric(stats::predict(m1_xgb, new = X))
    
    print(paste(
      "In Sample RMSE: ", 
      RMSE(dataset[idx1,]$nbrtotc, Yhat), 
      sep = ""))
    
    # out of sample performance metrics
    X_test <-
      data.matrix(dataset[idx2,] %>% dplyr::select(-nbrtotc))
    y_test <- data.matrix(dataset$nbrtotc[idx2])
    
    Yhat_test <- as.numeric(stats::predict(m1_xgb, new = X_test))
    
    print(paste(
      "Out of Sample RMSE: ",
      RMSE(dataset[idx1,]$nbrtotc, Yhat_test),
      sep = ""
    ))
  }
  
}

mfolds(dataset = mtpl_training)
