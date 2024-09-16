#### Methods for prediction functions ####

# Elasticnet --------------------------------------------------------------

method.elasticnet <- function(data_train, y_train, data_test, folds_idxs, betareg = TRUE, betatree = TRUE){
  y_train <- y_train + 1e-06 # ensure that is not equal to zero
  # Select hyperparameters
  hyperparam <- kfoldCV.elastic(data_train, y_train, folds_idxs)
  # Fit model
  elastic.fit <- glmnet::glmnet(x = data_train, y = y_train, family = "gaussian",
                                alpha = hyperparam$best.alpha,
                                lambda = hyperparam$best.lambda)
  # Predict over test data
  ypred <- glmnet::predict.glmnet(elastic.fit, as.matrix(data_test))
  colnames(ypred) <- "elasticnet"

  if(betareg){
    # retrieve selected variables by elastic net
    coeffs_elastic <- coef(elastic.fit, s = "lambda.min")
    vars_elastic <-  coeffs_elastic@Dimnames[[1]][coeffs_elastic@i +1]
    data_betareg <- data.frame(y_train, data_train[,colnames(data_train) %in% vars_elastic])
    colnames(data_betareg)[1] <- "y"
    datatest <- data_test[ , colnames(data_test) %in% vars_elastic]
    # Fit beta regression model
    beta_elastic.fit <- tryCatch(betareg::betareg(y ~ . , data = data_betareg), error= function(e) {return(NA)}  )
    # Predict over test data
    betareg_pred <- tryCatch(predict(beta_elastic.fit, newdata = datatest), error= function(e) {return(NA)}  )
    betareg_pred <- as.data.frame(betareg_pred)
    colnames(betareg_pred) <- "beta-elastic"
    ypred <- cbind(ypred, betareg_pred)
  }

  if(betatree){
    # retrieve selected variables by elastic net
    coeffs_elastic <- coef(elastic.fit, s = "lambda.min")
    vars_elastic <-  coeffs_elastic@Dimnames[[1]][coeffs_elastic@i +1]
    data_betareg <- data.frame(y_train, data_train[,colnames(data_train) %in% vars_elastic])
    colnames(data_betareg)[1] <- "y"
    datatest <- data_test[ , colnames(data_test) %in% vars_elastic]
    # Fit beta regression model
    beta_tree_elastic.fit <- tryCatch(betareg::betatree(y ~ . , data = data_betareg), error= function(e) {return(NA)}  )
    # Predict over test data
    beta_tree_pred <- tryCatch(predict(beta_tree_elastic.fit, newdata = datatest), error= function(e) {return(NA)}  )
    beta_tree_pred <- as.data.frame(beta_tree_pred)
    colnames(beta_tree_pred) <- "beta-tree-elastic"
    ypred <- cbind(ypred, beta_tree_pred)
  }

  return(ypred)
}


# Betaboost (tree-based) --------------------------------------------------

method.betaboost <- function(data_train, y_train, data_test, nfolds){
  # merge ytrain and predictors
  y_train <- y_train + 1e-06 # ensure that is not equal to zero
  data_betaboost <- data.frame(y_train, data_train)
  colnames(data_betaboost)[1] <- "y"
  # Select hyperparameters
  hyperparam <- kfoldCV.betaboost(data_betaboost, nfolds)
  # Fit model
  betaboost.fit <- mboost::blackboost(y ~ ., data = data_betaboost, family = betaboost::BetaReg(),
                                      control = mboost::boost_control(mstop = 200),
                                      tree_controls = partykit::ctree_control(maxdepth = hyperparam$max_depth.min))

  # Predict over test data
  ypred <- predict(betaboost.fit, newdata = data_test, type = "response")
  colnames(ypred) <- "betaboost"
  return(ypred)
}


# linear-pls --------------------------------------------------------------

method.linearpls <- function(data_train, y_train, data_test, folds_idxs, pls.directions = 30){
  y_train <- y_train + 1e-06 # ensure that is not equal to zero
  # Separate regions from data set
  regions.cols_train <- which(grepl(pattern = "^df.region", colnames(data_train)))
  regions.cols_test <- which(grepl(pattern = "^df.region", colnames(data_test)))
  Rtrain <- data_train[,regions.cols_train]
  Xtrain <- data_train[,-regions.cols_train]
  Rtest <- data_test[,regions.cols_test]
  Xtest <- data_test[,-regions.cols_test]
  # Select hyperparameters
  hyperparam <- kfoldCV.pls(data_train, y_train, folds_idxs, pls.directions)

  # Fit model
    # Estimate projections
  pls.projections <- chemometrics::pls1_nipals(Xtrain, y_train, a = hyperparam$d.min, scale = TRUE)
  datatrain <- as.data.frame(cbind(y_train, as.matrix(Xtrain) %*% pls.projections$W))
  colnames(datatrain)[1] <- "y"
  datatest <- data.frame(as.matrix(Xtest) %*% pls.projections$W)
  colnames(datatest) <- colnames(datatrain)[-1]

  # Train and test data with dimension reduction and dummies of time and region - formula of fit
  datatrain <- data.frame(datatrain, Rtrain)
  datatest <- data.frame(datatest, Rtest)

  # Fit
  pls.fit <- tryCatch(lm(y ~ . , datatrain), error= function(e) {return(NA)}  )

  # Predict over test data
  ypred <- predict(pls.fit, newdata = datatest)
  ypred <- as.data.frame(ypred)
  colnames(ypred) <- "linear-pls"
  return(ypred)
}


# Beta-PLS ----------------------------------------------------------------

method.beta_pls <- function(data_train, y_train, data_test, folds_idxs, pls.directions = 30){
  y_train <- y_train + 1e-06 # ensure that is not equal to zero
  # Separate regions from data set
  regions.cols_train <- which(grepl(pattern = "^df.region", colnames(data_train)))
  regions.cols_test <- which(grepl(pattern = "^df.region", colnames(data_test)))
  Rtrain <- data_train[,regions.cols_train]
  Xtrain <- data_train[,-regions.cols_train]
  Rtest <- data_test[,regions.cols_test]
  Xtest <- data_test[,-regions.cols_test]
  # Select hyperparameters
  hyperparam <- kfoldCV.beta_pls(data_train, y_train, folds_idxs, pls.directions)

  # Fit model
  # Estimate projections
  pls.projections <- chemometrics::pls1_nipals(Xtrain, y_train, a = hyperparam$d.min, scale = TRUE)
  datatrain <- as.data.frame(cbind(y_train, as.matrix(Xtrain) %*% pls.projections$W))
  colnames(datatrain)[1] <- "y"
  datatest <- data.frame(as.matrix(Xtest) %*% pls.projections$W)
  colnames(datatest) <- colnames(datatrain)[-1]

  # Train and test data with dimension reduction and dummies of time and region - formula of fit
  datatrain <- data.frame(datatrain, Rtrain)
  datatest <- data.frame(datatest, Rtest)

  # Fit
  beta_pls.fit <- tryCatch(betareg::betareg(y ~ . , datatrain), error= function(e) {return(NA)}  )

  # Predict over test data
  ypred <- tryCatch(predict(beta_pls.fit, newdata = datatest), error= function(e) {return(NA)}  )
  ypred <- as.data.frame(ypred)
  colnames(ypred) <- "beta-pls"
  return(ypred)
}


# Beta-Tree-PLS -----------------------------------------------------------

method.beta_tree_pls <- function(data_train, y_train, data_test, folds_idxs, pls.directions = 30){
  y_train <- y_train + 1e-06 # ensure that is not equal to zero
  # Separate regions from data set
  regions.cols_train <- which(grepl(pattern = "^df.region", colnames(data_train)))
  regions.cols_test <- which(grepl(pattern = "^df.region", colnames(data_test)))
  Rtrain <- data_train[,regions.cols_train]
  Xtrain <- data_train[,-regions.cols_train]
  Rtest <- data_test[,regions.cols_test]
  Xtest <- data_test[,-regions.cols_test]
  # Select hyperparameters
  hyperparam <- kfoldCV.beta_tree_pls(data_train, y_train, folds_idxs, pls.directions)
  if(length(hyperparam$d.min) == 0){hyperparam$d.min <- 1}
  # Fit model
  # Estimate projections
  pls.projections <- chemometrics::pls1_nipals(Xtrain, y_train, a = hyperparam$d.min, scale = TRUE)
  datatrain <- as.data.frame(cbind(y_train, as.matrix(Xtrain) %*% pls.projections$W))
  colnames(datatrain)[1] <- "y"
  #datatrain$dummy <-  ifelse(y_train <= 0.2 , 1, 0)
  datatest <- data.frame(as.matrix(Xtest) %*% pls.projections$W)
  #datatest$dummy <-  ifelse(ytest <= 0.2 , 1, 0)
  colnames(datatest) <- colnames(datatrain)[-1]

  # Train and test data with dimension reduction and dummies of time and region - formula of fit
  datatrain <- data.frame(datatrain, Rtrain)
  datatest <- data.frame(datatest, Rtest)

  # Fit
  beta_tree_pls.fit <- tryCatch(betareg::betatree(y ~ . ,~ dummy , datatrain), error= function(e) {return(NA)}  )

  # Predict over test data
  ypred <- tryCatch(predict(beta_tree_pls.fit, newdata = datatest), error= function(e) {return(NA)}  )
  ypred <- as.data.frame(ypred)
  colnames(ypred) <- "beta-tree-pls"
  return(ypred)
}


# xgboost -----------------------------------------------------------------

method.xgboost <- function(data_train, y_train, data_test, nfolds){
  y_train <- y_train + 1e-06 # ensure that is not equal to zero
  # Select hyperparameters
  hyperparam <- kfoldCV.xgboost(data_train, y_train, nfolds)

  # Fitted model
  xgb.fit <- xgboost::xgboost(params = hyperparam$xgb.params,
                              data = as.matrix(data_train),
                              label = y_train,
                              nrounds = hyperparam$xgb.model, verbose = F, nthread = 5)
  # Predict  over test data
  ypred <- as.matrix(predict(xgb.fit, as.matrix(data_test)))

  colnames(ypred) <- "xgboost"
  return(ypred)
}

