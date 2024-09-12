#### Methods for prediction functions ####

# Elasticnet --------------------------------------------------------------

method.elasticnet <- function(data_train, y_train, data_test, nfolds){
  # Select hyperparameters
  hyperparam <- kfoldCV.elastic(data_train, y_train, nfolds)
  # Fit model
  elastic.fit <- glmnet::glmnet(x = data_train, y = y_train, family = "gaussian",
                                alpha = hyperparam$best.alpha,
                                lambda = hyperparam$best.lambda)
  # Predict over test data
  ypred <- glmnet::predict.glmnet(elastic.fit, as.matrix(data_test))
  colnames(ypred) <- "elasticnet"
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

method.linearpls <- function(data_train, y_train, data_test, nfolds, pls.directions = 30){
  # Separate regions from data set
  regions.cols_train <- which(grepl(pattern = "^df.region", colnames(data_train)))
  regions.cols_test <- which(grepl(pattern = "^df.region", colnames(data_test)))
  Rtrain <- data_train[,regions.cols_train]
  Xtrain <- data_train[,-regions.cols_train]
  Rtest <- data_test[,regions.cols_test]
  Xtest <- data_test[,-regions.cols_test]
  # Select hyperparameters
  hyperparam <- kfoldCV.pls(data_train, y_train, nfolds, pls.directions)

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


# xgboost -----------------------------------------------------------------

method.xgboost <- function(data_train, y_train, data_test, nfolds){
  # Select hyperparameters
  hyperparam <- kfoldCV.xgboost(data_train, y_train, nfolds)
    # This returns the best xgboost model

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

