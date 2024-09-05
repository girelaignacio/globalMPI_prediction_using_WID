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
