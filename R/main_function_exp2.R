#' Function used for experiment 2
#'
#' @param data Which of the datasets in Appendix A is preferred to used
#' @param testIndexes Test indexes
#' @param target Target variable ("MPI","H","A")
#' @param nfolds Number of folds used in K-Fold Cross-Validation for hyperparameter selection
#' @param methods Which of the methods to be used ("linear-pls","beta-pls","beta-tree-pls","elasticnet","beta-elastic","beta-tree-elastic","betaboost","xgboost")
#' @param ... other arguments
#'
#' @return a data frame with the prediction of each method and its ground truth
#' @export
#'
main_function_exp2 <- function(data = NULL, testIndexes = NULL,  target = c("MPI","H","A"), nfolds = 5,
                               methods = c("linear-pls","beta-pls","beta-tree-pls",
                                           "elasticnet","beta-elastic","beta-tree-elastic",
                                           "betaboost","xgboost"), ...){

  # target to lower case ----------------------------------------------------
  target <- switch(target,
                   MPI = {"mpi"},
                   H = {"h"},
                   A = {"a"})

  # Preprocess data ---------------------------------------------------------

  # Include time effects as a trend
  data$year_trend <- as.numeric(data$year)
  data$year_trend <- data$year_trend - min(data$year_trend)

  # Split data in train and test using testIndexes argument
  # Train
  data_train <- data[-testIndexes,]
  ytrain <- data_train[, target]
  Xtrain <- data_train[,-which(colnames(data_train) %in% c("iso","region","country","year","mpi","h","a"))]
  rownames(Xtrain) <- names(ytrain) <- paste(data_train$country,data_train$year, sep = ".")

  # Test
  data_test <- data[testIndexes,]
  ytest <- data_test[, target]
  Xtest <- data_test[,-which(colnames(data_train) %in% c("iso","region","country","year","mpi","h","a"))]
  rownames(Xtest) <- names(ytest) <- paste(data_test$country,data_test$year, sep = ".")


  # K-fold indices ----------------------------------------------------------

  folds_idxs <- as.numeric(Kfold_idxs(ytrain, nfolds))

  # check methods argument when betareg and betatree is used with elasticnet
  # selected coefficients
  # beta regression with elasticnet coeffs
  if("beta-elastic" %in% methods){
    betareg_elastic = TRUE
    methods <- methods[methods != "beta-elastic"]
    if(!"elasticnet" %in% methods){
      methods <- c(methods, "elasticnet")
    }
  }else{betareg_elastic = FALSE}
  # beta tree regression with elasticnet coeffs
  if("beta-tree-elastic" %in% methods){
    beta_tree_elastic = TRUE
    methods <- methods[methods != "beta-tree-elastic"]
    if(!"elasticnet" %in% methods){
      methods <- c(methods, "elasticnet")
    }
  }else{beta_tree_elastic = FALSE}

  # RUN METHODS
  predictions <- lapply(methods, FUN = function(x) {
    switch(x,
           "linear-pls" = {method.linearpls(Xtrain, ytrain, Xtest, folds_idxs)},
           "beta-pls" = {method.beta_pls(Xtrain, ytrain, Xtest, folds_idxs)},
           "beta-tree-pls" = {method.beta_tree_pls(Xtrain, ytrain, Xtest, folds_idxs)},
           "elasticnet" = {method.elasticnet(Xtrain, ytrain, Xtest, folds_idxs, betareg = betareg_elastic, betatree = beta_tree_elastic)},
           "xgboost" = {method.xgboost(Xtrain, ytrain, Xtest, folds_idxs)},
           "betaboost" = {method.betaboost(Xtrain, ytrain, Xtest, folds_idxs)})
  })

  out <- do.call("cbind", predictions)
  ground.truth <- ytest
  out <- cbind(ground.truth, out)

  return(out)
}
