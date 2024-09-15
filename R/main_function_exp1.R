#' Function used for experiment 1
#'
#' @param data Which of the data frames in Annex is preferred to used
#' @param target Target variable ("MPI","H","A")
#' @param nfolds Number of folds used in K-Fold Cross-Validation for hyperparameter selection
#' @param methods Which of the methods to be used ("linear-pls","beta-pls","beta-tree-pls","elasticnet","beta-elastic","beta-tree-elastic","betaboost","xgboost")
#' @param split_size Proportion of the split of train and test data
#' @param ... other arguments
#'
#' @return a data frame with the prediction of each method and its ground truth
#' @export
#'
main_function_exp1 <- function(data = NULL, target = c("MPI","H","A"), nfolds = 5,
                          methods = c("linear-pls","beta-pls","beta-tree-pls",
                                      "elasticnet","beta-elastic","beta-tree-elastic",
                                      "betaboost","xgboost"), split_size = 0.8, ...){

  # target to lower case ----------------------------------------------------
  target <- switch(target,
                   MPI = {"mpi"},
                   H = {"h"},
                   A = {"a"})

  # Preprocess data ---------------------------------------------------------

  # Include time effects as a trend
  data$year_trend <- as.numeric(data$year)
  data$year_trend <- data$year_trend - min(data$year_trend)

  # Split data in train and test
    # Train
  split <- random.split(data, split_size)
  ytrain <- split$data_train[, target]
  Xtrain <- split$data_train[,-which(colnames(split$data_train) %in% c("iso","region","country","year","mpi","h","a"))]
  rownames(Xtrain) <- names(ytrain) <- paste(split$data_train$country,split$data_train$year, sep = ".")

    # Test
  ytest <- split$data_test[, target]
  Xtest <- split$data_test[,-which(colnames(split$data_train) %in% c("iso","region","country","year","mpi","h","a"))]
  rownames(Xtest) <- names(ytest) <- paste(split$data_test$country,split$data_test$year, sep = ".")


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
  predictions <- parallel::mclapply(methods, FUN = function(x) {
    switch(x,
           "linear-pls" = {method.linearpls(Xtrain, ytrain, Xtest, nfolds)},
           "beta-pls" = {method.beta_pls(Xtrain, ytrain, Xtest, nfolds)},
           "beta-tree-pls" = {method.beta_tree_pls(Xtrain, ytrain, Xtest, nfolds)},
           "elasticnet" = {method.elasticnet(Xtrain, ytrain, Xtest, nfolds, betareg = betareg_elastic, betatree = beta_tree_elastic)},
           "xgboost" = {method.xgboost(Xtrain, ytrain, Xtest, nfolds)},
           "betaboost" = {method.betaboost(Xtrain, ytrain, Xtest, nfolds)})
  }, mc.cores = length(methods))

  out <- do.call("cbind", predictions)
  ground.truth <- ytest
  out <- cbind(ground.truth, out)

  return(out)
}
