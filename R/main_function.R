main_function <- function(data = NULL, target = c("MPI","H","A"), nfolds = 5,
                          methods = c("elasticnet","betaboost"), ...){

  # target to lower case ----------------------------------------------------
  target <- switch(target,
                   MPI = {"mpi"},
                   H = {"h"},
                   A = {"a"})

  # Preprocess data ---------------------------------------------------------

  # Include time effects as a trend
  data$year_trend <- as.numeric(as.character(data$year_Other))
  data$year_trend <- data$year_trend - min(data$year_trend)

  # Split data in train and test
  split <- random.split(data, 0.8)
  ytrain <- split$data_train[, target]
  Xtrain <- split$data_train[,-c(1:33)]

  ytest <- split$data_test[, target]
  Xtest <- split$data_test[,-c(1:33)]

  # keep regions dummy variables apart
  Rtrain <- split$data_train[, c(8:12)]
  Rtest <- split$data_test[, c(8:12)]


  # Split in train and test predictors
  data_train <- data.frame(Xtrain, Rtrain)
  data_test <- data.frame(Xtest, Rtest)
  #for PLS....
  #regions.cols <- which(grepl(pattern = "^df.region_", colnames(data_train)))

  predictions <- lapply(methods, FUN = function(x) {
    switch(x,
           "elasticnet" = {method.elasticnet(data_train, ytrain, data_test, nfolds)},
           "betaboost" = {method.betaboost(data_train, ytrain, data_test, nfolds)})
  })

  return(apply((do.call("cbind",predictions) - ytest)^2, 2, mean) )
}
