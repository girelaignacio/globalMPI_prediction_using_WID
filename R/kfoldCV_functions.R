#### KFold CV Functions ####


# Elasticnet --------------------------------------------------------------

kfoldCV.elastic <- function(Xtrain, ytrain, nfolds) {

  Xtrain <- as.matrix(Xtrain)

  #search the alpha
  alpha <- seq(0.01, 0.99, 0.04)
  best <- list(a=NULL, mse=NULL)

  for (i in 1:length(alpha))
  {
    cvg <- glmnet::cv.glmnet(x = Xtrain, y=ytrain, nfolds = nfolds, family = "gaussian", alpha = alpha[i])
    best$a <- c(best$a, alpha[i])
    best$mse <- c(best$mse, min(cvg$cvm))
  }

  index <- which(best$mse==min(best$mse))
  best.alpha <- best$a[index]

  elastic_cv <- glmnet::cv.glmnet(x = Xtrain, y = ytrain, family = "gaussian", alpha = best.alpha)


  return(list(best.alpha =  best.alpha,  best.lambda = elastic_cv$lambda.min ))
}

# Betaboost ---------------------------------------------------------------

kfoldCV.betaboost <- function(data, nfolds) {
  print("kfold working")
  K <- nfolds
  n <- nrow(data)
  id <- sample(1:K, n, replace=TRUE, prob=rep(1/K,K))
  print(id)
  # Hyperparameters
  hyper_max_depth <- c(3,5,7)

  # empty array to save results
  results <- array(NA, dim=c(length(hyper_max_depth),K))
  print("Here it will start estimating")
  for(lambda in 1:length(hyper_max_depth)){
    hyperparam <- hyper_max_depth[lambda]
    print(paste("Hyperparam: ", hyperparam))
    for(i in 1:K){
      data_train.i <- data[id!=i,]
      data_test.i <- data[id==i,]

      print(dim(data_train.i))


      betaboost.fit <- mboost::blackboost(y ~ ., data = data_train.i, family = betaboost::BetaReg(),
                                          control = mboost::boost_control(mstop = 200),
                                          tree_controls = partykit::ctree_control(maxdepth = hyperparam))

      ypred <- predict(betaboost.fit, newdata = data_test.i[,-1], type = "response")

      mse.i <- mean((ypred - data_test.i[,1])^2)

      results[lambda, i] <- mse.i
    }
  }
  min.mse <- hyper_max_depth[which.min(apply(results, MARGIN = 1, FUN = mean))]

  return(list(max_depth.min = min.mse))

}
