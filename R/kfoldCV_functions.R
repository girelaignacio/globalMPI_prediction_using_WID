#### KFold CV Functions ####


# Elasticnet --------------------------------------------------------------

kfoldCV.elastic <- function(Xtrain, ytrain, folds_idxs) {

  Xtrain <- as.matrix(Xtrain)

  #search the alpha
  alpha <- seq(0.01, 0.99, 0.04)
  best <- list(a=NULL, mse=NULL)

  for (i in 1:length(alpha))
  {
    cvg <- glmnet::cv.glmnet(x = Xtrain, y=ytrain, foldid = folds_idxs, family = "gaussian", alpha = alpha[i])
    best$a <- c(best$a, alpha[i])
    best$mse <- c(best$mse, min(cvg$cvm))
  }

  index <- which(best$mse==min(best$mse))
  best.alpha <- best$a[index]

  elastic_cv <- glmnet::cv.glmnet(x = Xtrain, y = ytrain, foldid = folds_idxs, family = "gaussian", alpha = best.alpha)


  return(list(best.alpha =  best.alpha,  best.lambda = elastic_cv$lambda.min ))
}

# Betaboost ---------------------------------------------------------------

kfoldCV.betaboost <- function(data, folds_idxs) {
  K <- length(unique(folds_idxs))
  n <- nrow(data)
  id <- folds_idxs #sample(1:K, n, replace=TRUE, prob=rep(1/K,K))
  # Hyperparameters
  hyper_max_depth <- c(3,5)

  # empty array to save results
  results <- array(NA, dim=c(length(hyper_max_depth),K))
  for(lambda in 1:length(hyper_max_depth)){
    hyperparam <- hyper_max_depth[lambda]
    for(i in 1:K){
      data_train.i <- data[id!=i,]
      data_test.i <- data[id==i,]


      betaboost.fit <- mboost::blackboost(y ~ ., data = data_train.i, family = betaboost::BetaReg(),
                                          control = mboost::boost_control(mstop = 100),
                                          tree_controls = partykit::ctree_control(maxdepth = hyperparam))

      ypred <- predict(betaboost.fit, newdata = data_test.i[,-1], type = "response")

      mse.i <- mean((ypred - data_test.i[,1])^2)

      results[lambda, i] <- mse.i
    }
  }
  min.mse <- hyper_max_depth[which.min(apply(results, MARGIN = 1, FUN = mean))]

  return(list(max_depth.min = min.mse))

}


# linearpls ---------------------------------------------------------------

kfoldCV.pls <- function(X, y, folds_idxs, max.d) {

  X <- as.matrix(X)
  regions.cols <- which(grepl(pattern = "^df.region", colnames(X)))

  K <- length(unique(folds_idxs))
  D <- max.d
  n <- nrow(X); p <- ncol(X) #number of observations and number of predictors
  #id <- sample(1:K, n, replace=TRUE, prob=rep(1/K,K))
  id <- folds_idxs

  results <- array(NA, dim=c(D,K))
  for (d in 1:D){
    for(i in 1:K){

      ytrain <- y[id!=i]
      Xtrain <- X[id!=i,-regions.cols]
      Rtrain <- X[id!=i,regions.cols]

      ytest <- y[id==i]
      Xtest <- X[id==i,-regions.cols]
      Rtest <- X[id==i,regions.cols]

      W <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d, scale = FALSE)$W

      # Projection in train set
      Pr_train <- as.matrix(Xtrain)%*%W
      # Projection in test set
      Pr_test <- as.matrix(Xtest)%*%W

      # Fit linear model
        lm.data <- data.frame(cbind(ytrain,Pr_train,Rtrain))
      lm.fit <- lm(ytrain ~., data = lm.data, na.action="na.exclude")
      # Predictions in test
      newdata <- data.frame(cbind(Pr_test,Rtest))
        colnames(newdata) <- colnames(lm.data)[-1]
      y_pred_test <- predict(lm.fit, newdata = newdata)
      MSE_test <- mean(c(ytest-y_pred_test)^2, na.rm = TRUE)

      results[d, i] <- MSE_test
    }
  }
  min.MSE <- which.min(apply(results, MARGIN = 1, FUN = mean))

  return(list( d.min = min.MSE))
}


# Beta-PLS ----------------------------------------------------------------

kfoldCV.beta_pls <- function(X, y, folds_idxs, max.d) {

  X <- as.matrix(X)
  regions.cols <- which(grepl(pattern = "^df.region", colnames(X)))

  K <- length(unique(folds_idxs))
  D <- max.d
  n <- nrow(X); p <- ncol(X) #number of observations and number of predictors
  #id <- sample(1:K, n, replace=TRUE, prob=rep(1/K,K))
  id <- folds_idxs

  results <- array(NA, dim=c(D,K))
  for (d in 1:D){
    for(i in 1:K){

      ytrain <- y[id!=i]
      Xtrain <- X[id!=i,-regions.cols]
      Rtrain <- X[id!=i,regions.cols]

      ytest <- y[id==i]
      Xtest <- X[id==i,-regions.cols]
      Rtest <- X[id==i,regions.cols]

      W <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d, scale = FALSE)$W

      # Projection in train set
      Pr_train <- as.matrix(Xtrain)%*%W
      # Projection in test set
      Pr_test <- as.matrix(Xtest)%*%W

      # Fit beta PLs model
      beta.data <- data.frame(cbind(ytrain,Pr_train,Rtrain))
      # beta.fit <- tryCatch(betareg::betareg(ytrain ~., data = beta.data,
      #                                       na.action="na.exclude"),
      #                      error= function(e) {return(NA)})
      beta.fit <- tryCatch(betareg::betareg(ytrain ~., data = beta.data,link.phi = "log", link = "logit"), error= function(e) {return(NA)}  )

      # Predictions in test
      newdata <- data.frame(cbind(Pr_test,Rtest))
      colnames(newdata) <- colnames(beta.data)[-1]
      y_pred_test <- tryCatch(predict(beta.fit, newdata = newdata), error= function(e) {return(rep(NA,nrow(newdata)))}  )
      MSE_test <- mean(c(ytest-y_pred_test)^2, na.rm = TRUE)

      results[d, i] <- MSE_test
    }
  }
  min.MSE <- which.min(apply(results, MARGIN = 1, FUN = mean, na.rm = TRUE))

  return(list( d.min = min.MSE))
}


# xgboost -----------------------------------------------------------------

kfoldCV.xgboost <- function(X, y, folds_idxs) {

  X <- as.matrix(X)
  Y <- as.matrix(y)
  K <- length(unique(folds_idxs))

  # Custom folds idxs
  custom.folds <- vector("list", K)
  for( k in unique(folds_idxs)){
    custom.folds[[k]] <- which( folds_idxs == k )
    }

  XGBparams <-  list(objective = "reg:squarederror",
                     max_depth = c(3,5,7),
                     # number of trees
                     # default values below
                     eta = 0.3,
                     gamma = 0,
                     subsample = 1,
                     min_child_weight = 1,
                     colsample_bytree = 0.6)


  xgbcv <- xgboost::xgb.cv(data= X,
                           params = XGBparams, label = y,
                           folds = custom.folds, nrounds = 100,
                           verbose = F, nthread=2)

  optimal.rounds <- which.min(xgbcv$evaluation_log$test_rmse_mean)

  return(list( xgb.model = optimal.rounds, xgb.params = XGBparams))
}


# beta-tree-PLS -----------------------------------------------------------

kfoldCV.beta_tree_pls <- function(X, y, folds_idxs, max.d) {

  X <- as.matrix(X)
  regions.cols <- which(grepl(pattern = "^df.region", colnames(X)))

  K <- length(unique(folds_idxs))
  D <- max.d
  n <- nrow(X); p <- ncol(X) #number of observations and number of predictors
  id <- folds_idxs

  results <- array(NA, dim=c(D,K))
  for (d in 1:D){
    for(i in 1:K){

      ytrain <- y[id!=i]
      Xtrain <- X[id!=i,-regions.cols]
      Rtrain <- X[id!=i,regions.cols]

      ytest <- y[id==i]
      Xtest <- X[id==i,-regions.cols]
      Rtest <- X[id==i,regions.cols]

      W <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d, scale = FALSE)$W

      # Projection in train set
      Pr_train <- as.matrix(Xtrain)%*%W
      # Projection in test set
      Pr_test <- as.matrix(Xtest)%*%W

      # Fit beta PLS model
      dummy <- ifelse(ytrain <= 0.2 , 1, 0)
      beta.data <- data.frame(cbind(ytrain,Pr_train,Rtrain,dummy))
      # beta.fit <- tryCatch(betareg::betatree(ytrain ~., ~dummy ,data = beta.data,
      #                                       na.action="na.exclude",link.phi = "log", link = "logit"),
      #                      error= function(e) {return(NA)})
      beta.fit <- betareg::betatree(ytrain ~., ~dummy ,data = beta.data,link.phi = "log", link = "logit")
      # Predictions in test
      dummy <- ifelse(ytest <= 0.2 , 1, 0)
      newdata <- data.frame(cbind(Pr_test,Rtest, dummy))
      colnames(newdata) <- colnames(beta.data)[-1]
      y_pred_test <- tryCatch(predict(beta.fit, newdata = newdata), error= function(e) {return(rep(NA,nrow(newdata)))}  )
      MSE_test <- mean(c(ytest-y_pred_test)^2, na.rm = TRUE)

      results[d, i] <- MSE_test
    }
  }
  min.MSE <- which.min(apply(results, MARGIN = 1, FUN = mean, na.rm = TRUE))

  return(list( d.min = min.MSE))
}
