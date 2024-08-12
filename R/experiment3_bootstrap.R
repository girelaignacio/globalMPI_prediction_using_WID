# Experiment 3 - Bootstrap ------------------------------------------------
# This function contains the code used for each bootstrap step

boostrap <- function(in_sample, out_of_sample, ...){

  df.is <- in_sample
  df.oos <- out_of_sample

  boot.idxs <- sample(1:nrow(df.is), size = nrow(df.is), replace = TRUE)

  boot.rows <- lapply(boot.idxs, function(x) df.is[x,])

  boot.data <- do.call("rbind", boot.rows)

  # BetaBoost #
  # Fit model
  betaboost.fit <- mboost::glmboost(y ~ ., data = boot.data, center = FALSE,
                                    family = betaboost::BetaReg(), control = mboost::boost_control(mstop = 2000))

  # Predict out-of-sample
  betaboost.pred <- predict(betaboost.fit, newdata = df.oos[,-1], type = "response")

  # ElasticNet #
  # Select model with 5-fold CV
  hyperparam_elasticnet <- kfoldCV.elastic(boot.data[,-1], boot.data[,1], 5)
  # Fit model
  elastic.fit <- glmnet::glmnet(x = boot.data[,-1], y = boot.data[,1],
                                family = "gaussian",
                                alpha = hyperparam_elasticnet$best.alpha,
                                lambda = hyperparam_elasticnet$best.lambda)
  # Predict out-of-sample
  elastic.pred <- glmnet::predict.glmnet(elastic.fit, as.matrix(df.oos[,-1]))
  # save

  # Beta-tree elastic #
  # Create new dummy variable according to the distribution of the response variable
  cut.tree <- 0.2 # 0.2 value was taken in experiments 1 and 2
  dummy_cut.tree <- ifelse(boot.data[,1] <= cut.tree, 1, 0)
  dummy_cut.tree <- as.vector(ifelse(df.oos[,1] <= cut.tree, 1, 0))
  # Retrieve selected elastic net coefficients
  elastic.coefs <- coef(elastic.fit, s = "lambda.min")
  temp <- c("y",elastic.coefs@Dimnames[[1]][elastic.coefs@i +1])
  num_chosen_ela <-  length(temp)
  betatree.train <- boot.data[,c(colnames(boot.data) %in% temp)]
  betatree.test <- df.oos[,colnames(df.oos) %in% temp]
  # Fit
  betatree.fit <- tryCatch(betareg::betareg(y ~ ., data = betatree.train, link.phi = "log", link = "logit"), error= function(e) {return(NA)}  )
  # Predict out-of-sample
  betatree.pred <- tryCatch(predict(betatree.fit, betatree.test[,-1]), error= function(e) {return(NA)}  )

  # XGBoost #
  # Select model with 5-fold CV
  hyperparam_xgb <- kfoldCV.xgboost(boot.data[,-1], boot.data[,1], 5)
  # Fitted model
  xgb.fit <- hyperparam_xgb$xgb.model
  # Predict out-of-sample
  xgb.pred <- as.matrix(predict(xgb.fit, df.oos[,-1]))
  # These preds do not have rownames
  rownames(xgb.pred) <- rownames(elastic.pred)

  # Save results
  results <- cbind(betaboost.pred, elastic.pred, betatree.pred, xgb.pred)
  colnames(results) <- c("betaboost", "elasticnet", "betatree","xgboost")

  return(results)
}
