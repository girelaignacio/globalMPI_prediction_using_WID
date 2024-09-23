#### Auxiliary utils functions ####


# Split train and test data -----------------------------------------------

random.split <- function(df, train_size){
  sam <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(train_size,1-train_size))
  train <- df[sam,]
  test <- df[!sam,]
  return(list(data_train = train, data_test = test))
}


# kfold_indices -----------------------------------------------------------

#' Create K-fold indices for Cross Validation
#'
#' @param y vector of observations
#' @param k number of folds
#' @param ...
#'
#' @return a vector with length(y) with the corresponding fold each observations belongs
#' @export
#'
Kfold_idxs <- function(y, k, ...){
  # number of observations
  n <- length(y)
  # i-th observation index
  i_index <- 1:n
  # sample i_index
  sampled_index <- sample(i_index, n, replace = FALSE)
  # sample and divide data
  fold_index <- cut(sampled_index, k, labels = 1:k)
  return(fold_index)
}
