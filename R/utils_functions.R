#### Auxiliary utils functions ####


# Split train and test data -----------------------------------------------

random.split <- function(df, train_size){
  sam <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(train_size,1-train_size))
  train <- df[sam,]
  test <- df[!sam,]
  return(list(data_train = train, data_test = test))
}

