#### EXPERIMENT 2 SCRIPT FILE ####

# The experiment depends mainly on the data set used for the
# experiment (see Table in Appendix A) and the target variable (MPI,H,A).
# In this script we have the example of data set 1 and MPI as the
# target. To reproduce the code just change the desired data frame
# and target variable.

# Install the last version ------------------------------------------------

devtools::install_github("https://github.com/girelaignacio/globalMPI_prediction_using_WID")


# Load package ------------------------------------------------------------

library(globalMPI.prediction.using.WID)

# Load data ---------------------------------------------------------------

which_data <- 1 # Which data set will be used (see Table in Appendix A)
if (which_data == 1){
  data <- globalMPI.prediction.using.WID::dataset1
} else if (which_data == 2){
  data <- globalMPI.prediction.using.WID::dataset2
} else if (which_data == 13){
  data <- globalMPI.prediction.using.WID::dataframe13
}

# Set parameters ----------------------------------------------------------

target <- "MPI"    # Target variable
nfolds <- 5        # Number of folds
methods <- c("xgboost")# by default all methods are used.


# Run 10-Fold CV ----------------------------------------------------------

# Define folds indices
folds_index <- Kfold_idxs(data[,1], 10)
results <- lapply(1:10, function(x){
  print(x)
  testIdxs <- which(folds_index == x,arr.ind=TRUE)
  return(suppressWarnings(main_function_exp2(data = data, testIndexes = testIdxs,
                     target = target, methods = methods, nfolds = nfolds)))
})

# Run paralleled code ---------------------------------------------------

library(doParallel)

cores <- 10
#create the cluster
cl <- parallel::makeCluster(cores)
# check cluster definition
print(cl)
# register it to be used by %dopar%
doParallel::registerDoParallel(cl = cl)

#check if it is registered (optional)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

experiment_start <- Sys.time()
results <- foreach(
  i = 1:10,
) %do% {
  testIdxs <- which(folds_index == i,arr.ind=TRUE)
  main_function_exp2(data = data, testIndexes = testIdxs,
      target = target, methods = methods,
      nfolds = nfolds)
}
experiment_end <- Sys.time()
experiment_time <- difftime(experiment_end, experiment_start, units = "mins")
experiment_time

parallel::stopCluster(cl = cl)

# Save results ------------------------------------------------------------

filename <- paste(getwd(),"/results/EXPERIMENT2_",
                  paste(target,paste("dataset",which_data, sep = ""), sep="_"),
                  sep = "")
saveRDS(results,file = filename)

# Load results to check!
load_results <- readRDS(filename)
