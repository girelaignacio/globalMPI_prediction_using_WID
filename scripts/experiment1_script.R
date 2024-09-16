#### EXPERIMENT 1 SCRIPT FILE ####

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
methods <- c("linear-pls","beta-pls","beta-tree-pls",
             "elasticnet","beta-elastic","beta-tree-elastic",
             "betaboost","xgboost")# by default all methods are used.
split_size <- 0.8 # Split proportion train and test

R <- 100 # Repetitions

# Run paralleled code ---------------------------------------------------

library(doParallel)

cores <- 26
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
results <- foreach(i = 1:R) %dopar% {
  globalMPI.prediction.using.WID::main_function_exp1(data = data, target = target, methods = methods,
                                                     nfolds = nfolds, split_size = split_size)
}
experiment_end <- Sys.time()
experiment_time <- difftime(experiment_end, experiment_start, units = "mins")
experiment_time

parallel::stopCluster(cl = cl)

# Save results ------------------------------------------------------------

filename <- paste(getwd(),"/results/EXPERIMENT1_reps",
                  paste(R,target,paste("dataset",which_data, sep = ""), sep="_"),
                  sep = "")
saveRDS(results,file = filename)

# Load results to check!
load_results <- readRDS(filename)

