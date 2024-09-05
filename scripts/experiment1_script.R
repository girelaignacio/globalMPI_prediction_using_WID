#### EXPERIMENT 1 SCRIPT FILE ####
devtools::install_github("https://github.com/girelaignacio/globalMPI_prediction_using_WID")

# The experiment depends mainly on the data set used for the
# experiment (see Table in Annex) and the target variable (MPI,H,A).
# In this script we have the example of dataframe 1 and MPI as the
# target. To reproduce the code just change the desired dataframe1
# and target variable.

# Load data ---------------------------------------------------------------

data <- dataframe1

# Set parameters ----------------------------------------------------------

target <- c("MPI")
nfolds <- 5
methods <- c("elasticnet","betaboost")
split_size <- 0.8

R <- 1 # Repetitions


main_function(data = data, target = target, nfolds = nfolds,
              methods = methods, split_size = split_size)

# Run parallelized code ---------------------------------------------------

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
  main_function(data = data, target = target, nfolds = nfolds,
                methods = methods, split_size = split_size)
}
experiment_end <- Sys.time()
experiment_time <- difftime(experiment_end, experiment_start, units = "mins")
experiment_time

parallel::stopCluster(cl = cl)

# end of code

apply((do.call("cbind",results) - ytest)^2, 2, mean)
