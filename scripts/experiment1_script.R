#### EXPERIMENT 1 SCRIPT FILE ####

# The experiment depends mainly on the data set used for the
# experiment (see Table in Annex) and the target variable (MPI,H,A).
# In this script we have the example of dataframe 1 and MPI as the
# target. To reproduce the code just change the desired data frame
# and target variable.


# Install the last version ------------------------------------------------

devtools::install_github("https://github.com/girelaignacio/globalMPI_prediction_using_WID")


# Load package ------------------------------------------------------------

library(globalMPI.prediction.using.WID)

# Load data ---------------------------------------------------------------

which_data <- 1 # Which dataframe will be used (see Tale in Annex)
if (which_data == 1){
  data <- globalMPI.prediction.using.WID::dataframe1
  } else if (which_data == 2){
    data <- globalMPI.prediction.using.WID::dataframe2
  } else if (which_data == 13){
    data <- globalMPI.prediction.using.WID::dataframe13
  }

# Set parameters ----------------------------------------------------------

target <- c("MPI") # Target variable
nfolds <- 5        # Number of folds
methods <- c("elasticnet","betaboost") # Method to be used in estimation
split_size <- 0.8 # Split proportion train and test

R <- 1 # Repetitions

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
  globalMPI.prediction.using.WID::main_function(data = data, target = target, nfolds = nfolds,
                methods = methods, split_size = split_size)
}
experiment_end <- Sys.time()
experiment_time <- difftime(experiment_end, experiment_start, units = "mins")
experiment_time

parallel::stopCluster(cl = cl)


# Save results ------------------------------------------------------------

filename <- paste(getwd(),"/results/EXPERIMENT1_reps",
                  paste(R,target,paste("df",which_data, sep = ""), sep="_"),
                  sep = "")
saveRDS(results,file = filename)

# Load results to check!
load_results <- readRDS(filename)

