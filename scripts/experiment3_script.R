#### EXPERIMENT 1 SCRIPT FILE ####

# The experiment depends mainly on the data set used for the
# experiment (see Table in Annex) and the target variable (MPI,H,A).
# In this script we have the example of dataframe 1 and MPI as the
# target. To reproduce the code just change the desired data frame
# and target variable.

# Install the last version ------------------------------------------------

devtools::install_github("https://github.com/girelaignacio/globalMPI_prediction_using_WID")

# Load data ---------------------------------------------------------------

which_data <- 1 # Which dataframe will be used (see Tale in Annex)
if (which_data == 1){
  data <- globalMPI.prediction.using.WID::dataframe1
} else if (which_data == 2){
  data <- globalMPI.prediction.using.WID::dataframe2
} else if (which_data == 13){
  data <- globalMPI.prediction.using.WID::dataframe13
}
