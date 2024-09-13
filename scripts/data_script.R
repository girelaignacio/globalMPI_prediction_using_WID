### Processing data script file ###
devtools::load_all()


# Load data ---------------------------------------------------------------

webscrapped_data <- webscrapped_data_experiments1and2


# Preprocess data ---------------------------------------------------------

data <- data_preprocessing(webscrapped_data)


# Process data ------------------------------------------------------------

# This is an example for data set 1 (see Table in Appendix A)
# How the rest of the data set are called and saved is straightforward
which_df <- 1
df <- data_processing(data, which_df)

# save it
assign(paste("dataset",which_df, sep = ""), df)
usethis::use_data(dataset1)
