### Processing data script file ###
devtools::load_all()


# Load data ---------------------------------------------------------------

webscrapped_data <- webscrapped_data_experiments1and2


# Preprocess data ---------------------------------------------------------

data <- data_preprocessing(webscrapped_data)


# Process data ------------------------------------------------------------

# This is an example for dataframe 1 (see Table in Annex)
# How the rest of the dataframe are called and saved is straightforward
which_df <- 2
df <- data_processing(data, which_df)

# save it
assign(paste("dataframe",which_df, sep = ""), df)
usethis::use_data(dataframe2)
