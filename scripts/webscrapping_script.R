### Webscrapping script file ###
devtools::load_all()


# Load data ---------------------------------------------------------------

# 1.1 Load global MPI data from 2000-2021
MPI <- MPI_data(globalMPI_all_years_raw)
countries <- unique(MPI$iso)

# 1.2 Download World Bank data
  # 1.2.1 Write url to download data
WB_url = "https://api.worldbank.org/v2/en/topic/1?downloadformat=csv"
data <- WB_data(WB_url)
  # 1.2.1 Clean WID data
data <- clean_WB_data(data,countries)


# Transform WID data format -----------------------------------------------
# 2.1 Transform WID data format for merging with MPI data
data_long <- WB_data_long(data)


# Create data 1 -----------------------------------------------------------
# This functions create the raw data for experiments 1 and 2 in the paper
# prior to NAs treatment

# 3.1 Join global MPI and WID dataframes
data1 <- data_join2(MPI, data_long, lags = 1)
# 3.2 Preserve the dataabse without lags (keep years with MPI observations)
data1 <- data1[(data1$year == data1$variable),]
# 3.3 Iteratively download the different topics of the WID data and merge it
# with data1 which has the proper data format
# The object "total" has the raw dataframe the the MPI and WID observations
# used in the first and second experiments of the paper
total <- data1
for (i in 2:21){
  WB_url = paste0("https://api.worldbank.org/v2/en/topic/",i,"?downloadformat=csv")
  data <- WB_data(WB_url)
  data <- clean_WB_data(data,countries)
  data <- data[!(data$indicator %in% colnames(data1)),]
  data_long <- WB_data_long(data)
  data2 <- data_join(MPI, data_long, 1)
  # Database without lags
  data2 <- data2[(data2$year == data2$variable),]
  data3 <- as.data.frame(data2[, !(colnames(data2) %in% colnames(total))])
  data3[,c("iso","country","year","MPI","H","A","variable")] <- data2[,c("iso","country","year","MPI","H","A","variable")]
  total <- merge(total,data3, by=c("iso","country","year","MPI","H","A","variable"))
}

# Create data 2 -----------------------------------------------------------
# This functions create the raw data for experiment 3 in the paper
# prior to NAs treatment

# 4.1 Join global MPI and WID dataframes
data2 <- merge(MPI,data_long, by.x = c("iso"="iso","country"="country"))
# 4.2 Iteratively download the different topics of the WID data and merge it
# with data2 which has the proper data format
# The object "total" has the raw dataframe the the MPI and WID observations
# used in the third experiment of the paper that predicts out-of-sample.
total <- data2
for (i in 2:21){
  WB_url = paste0("https://api.worldbank.org/v2/en/topic/",i,"?downloadformat=csv")
  data_i <- WB_data(WB_url)
  data_i <- clean_WB_data(data_i,countries)
  data_i <- data_i[!(data_i$indicator %in% colnames(total)),]
  data_long_i <- WB_data_long(data_i)
  total <- merge(total, data_long_i, all.x = TRUE)
}
