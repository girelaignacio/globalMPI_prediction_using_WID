#
# library(dplyr)
# library(tidyr)

MPI_data <- function(data){
  # 1.1 Download MPI dataset
  #MPI <- openxlsx::read.xlsx("C:/Users/girel/Dropbox/BiblioGirela/PLS/table-8-all-mpi-data-2010-2022.xlsx",
  #                           sheet = "8.1 All Published MPI's", startRow = 10, colNames = FALSE)[,2:8]
  MPI <- data
  # 1.2 Change Columns names
  cols_MPI <- c("iso","country","survey","year_survey","MPI","H","A")
  colnames(MPI) <- cols_MPI
  # 1.3 Take countries in the MPI for further operations
  countries <- unique(MPI$iso)
  # 1.4 Create "year" variable (take the last year of survey years)
  MPI$year <- as.numeric(substr(MPI$year_survey,nchar(MPI$year_survey) - 4 + 1, nchar(MPI$year_survey)))
  # 1.5 Discard missing values
  MPI <- stats::na.omit(MPI)
  # 1.6 Select variables
  MPI <- MPI[,c("iso", "country","year","MPI","H","A")]
  # 1.7 Remove duplicates
  MPI <- MPI[!(duplicated(MPI[c("iso","year")]) | duplicated(MPI[c("iso","year")], fromLast = TRUE)), ]
  return(MPI)
}


WB_data2 <- function(url){
  #temp <- "C:/Users/girel/Downloads"
  temp <- tempfile()
  utils::download.file(url,temp)
  filename <- utils::unzip(temp,list=T)$Name[grep("^API", utils::unzip(temp,list=T)$Name, ignore.case=TRUE)]
  data <- utils::read.csv(unz(temp,filename), header = T,skip = 3, as.is = T)
  return(data)
}

WB_data <- function(url){
  #temp <- tempfile(fileext=".csv")
  temp_file_path <- tempfile(fileext = ".zip")
  utils::download.file(url, destfile = temp_file_path, mode = "wb")
  #download.file(url,temp)
  filename <- utils::unzip(temp_file_path,list=T)$Name[grep("^API", utils::unzip(temp_file_path,list=T)$Name, ignore.case=TRUE)]
  data <- utils::read.csv(unz(temp_file_path,filename), header = T,skip = 3 ,as.is = T)
  #unzip(temp_file_path, exdir = temp_dir)
  return(data)
}

clean_WB_data <- function(WBdata, countries_list){
  WBdata <- WBdata[WBdata$Country.Code %in% countries_list,-4] # 2.1. Keep only countries in MPI data and drop "Indicator.Code" variable
  WBdata <- WBdata[,-ncol(WBdata)] # Drop the last empty column
  colnames(WBdata)[1:3] <- c("country","iso","indicator") # Change some columns' names - to match with MPI data
  years_to_drop <- c(grep("^X196", colnames(WBdata)), grep("^X197", colnames(WBdata)), grep("^X198", colnames(WBdata)))#, grep("^X199", colnames(WBdata)))
  WBdata <- WBdata[,-years_to_drop] # Keep from 2000 year
  years <- sub("X","", colnames(WBdata)[startsWith(colnames(WBdata), "X")]) # Rule out "X" before every
  colnames(WBdata)[startsWith(colnames(WBdata), "X")] <- years
  WBdata$indicator <- as.factor(WBdata$indicator) # Set "indicator" as factor
  return(WBdata)
}

WB_data_long <- function(WBdata) {
  WBdata_long <- reshape2::melt(WBdata[WBdata$indicator == unique(WBdata$indicator)[1],-3], id.vars=c("iso", "country"))[,1:3]
  #tobe_included <- 0
  for (col in unique(WBdata$indicator)) {
    print(col)
    indic_long <- reshape2::melt(WBdata[WBdata$indicator == col,-3], id.vars=c("iso", "country"), value.name = col)
    #condition <- sum(is.na(indic_long))/dim(WBdata_long)[1] > 0.50 # TRUE = more than the 50% of the observations are missing values
    #if (!condition) {
    #  cat("'\n")
    #  cat("'--> included\n")
    #  tobe_included <- tobe_included + 1
    WBdata_long <- WBdata_long %>% right_join(indic_long,by=c("iso"="iso","country"="country","variable"="variable"))
    #}
  }
  #print(paste("Number of variables to be included:", tobe_included))
  return(WBdata_long)
}

data_join <- function(MPIdata,WBdata,lags){
  data <- MPIdata %>% right_join(WBdata,by=c("iso"="iso","country"="country"), multiple = "all")
  data <- data %>% drop_na(year)
  data$variable <- as.numeric(as.character(data$variable))
  data <- data[data$year >= data$variable,]
  data <- data[data$year - data$variable <= lags,]
  return(data)
}

data_join <- function(MPIdata,WBdata,lags){
  data <- MPIdata %>% right_join(WBdata,by=c("iso"="iso","country"="country"), multiple = "all")
  data <- data %>% drop_na(year)
  data$variable <- as.numeric(as.character(data$variable))
  data <- data[data$year >= data$variable,]
  data <- data[data$year - data$variable <= lags,]
  return(data)
}

data_join2 <- function(MPIdata,WBdata,lags){
  data <- MPIdata %>% right_join(WBdata,by=c("iso"="iso","country"="country"), multiple = "all")
  data <- data %>% drop_na(year)
  data$variable <- as.numeric(as.character(data$variable))
  #data <- data[data$year >= data$variable,]
  #data <- data[data$year - data$variable <= lags,]
  return(data)
}

prevrows <- function(data,n) {
  sapply(1:n,function(x) c(rep(NA,x), utils::head(data,-x)))
}

prevrows2 <- function(data,n){
  if (length(data) >= 10){
    sapply(1:n,function(x) c(rep(NA,x),utils::head(data,-x)))
  } else {
    cbind(sapply(1:length(data),function(x) c(rep(NA,x),utils::head(data,-x))),
          matrix(NA,nrow = length(data),ncol= n - length(data)))}
}

#' @importFrom dplyr right_join
#' @importFrom tidyr drop_na
#' @importFrom magrittr %>%
cleaned_data <- function(joined_data,lags){
  topic_data <- joined_data[, 1:7]
  ids <- as.factor(paste0(joined_data$iso,as.character(joined_data$year)))
  topic_data$id <- ids
  topic_data <- topic_data[order(topic_data$id),]
  joined_data$id <- ids
  joined_data <- joined_data[order(joined_data$id),]
  for (j in 8:(ncol(joined_data)-1)){
    result <- tapply(joined_data[,j],joined_data$id,prevrows,lags)
    temp<- data.frame(do.call(rbind,result))
    rownames(temp) <- NULL
    colname <- colnames(joined_data)[j]
    var_colnames <- as.matrix(NA,nrow=1,ncol=lags)
    for (i in 1:lags){
      var_colnames[i] <- paste0(colname," t - ", i)
    }
    colnames(temp) <- var_colnames
    topic_data <- cbind(topic_data, temp)
  }
  topic_data <- topic_data[(topic_data$year == topic_data$variable),-7]
  rownames(topic_data) <- NULL
  return(topic_data)
}

valid_url <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}
