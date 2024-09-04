### Processing data util files ###

# Data Processing ------------------------------------------------------

data_processing <- function(df, num){

  withNAs <- selectrawdfs(df, num)

  datafr <- removeNAs(withNAs)

  return(datafr)
}

selectrawdfs <- function(df, num){
  # (1) Data original ----
  if (num == 1){
    datafr <- df
  }
  # (2) Datos menos los paises que tienen una sola observacion (años) ----
  else if (num == 2){
    datafr <- df[(duplicated(df$country)|duplicated(df$country, fromLast=TRUE)),]
  }
  # (3) Datos menos los paises que  tienen mas de 60 NAS ----
  else if (num == 3){
    datafr <- data3(df)
  }
  # (4) Datos menos los paises/obs que haga a la/s variables tener cierta cantidad de un NAs ----
  else {
    data <- data3(df)
    datafr <- Reduce(generico5, 1:(num-3), init = data) #repito (num-3) la funcion para ir generando los dfs

  }

  return(datafr)
}

removeNAs <- function(df){
  datafr <- df[ , apply(df, 2, function(x) !any(is.na(x)))]

  return(datafr)
}
