### Processing data util files ###

# Data Processing ------------------------------------------------------

data_processing <- function(df, num){

  withNAs <- selectrawdfs(df, num)

  # define remove NAs functions
  removeNAs <- function(df){
    datafr <- df[ , apply(df, 2, function(x) !any(is.na(x)))]
    return(datafr)
  }

  datafr <- removeNAs(withNAs)

  return(datafr)
}

### data_processing() dependencies

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
    datafr <- Reduce(generate.datasets, 1:(num-3), init = data) #repito (num-3) la funcion para ir generando los dfs

  }

  return(datafr)
}


###### selectrawdfs() dependencies

data3 <- function(df){
  data <- df[(duplicated(df$country)|duplicated(df$country, fromLast=TRUE)),]

  datasacandopaises = function(df, paises, paisadc = NULL){
    if (is.null(paisadc) == FALSE){
      paises = c(paises, paisadc)
    }
    nuevadata = df[!(df$country %in% paises),]
    return(nuevadata)
  }

  paisesasacar <- function(df, cantdeNAs){
    respais <- resumenporpais(df)
    datapaises <- respais[respais[,6] >= cantdeNAs,]
    paises <- c( paste(datapaises[,4]))
    return(paises)
  }

  datafr <- datasacandopaises(data, paisesasacar(data, 2000))
  return(datafr)
}

generate.datasets <- function(df,times){

  for (cantNAS in 1:max(resumenporvariable(df)[,1])) {

    if(sum(is.na(df)) == 0){
      #print("Ya no hay mas Nas")
      break
    }else if (identical(paste(datossolona(df,cantNAS)$country), character(0))){
      #print(paste("Tengo que tomar variables con", cantNAS+1, "NAs"))
      next

    }else{

      paises = paste(datossolona(df,cantNAS)$country) #lista de paises con ciertos nas
      resu = resumenporpais(df)[resumenporpais(df)$pais %in% paises,] #cantidad de observaciones de la lista de paises
      obsdelpais = numeric()
      for (pais in paises) {
        obs = resu[resu$pais == pais,][,2]
        obsdelpais = c(obsdelpais,obs)
      }
      years = datossolona(df,cantNAS)$year #año de la observacion
      combi = data.frame(paises,obsdelpais, years) #combinacion de todos

      sacados = data.frame()
      for (i in 1:nrow(combi)) {

        if(combi$obsdelpais[i] == 2){
          sacar = df[(df$country %in% combi$paises[i]),] #saco el pais entero
          sacados = rbind(sacados,sacar) #saco la observacion correspondiente

        }else{
          sacar = df[(df$country == combi$paises[i] & df$year == combi$years[i] ),]
          sacados = rbind(sacados,sacar) #saco la observacion correspondiente
        }

      }
      datos = df %>% anti_join(sacados)
      # si despues de todo esto queda algun pais con 1 sola observacion, lo saco
      if(sum(!(duplicated(datos$country)|duplicated(datos$country, fromLast=TRUE))) != 0){
        datos = datos[(duplicated(datos$country)|duplicated(datos$country, fromLast=TRUE)),]
      }

      #print(paste("Se tomaron variables con", cantNAS, "NAs"))
      break
    }
  }
  return(datos)
}

######### generate.datasets() dependencies

resumenporvariable <- function(df){
  #cantidad de NAs por variable
  naporvar <- sapply(df, function(y) sum(is.na(y)))
  #ordenamos las variables por porcentaje de NAs
  res <- data.frame("nas" = naporvar )
  #ordenamos las variables por porcentaje de NAs
  resumen <- res %>% arrange(res$nas)
  return(resumen)
}

datossolona <- function(df, cantNAS){
  vars <- variablesconNAs(df,cantNAS)
  datos <- cbind(df[,1:4],df[,(colnames(df) %in% vars),])
  datosunna <- datos[!complete.cases(datos), ]
  return(datosunna)
}

resumenporpais <- function(df){
  #contamos la cantidad de años por pais
  obs <- aggregate(year ~ country, data = df, FUN = length)
  #tomamos los nas por fila
  nasporfila <- resumenporfila(df)
  #sumamos los nas por pais
  nasporpais2 <- aggregate(cantidadNAs ~ df$country, data = nasporfila, FUN = sum)[2]

  res <- cbind("pais"=obs[,1], "cant años" = obs[,2], "NAs por pais" = nasporpais2)
  #ordenamos las variables por cantidad de NAs
  varorden <- res[order(res[,3]),]
  #ponemos todo en el mismo coso
  resumen <- cbind(res, varorden[,colnames(varorden)])

  return(resumen)

}

resumenporfila <- function(df){
  nasperrow <- data.frame("cantidadNAs" = apply(df, 1, function(x) sum(is.na(x))))
  nasporfila <- cbind(df$iso, df$country, nasperrow)

  return(nasporfila)
}

variablesconNAs <- function(df, cantdeNAs){
  res <- resumenporvariable(df)
  res$names <- rownames(res)
  datavariables <- res[res[,1] == cantdeNAs,]
  variables <- paste(datavariables[,2])
  return(variables)
}
