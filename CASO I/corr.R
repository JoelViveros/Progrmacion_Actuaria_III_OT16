corr <- function(directorio, horizonte = 0) {
  source("completos.R")
  #Con source abrimos una sesión de R y ejecutamos todo el código dentro un archivo determinado
  tabla <- completos(directorio)
  ids <- tabla[tabla["nobs"] > horizonte, ]$id #Aqui realizamos la extraccion
  correr <- numeric()
  
  for(a in ids) {
    casos <- read.csv(paste(directorio, "/", formatC(a, width = 3, flag = "0"), 
                               ".csv", sep = "")) 
    comp<- casos[complete.cases(casos), ] 
    correr <- c(correr, cor(comp$sulfate, comp$nitrate)) 
    #funcion cor es la correlacion entre las variables
  }
  return(correr)
}
