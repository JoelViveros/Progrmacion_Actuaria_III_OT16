completos <- function(directorio, id = 1:332) {
  nobs <- numeric()
  for(a in id) {
    casos<- read.csv(paste(directorio, "/", formatC(a, width = 3, flag = "0"), 
                              ".csv", sep = "")) 
    nobs <-c(nobs, sum(complete.cases(casos))) 
    #La funcion complete.cases devuelve un vector lógico 
    #que se indique los casos están completos, es decir, no tienen valores que faltan
    #Sum es la funcion suma
  }
  tabla<- data.frame(id, nobs)
  tabla
}


