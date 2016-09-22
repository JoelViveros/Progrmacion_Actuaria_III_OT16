mediacontaminante <- function(directorio, contaminante, id = 1:332) {
   nom<-numeric()
  
  for(a in id) {
    ubicacion<- read.csv(paste(directorio, "/", formatC(a, width = 3, flag = "0"), 
                               ".csv", sep = ""))
    
    #Read.cvs nos permite leer este tipo de terminacion y paste permite concatenar 
    #el nombre del archivo specdata/221.cvs
    
    nom <-c(nom, ubicacion[[contaminante]])
    #Este vector indica el nombre del contaminante 
  }
  return(mean(nom, na.rm = TRUE)) #EL comando returm permite mostrar el resultado de la media
   #mientras mean es la funcion promedio y na.rm omite los vacios
}

#NOTA: EL DIRECTORIO DE TRABAJO ES "SPECDATA", EJEMPLO mediacontaminante("specdata", "nitrate", 4)