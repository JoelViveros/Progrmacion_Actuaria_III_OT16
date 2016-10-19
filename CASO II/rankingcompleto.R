rankingcompleto <- function( resultado, num = "mejor") {
    #LEER DATOS 
        archivo <- read.csv("outcome-of-care-measures.csv")
    
    #REVISION DE VALIDEZ 
    
    
       if (!((resultado == "ataque") | (resultado == "falla")
             | (resultado == "neumonia"))) {
            stop ("Resultado Inválido")
          }
    
    #REGRESA NOMBRE DEL HOSPITAL 
         columna <- if (resultado == "ataque") {
              11 
              } else if (resultado == "falla") {
                17 
                } else {
                      23 
                      } #Los numeros despues de cada if, hacen referencia al # de columana de cada padecimiento
    
      archivo[, columna] <- suppressWarnings(as.numeric(levels(archivo[, columna])[archivo[, columna]]))
          #HOSPITALES   
              archivo[, 2] <- as.character(archivo[, 2]) 
    
    
          hospitales <- vector()
              estados <- levels(archivo[,7])
    
    for (r in 1:length(estados)){
    
           nombre <- archivo[grep(estados[r], archivo$State), ]#La funcion grep permite la  búsqueda de un determinado patrón en cada elemento de un vector x
               empate <- nombre[order(nombre[, columna], nombre[, 2], na.last = NA), ]
    
                   nomhospital <- if(num == "mejor") {
                        empate[1, 2]
                            } else if(num == "peor") {
                                empate[nrow(empate), 2]
                                    } else{
                                        empate[num, 2]
                  }
    
            hospitales <- append(hospitales, c(nomhospital, estados[r])) 
    
    
    }
    
    #REGRESA UN DATA FRAME CON EL NOMBRE DEL HOSPITAL Y LA ABREVIATURA del estado al que pertenecen
 
    hospitales <- as.data.frame(matrix(hospitales, length(estados), 2, byrow = TRUE))
            colnames(hospitales) <- c("hospital", "state")
                rownames(hospitales) <- estados
    
               hospitales
    }

    
    