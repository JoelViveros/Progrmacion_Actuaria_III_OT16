  mejor<-function(estado, resultado) {
    #LEER DATOS 
         archivo <- read.csv("outcome-of-care-measures.csv")
    
    #REVISION DE VALIDEZ 
         #UBICACION COLUMNA ESTADO
    state <- levels(archivo[, 7])[archivo[, 7]] 
          codes <- FALSE
     
        for  (a in 1:length(state)) {
             if (estado == state[a]) {
                codes <- TRUE
            }
         }
    
           if (!codes) {
             stop ("Estado Inv�lido")
          }
    
         if (!((resultado == "ataque") | (resultado == "falla")
               | (resultado == "neumonia"))) {
                stop ("Resultado Inv�lido")
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
    
                 nombre <- archivo[grep(estado, archivo$State), ]#La funcion grep permite la  b�squeda de un determinado patr�n en cada elemento de un vector x
                     empate <- nombre[order(nombre[, columna], nombre[, 2], na.last = NA), ]
                            empate[1, 2]
                  }