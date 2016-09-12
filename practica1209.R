inferior <- 0
superior <- 0
for (i in 1:100) {
z <- 5
caminata <- vector("numeric")
while(z>=3 && z<=10){
  print(z)
  caminata <- c(caminata,z)
  moneda <- rbinom(1,1,0.5)
  if(moneda==1){ #caminanata aleatoria
    z <- z+1  
  } else { 
    z <- z-1
  }
}

if (z>10) {superior <- (superior+1)} 
else {inferior <- (inferior+1)}

}
superior
inferior