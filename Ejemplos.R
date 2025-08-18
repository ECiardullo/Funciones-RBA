suma_cuadrados <- function(a,b) {
  resultado <- a^2 + b^2
  return(resultado)
}

suma_cuadrados()

suma_cuadrados <- function(a = 0,b = 0) {
  resultado <- a^2 + b^2
  return(resultado)
}

suma_cuadrados()

a = 4

suma_cuadrados <- function(b = 0) {
  resultado <- a^2 + b^2
  return(resultado)
}

suma_cuadrados()

#

a = 4

suma_cuadrados <- function() {
  a = a - 4
  b <- 2
  resultado <- a^2 + b^2
  return(resultado)
}

suma_cuadrados()

2 -> z

z<<-a

suma2 <- function(a = 2, b){
  
  if(missing(b)) b = a
  
  resultado <- a + b 
  return(resultado)
}

suma2()

b = 3

suma2()


suma2 <- function(a, b, ...){
  
  resultado <- sum(c(a, b), ... ) 
  
  return(resultado)
}



suma2(a = 2, b = NA, na.rm = T)


suma2 <- function(a, b){
  
  resultado <- sum(c(a, b), ... ) 
  
  return(resultado)
}



g11 <- function() {
  if (!exists("a")) {
    a <<- 1
  } else {
    a <<- a + 1
  }
  a
}

g11()
g11()

