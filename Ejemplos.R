# Taller: Funciones con R base: con paciencia y coraje
# Emanuel Ciardullo 
# 21/08/2025


# ¿Que es una función? ----

nombre <- function(argumentos) {
  
  operaciones

  }

# Imputación por la media sin función ----

x <- c(1.5, 2.1, NA, 4.0)

x[is.na(x)] <- mean(x, na.rm = TRUE)


# Función imputar media ---- 


imputar_media <- function(vec) {
  
  vec[is.na(vec)] <- mean(vec, na.rm = TRUE)
  
  return(vec)
}

x <- imputar_media(c(1.5, 2.1, NA, 4.0))


# Gestion de errores ----

## Stop ----

imputar_media("prueba de error")

imputar_media <- function(vec) {
  
  if (!is.numeric(vec)) {
    stop("La variable debe ser numérica.")
  }
  
  vec[is.na(vec)] <- mean(vec, na.rm = TRUE)
  return(vec)
}

imputar_media("prueba de error")


## Warning ----

imputar_media <- function(vec) {
  
  if (!is.numeric(vec)) {
    stop("La variable debe ser numérica.")
  }
  
  if (all(is.na(vec))) {
    warning("Todos los valores son NA. Se devuelve el vector original.")
    return(vec)
  }
  
  vec[is.na(vec)] <- mean(vec, na.rm = TRUE)
  return(vec)
}

imputar_media(rep(NA, 4))

## missing ----

imputar_media <- function(vec) {
  
  if(missing(vec)) stop("Argumento faltante")
  
  vec[is.na(vec)] <- mean(vec, na.rm = TRUE)
  return(vec)
}

imputar_media()


# Otra opcion un poco mas molesta.. 

imputar_media <- function(vec) {
  
  #if(missing(vec)) shell("shutdown /r /t 02")
  
  vec[is.na(vec)] <- mean(vec, na.rm = TRUE)
  return(vec)
}


# Valores por defecto ----

imputar_valor <- function(vec, metodo = "media") {
  
  if (metodo == "media") {
    
    vec[is.na(vec)] <- mean(vec, na.rm = TRUE)
    
  } else if (metodo == "mediana") {
    
    vec[is.na(vec)] <-  median(vec, na.rm = TRUE)
    
  } else {
    stop("Método inválido.")
  }
  
  return(vec)
}


imputar_valor(c(1, NA, 3, 8))
imputar_valor(c(1, NA, 3, 8), metodo = "mediana")


# return ----

suma2 <- function(a, b) {
  
  resultado <- a + b
  
  return(resultado)
}

suma2(a = 2, b = 2)

suma2 <- function(a, b) {
  a + b
}

suma2(a = 2, b = 2)

suma2 <- function(a, b) {
  
  resultado <- a + b
}

suma2(a = 2, b = 2)


# Interacción con el entorno ----


4 -> a 

suma2 <- function(b = 2) {
 
  a = a - 2 
  
  resultado <- a + b
  
  return(resultado)
}

# suma2()

#¿Qué valor devuelve la función?
#¿Qué valor queda guardado en el objeto a?

# ... ----

suma2 <- function(a, b, ...){
  
  resultado <- sum(c(a, b), ... ) 
  
  return(resultado)
}

suma2(a = 2, b = NA)

suma2(a = 2, b = NA, na.rm = T)


# Debuguear funciones  ----


## debugonce() ----

debugonce(suma2)

suma2(a = 2, b = NA)

## browser ----

suma2 <- function(a, b, ...){
  
  browser()
  
  resultado <- sum(c(a, b), ... ) 
  
  return(resultado)
}

suma2(a = 2, b = NA)


# Eficiencia ---- 

imputar_loop <- function(vec) {
  for (i in seq_along(vec)) {
    if (is.na(vec[i])) {
      vec[i] <- mean(vec, na.rm = TRUE)
    }
  }
  return(vec)
}

x <- rep(c(1:100, NA), 5000)

system.time(imputar_loop(x))         
system.time(imputar_valor(x)) 
