ImputacionKnn_Dummies <- function(x, 
                                  q=NULL, 
                                  w=NULL, 
                                  lambda=NULL, 
                                  knn=NULL, 
                                  kernel ="gaussian", 
                                  print.all=T){
  
  #### Librerias
  if(require(CVTuningCov)==T){require(CVTuningCov)} else{stop("Falta el paquete 'CVTuningCov'")}
  if(require(fastDummies)==T){require(fastDummies)} else{stop("Falta el paquete 'fastDummies'")}
  if(require(parallel)==T){require(parallel)} else{stop("Falta el paquete 'fastDummies'")}
  if(require(MASS)==T){require(MASS)} else{stop("Falta el paquete 'MASS'")}
  
  #### Genera las dummies asociadas a las variables categoricas combinadas en una unica matriz
  dumificacion_matrix <- function(x){
    dummies <- matrix(999, nrow(x), 1)
    for(i in 1:ncol(x)){
      TMP <- dummy_columns(x[,i])
      dummies <- cbind(dummies, TMP[,-c(1, grep("_NA", colnames(TMP)))])
    }
    return(dummies[,2:ncol(dummies)])
  }
  
  #### Genera una lista con dummies para cada una de las variables categoricas
  dumificacion <- function(x){
    dummies <- dummy_columns(x)
    return(dummies[,-c(1, grep("_NA", colnames(dummies)))])
  }
  
  
  ########################## Imputacion ##########################
  z = x
  
  if(is.null(q)){q=2    #Determina la distancia de Minkowski a utilizar
  if(print.all){print("Se utiliza la distancia Euclidea para el calculo de DcatSel")}}   
  if(is.null(w)){w=2}  
  if(is.null(lambda)){lambda = 2}
  if(is.null(knn)){knn = 5    #Numero de vecinos a utilizar en la imputacion
  if(print.all){print("Se toman los 5 vecinos mas cercanos para la imputacion")}} 
  
  P = sum(is.na(z)) #Cantidad de datos a imputar
  if(print.all){print(paste0("Se van a imputar ", P, " Datos"))}
  
  #Genero un vector con el numero de categorias de cada variable
  z_k <- apply(z,2, function(x) length(levels(factor(x, ordered=FALSE))))
  
  #Matriz de dummies y lista de dummies
  zD_matrix <- dumificacion_matrix(z)
  zD <- apply(z,2, dumificacion)
  
  #Ordeno los nombres de zD
  for(iii in 1:ncol(z)){
    zD[[iii]] = zD[[iii]][,order(names(zD[[iii]]))]
  }
  
  
  corr = sapply(1:ncol(zD_matrix), function(jj){ sapply(1:ncol(zD_matrix), function(zz){
    cor(zD_matrix[,jj], zD_matrix[,zz], use="pairwise.complete.obs")})})
  
  #Creo indicador de variable y dummie 
  name_V <- rep(1:ncol(z), z_k)
  
  if(length(unique(z_k))==1){
    name_D <- rep(1:z_k[1], ncol(z))
  } else { 
    name_D <- unlist(sapply(1:ncol(z), function(x) 1:z_k[x])) 
  }
    corr_N <- cbind(corr, name_V, name_D)
  
  #Mientras existan perdidos en los datos...
  # while(sum(is.na(z))!=0){    
  
  indice <- which(is.na(z), T)
  
  cl <- makeCluster(3)
  
  wew <- parSapply(cl = cl, 1:nrow(indice), function(jpk){  

    require(CVTuningCov)
    require(fastDummies)
    require(MASS)
    
    #Busco una observacion missing y guardo su posicion en la base
    filaNA =  indice[jpk,1]
    s = indice[jpk,2]
    
    vec_pi = sapply(1:z_k[s], function(gg){ 
    ## Calculo de la distancia DcatSel
    dcat <- c()
    
    O <- !is.na(z) #Matriz de indicadoras de no missing
    
    alpha = O %*% O[filaNA,]
    
    
    #Replico las dummies de la fila con el NA
    rep.filaNA<- t(replicate(nrow(z), as.matrix(zD_matrix[filaNA,]), simplify = T))
    
    #Replico las correlacion entre las variables la cantidad de niveles de cada variable  
    #R = matrix(unlist( sapply(1:length(z_k), function(jj){
    #  rep(corr[s,jj], z_k[jj])
    #})),1,31) 
    
    R = corr_N[,1:length(name_D)][name_V==s & name_D==gg]
    
    filasR = t(replicate(nrow(z), R, simplify = T))
    
    #Calculo del termino dentro de la sumatoria de la distancia
    tmp = abs(rep.filaNA - zD_matrix[,])^q  * abs(filasR)^w
    
    #Calculo de la distancia
    dcat = (apply(tmp, 1, sum, na.rm=T)/alpha)^(1/q)
    
    zD_2 <- list()
    
    #Concateno las distancias a las dummies, elimino la obs a imputar y ordeno obs por distancia
    z_dist <- cbind(zD_matrix, dcat)
    z_dist <- z_dist[-filaNA,]
    z_dist <- z_dist[order(z_dist$dcat),]
    
    for(i in 1:length(zD)){
      zD_2[[i]] <- cbind(zD[[i]], dcat)
      zD_2[[i]] <- zD_2[[i]][-filaNA,]
      zD_2[[i]] <- zD_2[[i]][order(zD_2[[i]]$dcat),]
    }
    
   #Elimino los vecinos mas cercanos con NAs para el calculo de probabilidades
    # while(anyNA(zD_2[[s]][,gg])){
    #   ff <- which(is.na(zD_2[[s]][,gg]), T)
    #   zD_2[[s]] <- zD_2[[s]][-ff[1],]  
    #   
    # }
    
    #Calculo de los kernel
    u <- z_dist[,"dcat"]/lambda
    if(kernel == "triangular")  K <- ifelse(abs(u) <= 1, (1 - abs(u)), 0)
    if(kernel == "gaussian") K <- (1/sqrt(2 * 3.141593)) * exp((-1/2) * u^2)
    

    
    if(knn == "n") {
      
      #Calculo de los pesos
      pesos <- K/sum(K, na.rm = T)
      
      #Calculo de las probabilidades de cada categoria
      sum(pesos[] * zD_2[[s]][,gg], na.rm = T) 
      
    } else {
      
      #Calculo de los pesos
      pesos <- K/sum(K[1:knn], na.rm = T)
      
      #Calculo de las probabilidades de cada categoria
      sum(pesos[1:knn] * zD_2[[s]][1:knn,gg], na.rm = T)
      
    }
    })
    
    vec_pi = vec_pi/sum(vec_pi)

    #Extraigo la categoria con la mayor probabilidad
    for(i in 1:z_k[s]){
      if(vec_pi[i]==max(vec_pi)){c_imp=i}
    }
    
    c_imp
  })
  
  stopCluster(cl)
  
  z[indice] = wew
  
  return(z)
}
