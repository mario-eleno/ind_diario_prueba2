
# Esta función sirve para desestacionalizar las series diarias de tarjetas, electricidad y VGE.
# Para ello nos quedamos con los datos hasta la fecha "corte". Para evitar que la estacionalidad
# esté demasiado afectada por los datos de 2020, estamos poniendo 31/12/2019.
# Se estiman los términos hasta orden seis de una serie de Fourier y restamos esa componente a la serie.
# Elegimos solo hasta orden seis para quitar solo la estacionalidad que se aprecie cuando agreguemos
# por meses. Sigue quedando estacionalidad asociada a frecuencias mayores.

desest.diaria <- function(df, corte){
  
  times <- df$fecha
  
  df %>% filter(fecha <= corte) -> dfc
  
  times_c <- as.numeric(dfc$fecha)
  
  freq <- 2*pi*seq(1,6)/365
  
  Mc <- cos(times_c %*% t(c(0, freq)))
  Ms <- sin(times_c %*% t(freq))
  
  M <- cbind(Mc, Ms)
  
  c <- solve(t(M) %*% M) %*% t(M) %*% dfc$valor
  
  Mc <- cos(times %*% t(c(0, freq)))
  Ms <- sin(times %*% t(freq))
  
  M <- cbind(Mc, Ms)
  
  seas <- M[,-1] %*% c[-1]
  df$valor <- df$valor - seas
  
  return(df)
  
}  

desest_indi <- function(df, id = "all", corte = as.Date("2019-12-31"), by_var ="indicador"){
  
  todos <- unique(df[[by_var]])
  
  if (identical(id, "all")){
   
    id <- todos
     
  }
  
  df2 <- data.frame()
  
  for (i in id){
    
    df2 <- bind_rows(df2, desest.diaria(df[df[[by_var]] == i,], corte))
    
  }
  
  for (i in setdiff(todos, id)){
    
    df2 <- bind_rows(df2, df[df[[by_var]] == i,])
    
  }
    
  return(df2)
  
}
  