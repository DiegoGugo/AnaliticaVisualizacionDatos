promedio <- function(data){
  sum <- 0
  for (i in data) {
    sum = sum + i
  }
  
  return(sum/length(data))
}

correlation <- function(longitud, ancho){
  x_mean <- promedio(longitud)
  y_mean <- promedio(ancho)
  
  s_xy <- 0
  for (i in length(longitud)) {
    s_xy <- s_xy + (longitud[i] - x_mean)*(ancho[i]-y_mean)
  }
  
  s_xx <- 0
  for (i in length(longitud)) {
    s_xx <- s_xx + (longitud[i] - x_mean)**2
  }
  
  s_yy <- 0
  for (i in length(longitud)) {
    s_yy <- s_yy + (ancho[i] - y_mean)**2
  }
  
  B1 <- s_xy / s_xx
  B0 <- y_mean - B1*x_mean
  
  cat("y = B_0  + B_1*x\n")
  cat("y=",B0, "+ (", B1,")(x)\n")
  
  r <- s_xy/sqrt(s_xx*s_yy)
  
  cat("r", r, "\n")
  
}

correlation(iris$Petal.Length, iris$Petal.Width)