fbidimfuncpoz <- function(f,lowX=-10^5,highX=10^5,lowY=-10^5,highY=10^5,precision = 10^4){

  if(is.infinite(lowX)){
    lowX <- -10^5
  }


  if(is.infinite(lowY)){
    lowy <- -10^5
  }


  if(is.infinite(highX)){
    highX <- 10^5
  }


  if(is.infinite(highY)){
    highY <- 10^5
  }

  # generam precision valori din intervalele date pentru x si y
  valori_x <- seq(from = lowX, to = highX , length.out = precision)
  valori_y <- seq(from = lowY, to = highY , length.out = precision)

  # Facem o matrice cu toate valorile functiei f apelate pe toate perechile doua cate doua generate pt x si y
  valori_f <- outer(x_values, y_values, Vectorize(f))


  # returnam true daca toate sunt pozitive sau false altfel

  return(all(valori_f>=0))
}
