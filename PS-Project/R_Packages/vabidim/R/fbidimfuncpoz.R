fbidimfuncpoz <- function(f,lowX=-10^8,highX=10^8,lowY=-10^8,highY=10^8,precision = 10^4){

  # generam precision valori din intervalele date pentru x si y
  valori_x <- seq(from = lowX, to = highX , length.out = precision)
  valori_y <- seq(from = lowY, to = highY , length.out = precision)

  # Facem o matrice cu toate valorile functiei f apelate pe toate perechile doua cate doua generate pt x si y
  valori_f <- outer(x_values, y_values, Vectorize(f))


  # returnam true daca toate sunt pozitive sau false altfel

  return(all(valori_f>=0))
}
