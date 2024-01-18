#fPcomun
fPcomun <- function(repCom=NULL,X,Y=NULL,lowX=-Inf,highX=Inf,lowY=-Inf,highY=Inf) {


  #caz in care vrem sa facem inegalitati doar pe X deci repCom e redundant

  if(is.null(Y)){

    # se extrag indexii
    indexes <-which(X[1,]> lowX & X[1,] < highX)

    # daca bound urile sunt egale inseamna ca vrem P(X=lowX)
    if(lowX==highX)
      indexes <- which(X[1,] == lowX)

    #returnam suma probabilitatilor
    return(sum(X[2,indexes]) )

  }

  #caz in care exista Y


  #se va lucra pe cazul in care valorile lui X sunt situate pe prima coloana

  n <- length(repCom[,1])-2
  m <- length(repCom[1,])-2

  # daca valorile lui X sunt pe prima linie atunci transpunem matricea
  if(identical(X[1,] , repCom[1,2:(m+1)]) && identical(X[2,],repCom[n+2,2:(m+1)])){

    repCom <- t(repCom)

    n <- length(repCom[,1])-2
    m <- length(repCom[1,])-2
  }

  indexesX <-which(X[1,]> lowX & X[1,] < highX)
  indexesY <-which(Y[1,]> lowY & Y[1,] < highY)

  if(lowX==highX)
    indexesX <- which(X[1,] == lowX)

  if(lowY==highY)
    indexesY <- which(Y[1,] == lowY)


  return(sum(repCom[indexesX+1, indexesY+1]))
}
