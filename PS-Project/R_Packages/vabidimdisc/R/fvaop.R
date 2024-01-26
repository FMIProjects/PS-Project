fvaop <- function(X,Y,repartitieComuna,op){

  #se va lucra pe cazul in care valorile lui X sunt situate pe prima coloana

  n <- length(repartitieComuna[,1])-2
  m <- length(repartitieComuna[1,])-2

  # daca valorile lui X sunt pe prima linie atunci transpunem matricea
  if(identical(X[1,] , repartitieComuna[1,2:(m+1)]) && identical(X[2,],repartitieComuna[n+2,2:(m+1)]))
    repartitieComuna <- t(repartitieComuna)


  # nx numarul de linii din repCom ny numarul de coloane din repCom
  nX <- length(X[1,])
  nY <- length(Y[1,])

  # o sa construim pe parcurs valorile si probabilitatile pentru noua v.a.
  valXY <- vector("numeric" , length =0)
  probXY <- vector("numeric", length=0)

  # variabile care o sa ne ajute
  valX <- X[1,]
  valY <-  Y[1,]


  if(op != '+' && op != '-' && op!= '*')
    return(-1)

  # vom folosi operatia pe fiecare valoare a lui X cu fiecare valoare a lui Y
  for(i in  valX){

    # obtinem probabilitatea asociata valorii curente a lui X
    indexProbX <- which(valX==i)

    # concatenam noile valori la vectorul de valori in functie de operatie
    if(op == "+"){
      valXY <- c(valXY,(valY+i))
    }
    if(op == "-"){
      valXY <- c(valXY,(i-valY))
    }
    if(op == "*"){
      valXY <- c(valXY,(valY*i))
    }

    # din repartitia comuna obtinem probabilitatile de pe randul cu X=i
    # grija la indici
    probX <-repartitieComuna[indexProbX+1,2:(nY+1)]

    # concatenam noile probabilitati
    probXY <- c(probXY, probX  )


  }


  # transformam vectorii in matrice
  tabelXY <- rbind(valXY,probXY)

  # determinare valori unice
  valori_unice <- unique(tabelXY[1, ])

  # matrice merged
  merge_XY <- matrix(0, nrow = 2, ncol = length(valori_unice))
  # punem valorile unice pe prima linie
  merge_XY[1,] <- valori_unice

  # se calculeaza suma probabilitatilor pentru fiecare valoare unica
  for (i in seq_along(valori_unice)) {
    coloane <- which(tabelXY[1, ] == valori_unice[i])
    merge_XY[2, i] <- sum(tabelXY[2, coloane])
  }

  tabelXY <- merge_XY

  if(length(tabelXY) != 2)
  {
    #sortare
    indici_sortati <- order(tabelXY[1, ])
    tabelXY <- tabelXY[, indici_sortati]
  }

  return(tabelXY)

}
