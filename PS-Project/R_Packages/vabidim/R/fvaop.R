fvaop <- function(X,Y,op){


  # o sa construim pe parcurs valorile si probabilitatile pentru noua v.a.
  valXY <- vector("numeric" , length =0)
  probXY <- vector("numeric", length=0)

  # variabile care o sa ne ajute
  valX <- X[1,]
  valY <-  Y[1,]

  probX <- X[2,]
  probY <- Y[2,]

  if(op != '+' && op != '-' && op!= '*')
    return(-1)

  # vom folosi operatia pe fiecare valoare a lui X cu fiecare valoare a lui Y
  for(i in  valX){

    # obtinem probabilitatea asociata valorii curente a lui X
    currentProbX <- probX[which(valX==i)]

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


    # concatenam noile probabilitati
    probXY <- c(probXY, (currentProbX * probY) )


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
