funarop <- function(X,nr,op,sort=TRUE)
{
  if(op == "+")
  {
    X[1,] <- X[1,] + nr
  }

  if(op == "-")
  {
    X[1,] <- X[1,] - nr
  }

  if(op == "*")
  {
    if(nr == 0)
    {
      X <- matrix(0, nrow = 2, ncol = 1)
      X[2,1] <- 1
    }
    else
      X[1,] <- X[1,] * nr
  }

  if(op == "/")
  {
    X[1,] <- X[1,] / nr
  }

  if(op == "^")
  {
    X[1,] <- X[1,] ^ nr

    # determinare valori unice
    valori_unice <- unique(X[1, ])

    # matrice merged
    merge_X <- matrix(0, nrow = 2, ncol = length(valori_unice))
    # punem valorile unice pe prima linie
    merge_X[1,] <- valori_unice

    # se calculeaza suma probabilitatilor pentru fiecare valoare unica
    for (i in seq_along(valori_unice)) {
      coloane <- which(X[1, ] == valori_unice[i])
      merge_X[2, i] <- sum(X[2, coloane])
    }

    X <- merge_X
  }

  if(length(X) != 2 && sort==TRUE)
  {
    #sortare
    indici_sortati <- order(X[1, ])
    X <- X[, indici_sortati]
  }

  return(X)
}
