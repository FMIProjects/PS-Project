frepcomgen <- function(n,m) {

  # Crearea unui tabel gol cu n+2 randuri și m+2 coloane
  # Se extinde deoarece avem nevoie si de prob. v.a. X si Y
  tabel <- matrix(NA, nrow = n+2, ncol = m+2)

  max_nr <- max(n,m)

  # Populare valori pentru v.a X
  tabel[2:(n+1),1] <- sort(sample(c(-max_nr:max_nr), n))


  # Populare valori pentru v.a Y
  tabel[1, 2:(m+1)] <- sort(sample(c(-max_nr:max_nr), m))


  return(tabel)
}
