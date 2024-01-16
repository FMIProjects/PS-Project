frepcomgen <- function(n,m) {

  # Crearea unui tabel gol cu n+2 randuri și m+2 coloane
  # Se extinde deoarece avem nevoie si de prob. v.a. X si Y
  tabel <- matrix(NA, nrow = n+2, ncol = m+2)

  #restul colturilor se fac -1 pentru a nu le lasa NA
  tabel[1,1] <- -1
  tabel [1,m+2] <- -1
  tabel [n+2,1] <- -1
  # coltul din dreapta jos este 1 reprezentand probabilitatea
  tabel[n+2,m+2] <- 1

  max_nr <- max(n,m)

  # Populare valori pentru v.a X
  tabel[2:(n+1),1] <- sort(sample(c(-max_nr:max_nr), n)) * 1.0


  # Populare valori pentru v.a Y
  tabel[1, 2:(m+1)] <- sort(sample(c(-max_nr:max_nr), m)) * 1.0

  # Generare probabilitati + pi-uri

  # generare p
  # n valori aleatoare din distributia uniforma pe intervalul [0,1].
  valori_p <- runif(n)
  # normalizam valorile astfel incat suma sa fie 1
  valori_p <- valori_p / sum(valori_p)

  tabel[2:(n+1),m+2] <- valori_p

  # generare pi-uri

  for (i in 2:(n+1))
  {
    # m valori aleatoare din distributia uniforma pe intervalul [0,1].
    valori_pi <- runif(m)

    # normalizam valorile astfel incat suma sa fie 1
    valori_pi <- valori_pi / sum(valori_pi)

    # inmultimim cu suma dorita
    valori_pi <- valori_pi * tabel[i,m+2]

    tabel[i,2:(m+1)] <- valori_pi
  }

  # generare q
  for (j in 2:(m+1))
  {
    tabel[n+2,j] <- sum(tabel[2:(n+1),j])
  }

  # Generare spatii goale

    indici_i <- sample(2:(n + 2), size = min(m,n), replace = FALSE)
    indici_j <- sample(2:(m + 2), size = min(m,n), replace = FALSE)



    for (i in 1:min(m,n))
    {
      repeat{
      x <- sample(indici_i,1)
      y <- sample(indici_j,1)

      # checker sa nu fie t[n+2][m+2] sau t[1][1]
      if((x != n+2 || y !=m+2) && (x != 1 && y!= 1))
          break
      }

      #testam cazul in care indicii_i si indici_j raman cu un singur element

      if(i==min(n,m)){
        x= indici_i[1]
        y= indici_j[1]

        # daca se intampla ca in cei 2 vectori sa ramana indicii n+2 si m+2 ignoram cazul
        if(x == n+2 && y == m+2)
          break;

      }




      # marcam celula goala
      tabel[x,y] <- NA

      # eliniminam indicii folositi
      indici_i <- setdiff(indici_i, x)
      indici_j <- setdiff(indici_j, y)


    }

  return(tabel)
}
