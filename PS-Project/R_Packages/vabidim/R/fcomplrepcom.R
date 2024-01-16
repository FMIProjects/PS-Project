# fcomplrepcom
fcomplrepcom <- function(n,m,tabel) {


  # luam prima data probabilitatile si le aflam suma
  p <- sum(tabel[2:(n+1),m+2],na.rm=TRUE)
  q <- sum(tabel[n+2,2:(m+1)],na.rm=TRUE)

  #aflam coordonatele fiecarei probabilitati +1 prentu ca indicele returnat de which este raportat la semi-matricea data
  coord_p <- which(is.na(tabel[2:(n+1),m+2]))+1
  coord_q <-which(is.na(tabel[n+2,2:(m+1)]))+1

  # completam probabilitatile
  tabel[coord_p,m+2] = 1 - p
  tabel[n+2,coord_q] = 1 - q

  #parcurgem liniile de pi-uri
  for (i in (2:n+1)){

    # se calculeaza suma de pe linia respectiva
    sumPi <- sum(tabel[i, 2:(m+1)],na.rm=TRUE)

    #obtinem coordonatele celulei libere
    coord_pi <- which(is.na(tabel[i, 2:(m+1)])) +1

    # completam celula
    tabel[i, coord_pi] = tabel[i,m+2] - sumPi

  }


return(tabel)
}

