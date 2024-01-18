# fPcond
fPcond <- function(X,Y,repCom) {

  #se extrag marimile matricei

  n <- length(repCom[,1])-2
  m <- length(repCom[1,])-2

  # luam fiecare coloana adica fiecare Y=y si impartim valorile (adica P(X=x, Y=y)) la P(Y=y)
  for(j in (2:(m+1))){
    repCom[(2:(n+1)),j] = repCom[(2:(n+1)),j] / repCom[n+2,j]
  }


  # scapam de ultima linie si coloana care reprezinta probabilitatile comune
  repCom <- repCom[-n-2,-m-2]

  # facem transpusa pentru a muta valorile lui X pe prima linie
  repCom <- t(repCom)



  # returnam matricea
  return(repCom)





}
