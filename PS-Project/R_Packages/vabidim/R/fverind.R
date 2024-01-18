fverind <-  function(X,Y,repCom){


  #se va lucra pe cazul in care valorile lui X sunt situate pe prima coloana

  n <- length(repCom[,1])-2
  m <- length(repCom[1,])-2


  # daca valorile lui X sunt pe prima linie atunci transpunem matricea
  if(identical(X[1,] , repCom[1,2:(m+1)]) && identical(X[2,],repCom[n+2,2:(m+1)])){

    repCom <- t(repCom)
    n <- length(repCom[,1])-2
    m <- length(repCom[1,])-2

  }




  probY <- repCom[n+2,2:(m+1)]

  #Stim ca daca P(X=x,Y=y) = P(X=x)P(Y=y) atunci v.a. X si Y sunt independente oricare ar fi x si y rin R
  #Deci P(Y=y) = P(X=x,Y=y)/P(X=x) oricare ar fi x si y din R

  #pentru fiecare rand impartim probabilitatile comune la
  #probabilitatea marginala si verificam daca noul vector de
  #valori este acelasi ca probabilitatile lui Y

  for(i in 2:(n+1)){

    randX <- repCom[i,(2:m+1)]
    probXi <-  repCom[i,m+2]

    if(identical(randX/probXi,probY) == FALSE){
      return(FALSE)
    }

  }

  return (TRUE)

}
