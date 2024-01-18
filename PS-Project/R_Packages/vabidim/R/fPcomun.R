#fPcomun
fPcomun <- function(repCom=NULL,X,Y=NULL,lowX=-Inf,highX=Inf,lowY=-Inf,highY=Inf) {

  #test pentru buna functionare

  if(identical(repCom,X) && !is.null(Y))
    stop("Repartitia comuna nu poate sa fie X daca Y e specificat")

  #caz in care vrem sa facem inegalitati doar pe X deci repCom e redundant

  if(is.null(Y)){

    # se extrag indexii
    indexes <-which(X[1,]> lowX & X[1,] < highY)

    # daca boundu urile sunt egale inseamna ca vrem P(X=lowX)
    if(lowX==highX){
      indexes <- which(X[1,] == lowX)
      return(X[2,indexes])
    }

    #returnam suma probabilitatilor
    return(sum(X[2,indexes]) )

  }

  return(0)
}
