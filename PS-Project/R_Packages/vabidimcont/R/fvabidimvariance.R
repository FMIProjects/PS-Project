fvabdimvariance <- function(fdens,fmedie,lowX=-10^5,highX=10^5,lowY=-10^5,highY=10^5){

  if(is.infinite(lowX)){
    lowX <-  -10^5
  }

  if(is.infinite(lowY)){
    lowY <-  -10^5
  }

  if(is.infinite(highX)){
    highX <-  10^5
  }

  if(is.infinite(highY)){
    highY <-  10^5
  }


  fmediePatrat <- function(x,y){

    return(fmedie(x,y) ^2)
  }

  valoareMediePatrat <-fvabidimexpectedvalue(fdens,fmedie,lowX,highX,lowY,highY)^2

  valoareMedieDePatrat <- fvabidimexpectedvalue(fdens,fmediePatrat,lowX,highX,lowY,highY)

  return(valoareMedieDePatrat - valoareMediePatrat)


}
