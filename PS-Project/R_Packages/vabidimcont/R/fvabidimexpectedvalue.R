fvabidimexpectedvalue <- function(fdens,fmedie,lowX=-10^5,highX=10^5,lowY=-10^5,highY=10^5){

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


  valoareMedie <- integral2(function(x, y) fdens(x, y) * fmedie(x, y),lowX,highX,lowY,highY)$Q

  return(valoareMedie)


}



