fPvabidimcont <-  function(fdens,lowX=-10^5,highX=10^5,lowY=-10^5,highY=10^5){

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


  valoare_integrala <- integral2(f,lowX,highX,lowY,highY)$Q

  return(valoare_integrala)


}
