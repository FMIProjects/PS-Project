fXdens <- function(fXY,x,lowY=-10^5,highY=10^5){


  #calculam integrala dupa Y din fXY cu captele egale cu domeniul lui Y si x este fixat
  valIntegrala <- integrate(fXY,lowY,highY,x=x)$value

  return(valIntegrala)

}


