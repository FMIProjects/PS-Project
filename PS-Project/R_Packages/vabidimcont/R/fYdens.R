fYdens <- function(fXY,y,lowX=-10^5,highX=10^5){

  #calculam integrala dupa X din fXY cu captele egale cu domeniul lui X si y este fixat
  valIntegrala <- integrate(fXY,lowX,highX,y=y)$value

  return(valIntegrala)

}
