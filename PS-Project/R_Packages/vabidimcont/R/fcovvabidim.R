fcovvabidim <- function(fdens,fX,fY,lowX=-10^5,highX=10^5,lowY=-10^5,highY=10^5){


  fXY <- function(x,y){
    return (fX(x,y)*fY(x,y))
  }



  exy <- fvabidimexpectedvalue(fdens,fXY,lowX,highX,lowY,highY)
  ex <- fvabidimexpectedvalue(fdens,fX,lowX,highX,lowY,highY)
  ey <- fvabidimexpectedvalue(fdens,fY,lowX,highX,lowY,highY)

  return (exy - ex* ey)
}
