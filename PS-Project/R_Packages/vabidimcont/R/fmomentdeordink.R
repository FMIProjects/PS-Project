fmomentdeordink <- function(fdens,fmedie,lowX=-10^5,highX=10^5,lowY=-10^5,highY=10^5,order,initial=FALSE){

if(order == 1)
  return(0)


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


  medie <- fvabidimexpectedvalue(fdens,fmedie,lowX,highX,lowY,highY)

  if(initial)
    medie <- 0

  fmedieordin <- function(x,y){

    return((fmedie(x,y) - medie)^order)

  }

  medie <- fvabidimexpectedvalue(fdens,fmedieordin,lowX,highX,lowY,highY)

  return(medie)


}
