fcoeficientvabidim <-  function(fdens,fX,fY,lowX=-10^5,highX=10^5,lowY=-10^5,highY=10^5){



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

  cov <- fcovvabidim(fdens,fX,fY,lowX,highX,lowY,highY)

  varX <- fvabdimvariance(fdens,fX,lowX,highX,lowY,highY)
  varY <- fvabdimvariance(fdens,fY,lowX,highX,lowY,highY)

  return(cov/sqrt(varX*varY))
}
