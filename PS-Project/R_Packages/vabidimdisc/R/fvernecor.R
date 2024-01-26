fvernecor <-  function(X,Y,repCom){

  # doua v a sunt necorelate daca covarianta lor este 0
  covarianta <- fcov(X,Y,repCom=repCom)

  if(covarianta == 0)
    return(TRUE)

  return (FALSE)

}
