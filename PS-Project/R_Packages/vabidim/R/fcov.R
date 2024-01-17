
fcov <-  function(X,Y){

  XY <- fvaop(X,Y,'*')

  return(fexpectedvalue(XY) - fexpectedvalue(X)*fexpectedvalue(Y))

}
