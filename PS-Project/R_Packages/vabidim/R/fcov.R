fcov <-  function(X,Y,repCom){

  XY <- fvaop(X,Y,repCom,'*')

  return(fexpectedvalue(XY) - fexpectedvalue(X)*fexpectedvalue(Y))

}
