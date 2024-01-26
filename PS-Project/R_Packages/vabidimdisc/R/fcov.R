fcov <-  function(X,Y,repCom){

  if(identical(X,Y))
    return(0);

  XY <- fvaop(X,Y,repCom,'*')

  return(fexpectedvalue(XY) - fexpectedvalue(X)*fexpectedvalue(Y))

}



