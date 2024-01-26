#fpropcov
fpropcov <- function(X,Y,repCom,a,b,c,d) {

  value <- a*c * fvariance(X) + (a*d+b*c) * fcov(X,Y,repCom) + b*d * fvariance(Y)

  return( value )

}
