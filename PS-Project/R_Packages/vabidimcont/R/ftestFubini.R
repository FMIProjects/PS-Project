ftestFubini <-function(f,lowX,highY,lowY,highX){
  # verificam limitele

  if (!all(is.finite(c(lowX, highX, lowY, highY)))) {
    return (FALSE)
  }

  return (TRUE)
}
