#frepmarginal
frepmarginal <- function(n,m,tabel) {
  # se obtin valorile
  valoriY <- tabel[1,2:(m+1)]
  valoriX <- tabel[2:(n+1),1]

  # se obtin probabilitatile
  probY <- tabel[n+2,2:(m+1)]
  probX <- tabel[2:(n+1),m+2]

  marginale <- matrix(c(valoriY,probY,valoriX,probX),nrow = 4, byrow = TRUE)

  return(marginale)
}
