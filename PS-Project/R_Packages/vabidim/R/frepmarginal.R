#frepmarginal
frepmarginal <- function(tabel) {
  # determinare n si m
  n <- nrow(tabel) - 2
  m <- ncol(tabel) - 2

  # se obtin valorile
  valoriY <- tabel[1,2:(m+1)]
  valoriX <- tabel[2:(n+1),1]

  # se obtin probabilitatile
  probY <- tabel[n+2,2:(m+1)]
  probX <- tabel[2:(n+1),m+2]

  marginalaX <- matrix(c(valoriX,probX),nrow = 2, byrow = TRUE)
  marginalaY <- matrix(c(valoriY,probY),nrow = 2, byrow = TRUE)

  marginale <- list(marginalaX,marginalaY)

  return(marginale)
}
