fexpectedvalue <- function(X)
{
  # E(c) = c
  if(is.numeric(X) && length(X) == 1)
      return(X)

  medie <- sum(X[1,] * X[2,])

  return(medie)
}
