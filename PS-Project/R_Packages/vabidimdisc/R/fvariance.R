fvariance <- function(X)
{
  # Var(c) = 0
  if(is.numeric(X) && length(X) == 1)
    return(0)

  medie_x <- fexpectedvalue(X)
  medie_x_2 <- fexpectedvalue(funarop(X,2,"^"))

  varianta <- medie_x_2 - (medie_x^2)

  return(varianta)
}


