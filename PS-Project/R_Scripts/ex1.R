# Proiect Probabilitati si Statistica
# Gabroveanu Razvan + Huma Stefan  - 243

# Exercitiul 1

# library custom ce cuprinde toate functiile
library(vabidim)

# pentru ca valorile numerice sa nu fie afisate
# de tipul e+...
options(scipen = 999)


# a).
# generare tabel rep. comuna a v.a. X si Y incompleta

n <- 2
m <- 2
rep_comuna_incompleta <- frepcomgen(n,m)


# b).
# completare tabel rep. comuna

rep_comuna_rezolvata <- fcomplrepcom(rep_comuna_incompleta)

# c).
# obtinerea repartititei marginale
 marginale <- frepmarginal(rep_comuna_rezolvata)
 marginalaX <- matrix(unlist(marginale[1]), nrow = 2)
 marginalaY <- matrix(unlist(marginale[2]), nrow = 2)

# d).

  marginalaX
  marginalaY



  XY <- fvaop(marginalaX,marginalaY,'*')
  XY
  sum(XY[2,])

  which(marginalaX[1,] == 1)

