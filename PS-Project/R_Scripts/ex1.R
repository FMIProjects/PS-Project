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

n <- 7
m <- 10
rep_comuna_incompleta <- frepcomgen(n,m)



# b).
# completare tabel rep. comuna

c <- which(is.na(rep_comuna_incompleta),arr.ind=TRUE)

sum(rep_comuna_incompleta[2,])
for(i in 1:n){
  rep_comuna_incompleta[c[i,1],c[i,2]] <- rep_comuna_incompleta[c[i,1],m+2] - sum()

}

fcomplrepcom()



