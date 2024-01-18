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

n <- 3
m <- 4
rep_comuna_incompleta <- frepcomgen(n,m)


# b).
# completare tabel rep. comuna

rep_comuna_rezolvata <- fcomplrepcom(rep_comuna_incompleta)

# c).
# obtinerea repartititei marginale
 marginale <- frepmarginal(rep_comuna_rezolvata)
 X <- matrix(unlist(marginale[1]), nrow = 2)
 Y <- matrix(unlist(marginale[2]), nrow = 2)

# d).
# determinarea covalentei cov(Z,T)
# unde Z = aX + bY si T = cX + dY
a <- 2
b <- 1
c <- 1
d <- 2
fpropcov(X,Y,rep_comuna_rezolvata,a,b,c,d)

# e).
# calcul probabilitate conditionata

fPcond(X,Y,rep_comuna_rezolvata)
fPcond(Y,X,rep_comuna_rezolvata)

# g)
# operatii comune

X
#calcule doar pe repartitia unei variabile
fPcomun(X=X,lowX=3,highX=3)
fPcomun(X=X,lowX=-4,highX=5)

#calcule pe 2 variabile
fPcomun(rep_comuna_rezolvata,X=X,Y=Y)
fPcomun(rep_comuna_rezolvata,X=X,Y=Y,-4,4,-3,2)



# h).
# verificare variabile independente si necorelate

fvernecor(X,Y,rep_comuna_rezolvata)
fverind(Y,X,rep_comuna_rezolvata)



