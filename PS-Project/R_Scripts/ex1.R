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

# f).
# calcul probabilitati legate de perechea (X,Y)

#calcule doar pe repartitia unei variabile
fPcomun(X=X,lowX=3,highX=3)
fPcomun(X=X,lowX=-4,highX=5)

#calcule pe 2 variabile
fPcomun(rep_comuna_rezolvata,X=X,Y=Y)
fPcomun(rep_comuna_rezolvata,X=X,Y=Y,-4,4,-3,2)

# g).

# 1). Cov(5X+9,-3Y-2) . Fie Z = 5X+9 , T= -3Y-2

Z <- funarop(funarop(X,5,"*",sort=FALSE),9,"+",sort=FALSE)

T <- funarop(funarop(Y,-3,"*",sort=FALSE),2,"-",sort=FALSE)

repComZT <- rep_comuna_rezolvata
repComZT[2:(n+1),1] <- Z[1,]
repComZT[2:(n+1),m+2] <- Z[2,]

repComZT[1,2:(m+1)] <- T[1,]
repComZT[n+2,2:(m+1)] <- T[2,]

# sortare repCom
indici_sortati_Y <- order(repComZT[1,2:(m+1)])
repComZT[,2:(m+1)] <- repComZT[, indici_sortati_Y+1]

indici_sortati_X <- order(repComZT[2:(n+1),1])
repComZT[2:(n+1),] <- repComZT[indici_sortati_X+1,]

fcov(Z,T,repComZT);

# 2). P(0<X<0.8|Y>0.3) = P(0<X<0.8,Y>0.3) / P(Y>0.3)

fPcomun(rep_comuna_rezolvata,X,Y,0,0.8,0.3) / fPcomun(rep_comuna_rezolvata,X=Y,lowX=0.3)

# 3) P(X>0.2,Y<1.7)

fPcomun(rep_comuna_rezolvata,X,Y,lowX = 0.2,highY = 1.7)

# h).
# verificare variabile independente si necorelate

# 1). independente

fverind(Y,X,rep_comuna_rezolvata)

# 2). necorelate
fvernecor(X,Y,rep_comuna_rezolvata)

# i).maniera vizuala de reprezentare a
# repartitiei comune pentru v.a. X, Y și Z.

# devine 3d (un paralelipiped dreptunghic)
# X -> axa OX ; y-> axa OY ; Z -> axa OZ

# p -> dreapta opusa in planul xoy
# q -> dreapta opusa in planul xoy
# k -> dreapta opusa in planul zo si diagonala xoy

# pi_ijk = P(X=xi,Y=yj,Z=zk)

# install.packages("rgl")
library(rgl)

# generam 100 pi_ijk
n <- 100
X <- runif(n, min = -50, max = 50)
Y <- runif(n, min = -50, max = 50)
Z <- runif(n, min = -50, max = 50)


# 3D plot
plot3d(X, Y, Z, col = "red", size = 0.4, type = "s", main = "Repartitie Comuna XYZ",
       xlab="", ylab="", zlab="",ann = FALSE, axes = FALSE)
box3d()
axes3d(edges = c("x--", "y--", "z--"))
mtext3d("X", "x--", line = 2)
mtext3d("Y", "y--", line = 2)
mtext3d("Z", "z--", line = 2)

mtext3d("p", "x+-", line = 2)
mtext3d("q", "y+-", line = 2)
mtext3d("k", "z++", line = 2)
play3d(spin3d(axis = c(0, 0, 1)), duration = 10000)


# Exercitiul 2








