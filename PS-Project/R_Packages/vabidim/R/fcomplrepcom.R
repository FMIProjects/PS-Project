# fcomplrepcom
fcomplrepcom <- function(n,m,tabel) {
  c <- which(is.na(rep_comuna_incompleta),arr.ind=TRUE)

  for(i in 1:n){
    print(rep_comuna_incompleta[c[i,1],c[i,2]])
  }
}
