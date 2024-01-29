\name{fmomentdeordink}
\alias{fmomentdeordink}
\title{fmomentdeordink}
\description{

 Calculeaza momentul centrat/initial de ordin k a unei variabile aleatoare cu ajutorul densitatii comune.

}
\usage{fmomentdeordink <- function(fdens,fmedie,lowX=-10^5,highX=10^5,lowY=-10^5,highY=10^5,order,initial=FALSE)}

\arguments{

  \item{fdens}{Densitatea de probabilitate a v.a. bidimensionale}
  \item{fmedie}{Functia bidimensionala care va fi aplicata asupra celor 2 v.a.}
  \item{lowX}{Lower bound-ul pentru parametrul x}
  \item{highX}{Upper bound-ul pentru parametrul x}
  \item{lowY}{Lower bound-ul pentru parametrul y}
  \item{highY}{Upper bound-ul pentru parametrul y}
  \item{order}{Ordinul momentului}
  \item{initial}{Verifica tipul momentului}

}

\details{

  Calculeaza prima data media lui fmedie(x,y) * fdens(x,y). Apoi creeaza o noua functie fmedieordin definita:
  fmedieordin= (fmedie(x,y) - medie)^order si calculeaza media de aceasta functie. Daca momentul este inital atunci medie = 0.
}
\value{Valoarea moentului de ordin k.}
