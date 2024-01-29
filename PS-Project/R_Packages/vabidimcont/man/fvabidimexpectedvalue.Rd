\name{fvabidimexpectedvalue}
\alias{fvabidimexpectedvalue}
\title{fvabidimexpectedvalue}
\description{

 Testeaza, cu aproximatie, daca functia dublu parametrizata data este pozitiva pe intevalul dat.

}
\usage{fvabidimexpectedvalue <- function(fdens,fmedie,lowX=-10^5,highX=10^5,lowY=-10^5,highY=10^5)}

\arguments{

  \item{fdens}{Densitatea de probabilitate a v.a. bidimensionale}
  \item{fmedie}{Functia bidimensionala care va fi aplicata asupra celor 2 v.a.}
  \item{lowX}{Lower bound-ul pentru parametrul x}
  \item{highX}{Upper bound-ul pentru parametrul x}
  \item{lowY}{Lower bound-ul pentru parametrul y}
  \item{highY}{Upper bound-ul pentru parametrul y}

}

\details{

  Calculeaza integrala dubla de fmedie(x,y) * fdens(x,y) cu captele corespunzatoare

}
\value{Valoarea integralei calculate}
