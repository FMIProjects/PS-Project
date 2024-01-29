\name{fbidimfuncpoz}
\alias{fbidimfuncpoz}
\title{fbidimfuncpoz}
\description{

 Testeaza, cu aproximatie, daca functia dublu parametrizata data este pozitiva pe intevalul dat.

}
\usage{fbidimfuncpoz <- function(f,lowX=-10^8,highX=10^8,lowY=-10^8,highY=10^8,precision = 10^4)}

\arguments{

  \item{f}{Functia bidimensionala}
  \item{lowX}{Lower bound-ul pentru parametrul x}
  \item{highX}{Upper bound-ul pentru parametrul x}
  \item{lowY}{Lower bound-ul pentru parametrul y}
  \item{highY}{Upper bound-ul pentru parametrul y}
  \item{precision}{Numarul de valori generate pentru fuctia data care vor fi testate}

}

\details{

  Se genereaza precision valori pentru x si y din intervalele date. Apoi se genereaza toate valorile doua cate doua pentru functia data si se testeaza daca toate sunt mai mari sau egale cu 0

}
\value{TRUE daca functia este pozitiva sau FALSE in caz contrar }
