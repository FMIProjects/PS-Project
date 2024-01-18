\name{fPcond}
\alias{fPcond}
\title{fPcond}
\description{
Calculeaza repartitia variabilei X|Y
}
\usage{fPcond <- function(X,Y,repCom)}

\arguments{
  \item{X}{Functia de repartitie a v.a X.}
  \item{Y}{Functia de repartitie a v.a Y.}
  \item{repCom}{Repartitia comuna a v.a X si Y unde valorile lui X sunt reprezentate pe coloana 1 a repartitiei comune.}
  }

\details{
    Daca avem repartitia lui XY adica valorile lui X sunt reprezentate pe prima coloana,
    daca vrem repartitia lui YX pentru a calcula Y|X atunci trebuie sa transpunem matricea
  }
\value{returneaza o matrice unde pe prima linia sunt valorile lui X si pe prima coloana sunt valorile lui Y, iar restul celulelor reprezinta P(X=x|Y=y)}

