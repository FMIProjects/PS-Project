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
  \item{repCom}{Repartitia comuna a v.a X si Y.}
  }

\details{
  Se impart valorile de pe fiecare coloana a valorilor comune la probabilitatile de pe coloanele corespunzatoare a celeilalte v.a. Y. Apoi se extrage o repartitie extinsa pentru variabila X unde pe primul rand se afla valorile lui X, si pe urmatoarele randuri se afla probabilitatile P(X|Y=y)
  }
\value{returneaza o matrice unde pe prima linia sunt valorile lui X si pe prima coloana sunt valorile lui Y, iar restul celulelor reprezinta P(X=x|Y=y)}

