\name{fvabdimvariance}
\alias{fvabdimvariance}
\title{fvabdimvariance}
\description{

 Calculeaza varianta unei functii pe doua v.a. continue pe baza densitatii lor comune.

}
\usage{fvabdimvariance <- function(fdens,fmedie,lowX=-10^5,highX=10^5,lowY=-10^5,highY=10^5)}

\arguments{

  \item{fdens}{Densitatea de probabilitate a v.a. bidimensionale}
  \item{fmedie}{Functia bidimensionala care va fi aplicata asupra celor 2 v.a.}
  \item{lowX}{Lower bound-ul pentru parametrul x}
  \item{highX}{Upper bound-ul pentru parametrul x}
  \item{lowY}{Lower bound-ul pentru parametrul y}
  \item{highY}{Upper bound-ul pentru parametrul y}

}

\details{

  Se calculeaza E[fmedie(X,Y)^2] si apoi E[fmedie(X,Y)]^2.

}
\value{Valoarea variantei calculate.}
