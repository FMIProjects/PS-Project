\name{fcovvabidim}
\alias{fcovvabidim}
\title{fcovvabidim}
\description{

 Calculeaza covarianta a doua v.a. continue.

}
\usage{function(fdens,fmedie,lowX=-10^5,highX=10^5,lowY=-10^5,highY=10^5)}

\arguments{

  \item{fdens}{Densitatea de probabilitate a v.a. bidimensionale}
  \item{fX}{Functia pentru primul parametru}
  \item{fY}{Functia pentru al doilea parametru}
  \item{lowX}{Lower bound-ul pentru parametrul x}
  \item{highX}{Upper bound-ul pentru parametrul x}
  \item{lowY}{Lower bound-ul pentru parametrul y}
  \item{highY}{Upper bound-ul pentru parametrul y}


}

\details{

  Calculeaza media lui fx*fy media lui fx si media lui fy si aplica formula covariantei.

}
\value{Valoarea covariantei. }
