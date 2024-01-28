\name{fXdens}
\alias{fXdens}
\title{fXdens}
\description{

  Calculeaza valoarea lui fX(x) pe baza unei densitati comune fXY.

}
\usage{fXdens(fXY,x)}

\arguments{

  \item{fXY}{Densitatea bidimensionala a lui X si Y}
  \item{x}{Valoarea lui x}
  \item{lowY}{Lower bound pentru Y deoarece integrala este dupa y.}
  \item{highY}{Higher bound pentru Y deoarece integrala este dupa y.}

}

\details{

 In densitatea comuna fXY se fixeaza x cu valoare data si se integreaza functia astfel obtinuta dupa y.
}
\value{Valoarea functiei fX(x). }
