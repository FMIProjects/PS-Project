\name{fYdens}
\alias{fYdens}
\title{fYdens}
\description{

  Calculeaza valoarea lui fY(y) pe baza unei densitati comune fXY.

}
\usage{fYdens(fXY,y)}

\arguments{

  \item{fXY}{Densitatea bidimensionala a lui X si Y}
  \item{y}{Valoarea lui y}
  \item{lowX}{Lower bound pentru X deoarece integrala este dupa x.}
  \item{highX}{Higher bound pentru X deoarece integrala este dupa x.}

}

\details{

 In densitatea comuna fXY se fixeaza y cu valoare data si se integreaza functia astfel obtinuta dupa x.
}
\value{Valoarea functiei fY(y). }
