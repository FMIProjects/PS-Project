\name{fbidimdens}
\alias{fbidimdens}
\title{fbidimdens}
\description{

  Testeaza daca functia bidimensionala este densitate de probabilitate.

}
\usage{fvabidimdens <- function(f,lowX=-10^5,highX=10^5,lowY=-10^5,highY=10^5)}

\arguments{

  \item{f}{Functia bidi}
  \item{lowX}{Lowwer bound-ul pentru parametrul x}
  \item{highX}{Upper bound-ul pentru parametrul x}
  \item{lowY}{Lower bound-ul pentru parametrul y}
  \item{highY}{Upper bound-ul pentru parametrul y}


}

\details{

 Se calculeaza integrala in intervalul dat si se testeaza daca este 1 si totodata, cu aproximatie, daca este pozitiva.

}
\value{TRUE daca functia este densitate sau FALSE in caz contrar }
