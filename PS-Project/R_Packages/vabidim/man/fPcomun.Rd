\name{fPcomun}
\alias{fPcomun}
\title{fPcomun}
\description{
  Caluculeaza P(lowX<X<highX , lowY<Y<highY). Poate sa calculeze si P(lowX<X<highX)

}
\usage{fPcomun(repCom=NULL,X,Y=NULL,lowX=-Inf,highX=Inf,lowY=-Inf,highY=Inf)}

\arguments{

  \item{repCom}{Repartitia comuna a variabilelor X si Y.}
  \item{X}{Functia de repartitie a v.a X.}
  \item{Y}{Functia de repartitie a v.a Y.}
  \item{lowX}{Lowwer bound-ul pentru variabila X}
  \item{highX}{Upper bound-ul pentru variabila X}
  \item{lowY}{Lower bound-ul pentru variabila Y}
  \item{highY}{Upper bound-ul pentru variabila Y}

}

\details{

  Toate argumentele in afara de X sunt optionale. Daca se doreste calcul pe o singura variabila doar parametrul X este obligatoriu si lowX highX optionale.

  Daca se doreste calcul cu doua variabile trebuie sa se foloseasca variabilele repCom,X,Y obligatoriu, restul sunt optionale.

  Functionare bound-uri:

    Daca lowX nu este specificat:
    lowX<X<highX <-> X<highX

    Daca highX nu este specificat:
    lowX<X<highX <-> X>highX

    Daca nici lowX si nici highX nu sunt specificate:
    lowX<X<highX <-> X (toate valorile sunt selectate)

}
\value{P(lowX<X<highX , lowY<Y<highY) sau P(lowX<X<highX)}

