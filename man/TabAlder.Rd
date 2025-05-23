% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NIRtabellerBeredskap.R
\name{TabAlder}
\alias{TabAlder}
\title{Aldersfordeling, tabell}
\usage{
TabAlder(
  RegData,
  reshID = 0,
  enhetsNivaa = "Alle",
  skjemastatus = 9,
  resp = 9,
  bekr = 9,
  dodInt = 9,
  erMann = 9,
  sens = 0
)
}
\arguments{
\item{RegData}{datatabell, beredskapsdata}

\item{reshID}{avdelingsresh}

\item{enhetsNivaa}{enhetsnivå}

\item{skjemastatus}{status på registreringa 0-ingen, 1-kladd, 2-ferdigstilt, 4-slettet, 5-returnert}

\item{resp}{Fått respiratorbehandling: 1-ja, 2-nei}

\item{bekr}{1-bekreftet, 0-mistenkt tilfelle}

\item{dodInt}{død på intensiv 0-nei, 1-ja}

\item{erMann}{kjønn, 0-kvinne, 1-mann}

\item{sens}{maskere celler <3. 0-nei, 1-ja}
}
\value{
aldersfordeling
}
\description{
Aldersfordeling, tabell
}
\examples{
TabAlder(RegData=CoroData, enhetsNivaa='HF')
}
