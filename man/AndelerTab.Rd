% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NIRtabellerBeredskap.R
\name{AndelerTab}
\alias{AndelerTab}
\title{Tabell med andel av div. variabler for koblet datasett (intensiv+beredskap)
Kun ferdigstilte registreringer}
\usage{
AndelerTab(
  RegData,
  datoFra = "2020-01-01",
  datoTil = Sys.Date(),
  erMann = 9,
  bekr = 9,
  dodInt = 9,
  valgtRHF = "Alle",
  resp = 9,
  minald = 0,
  maxald = 110
)
}
\arguments{
\item{RegData}{data, beredskapsskjema}

\item{datoFra}{startdato 'yyyy-mm-dd'}

\item{datoTil}{sluttdato 'yyyy-mm-dd'}

\item{erMann}{kjønn, 0-kvinne, 1-mann}

\item{bekr}{1-bekreftet, 0-mistenkt tilfelle}

\item{dodInt}{død på intensiv 0-nei, 1-ja}

\item{valgtRHF}{'Alle' (standard), RHF-navn uten 'Helse '}

\item{resp}{Fått respiratorbehandling: 1-ja, 2-nei}
}
\value{
andel for ulike nøkkelverdier
}
\description{
Tabell med andel av div. variabler for koblet datasett (intensiv+beredskap)
Kun ferdigstilte registreringer
}
