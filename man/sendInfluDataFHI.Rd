% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NIRberedHjelpefunk.R
\name{sendInfluDataFHI}
\alias{sendInfluDataFHI}
\title{Funksjon som henter filer som skal sendes til FHI. To filer fra intensivopphold
og to filer fra sykehusopphold. Dvs. Ei fil for hvert opphold og ei aggregert til
person, for hvert register. IKKE i bruk på beredskap. Filer sendes fra Pandemi...}
\usage{
sendInfluDataFHI(zipFilNavn = "Testfil", brukernavn = "testperson")
}
\arguments{
\item{zipFilNavn}{Navn på fila som skal kjøres. DataFHIPanBered, Testfil}

\item{brukernavn}{Innlogget brukernavn}
}
\value{
Filsti til fil med filsti til zip...
}
\description{
Funksjon som henter filer som skal sendes til FHI. To filer fra intensivopphold
og to filer fra sykehusopphold. Dvs. Ei fil for hvert opphold og ei aggregert til
person, for hvert register. IKKE i bruk på beredskap. Filer sendes fra Pandemi...
}
