#' Gjøre utvalg i beredskapsskjema
#'
#' Returnerer filtrert dataramme og utvalgstekst
#'
#' @param RegData data, beredskapsskjema
#' @param datoFra startdato 'yyyy-mm-dd'
#' @param datoTil sluttdato 'yyyy-mm-dd'
#' @param erMann kjønn, 0-kvinne, 1-mann
#' @param bekr 1-bekreftet, 0-mistenkt tilfelle
#' @param skjemastatus status på registreringa 0-ingen, 1-kladd, 2-ferdigstilt, 4-slettet, 5-returnert
#' @param enhetsUtvalg enhetsutvalg...
#' @param resp Fått respiratorbehandling: 1-ja, 2-nei
#' @param dodInt død på intensiv 0-nei, 1-ja
#' @param reshID reshID fra innlogging
#' @param velgAvd valgt reshID
#'
#' @return Filtrert datasett samt tekst som angir hvilke filtreringer som er gjort
#' @export
NIRUtvalgBeredsk <- function(RegData, datoFra=0, datoTil=0, erMann=9, minald=0, maxald=110, #enhetsUtvalg=0,
                             bekr=9, skjemastatus=9, resp=9, respIN=9, datovar='InnDato',
                             dodInt=9, reshID=0, valgtRHF='Alle', velgAvd=0) {


  if (bekr %in% 0:1){RegData <- subset(RegData, RegData$Bekreftet==bekr)}
  if (skjemastatus %in% 1:2){RegData <- subset(RegData, RegData$FormStatus==skjemastatus)}
  if (resp %in% 1:2){RegData <- subset(RegData, RegData$MechanicalRespirator==resp)}
  if (respIN %in% 1:2){RegData <- subset(RegData, RegData$MechanicalRespirator==respIN)}
  if (dodInt %in% 0:1){RegData <- subset(RegData, RegData$DischargedIntensiveStatus==dodInt)}
  if (erMann %in% 0:1){
    vec <- (RegData$erMann == erMann)
    RegData <- subset(RegData, vec)}
  if (valgtRHF != 'Alle'){RegData <- subset(RegData, RegData$RHFut == valgtRHF)}

  if(minald>0 | maxald<110) {RegData <- subset(RegData,
                                               RegData$Alder >= minald & RegData$Alder <= maxald)}

  if(datoFra!=0) {RegData <- subset(RegData, RegData[,datovar] >= as.Date(datoFra, tz= 'UTC'))}
  if(datoTil!=0) {RegData <- subset(RegData, RegData[,datovar] <= as.Date(datoTil, tz= 'UTC'))}

  N <- dim(RegData)[1]

  utvalgTxt <- c(
    if (bekr %in% 0:1){paste(c('Mistenkte','Bekreftede')[bekr+1], 'tilfeller')
    } else {'Alle tilfeller (mistenkte og bekreftede)'},
    if(datoFra!=0 | datoTil!=0) {paste0(
      'Innleggelsesdatoer: ', if (N>0) {min(as.Date(RegData[,datovar]), na.rm=T)} else {datoFra},
      ' til ', if (N>0) {max(as.Date(RegData[,datovar]), na.rm=T)} else {datoTil})} else {NULL},
    if ((minald>0) | (maxald<110)) {
  RegData$Alder <- round(RegData$Alder,1)
      paste0('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald},
             ' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år')},
    if (skjemastatus %in% 0:5){paste('Skjemastatus:',
                                     c('ingen', 'kladd', 'ferdigstilt', '','slettet', 'returnert')[skjemastatus+1])},
    if (resp %in% 1:2){c('Fått respiratorbehandling', 'Ikke fått respiratorbehandling')[resp]},
    if (respIN %in% 1:2){c('Invasiv respiratorbehandling', 'Non-invasiv respiratorbehandling')[resp]},
    if (erMann %in% 0:1) {paste0('Kjønn: ', c('Kvinner', 'Menn')[erMann+1])},
    if (dodInt %in% 0:1) {paste0('Status ut fra intensiv: ', c('Levende','Død')[as.numeric(dodInt)+1])},
    if (valgtRHF != 'Alle'){paste('Valgt RHF:', valgtRHF)},
    if (velgAvd[1] != 0 & reshID==0) {'Viser valgte sykehus'}
  )

  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt)

  return(invisible(UtData))
}
