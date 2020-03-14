#' Gjøre utvalg i beredskapsskjema
#'
#' Returnerer filtrert dataramme og utvalgstekst
#'
#' @param RegData data, beredskapsskjema
#' @param datoFra startdato 'yyyy-mm-dd'
#' @param datoTil sluttdato 'yyyy-mm-dd'
#' @param erMann kjønn, 0-kvinne, 1-mann
#' @param bekr 1-bekreftet, 0-mistenkt tilfelle
#' @param skjemaStatus status på registreringa 0-ingen, 1-kladd, 2-ferdigstilt, 4-slettet, 5-returnert
#' @param enhetsUtvalg enhetsutvalg...
#' @param dodInt død på intensiv 0-nei, 1-ja
#' @param reshID reshID fra innlogging
#' @param velgAvd valgt reshID
#'
#' @return
#' @export
#'
NIRUtvalgBeredsk <- function(RegData, datoFra=0, datoTil=0, erMann=9, #enhetsUtvalg=0, minald=0, maxald=110,
                             bekr=9, skjemaStatus=9,
                         dodInt=9, reshID=0, velgRHF=0, velgAvd=0) {

#RHF <- unique(RegData$RHF)


 if (bekr %in% 0:1){RegData <- subset(RegData, RegData$Bekreftet==bekr)}
 if (skjemaStatus %in% 1:2){RegData <- subset(RegData, RegData$FormStatus==skjemaStatus)}
 if (dodInt %in% 0:1){RegData <- subset(RegData, RegData$DischargedIntensivStatus==dodInt)}
 if (erMann %in% 0:1){
   vec <- (RegData$erMann == erMann)
   RegData <- subset(RegData, vec)}


 if(datoFra!=0) {RegData <- subset(RegData, RegData$InnDato >= as.Date(datoFra, tz= 'UTC'))}
 if(datoTil!=0) {RegData <- subset(RegData, RegData$InnDato <= as.Date(datoTil, tz= 'UTC'))}

  utvalgTxt <- c(
    if(datoFra!=0 | datoTil!=0) {paste0(
      'Innleggelsesdatoer: ', if (N>0) {min(as.Date(RegData$InnDato), na.rm=T)} else {datoFra},
      ' til ', if (N>0) {max(as.Date(RegData$InnDato), na.rm=T)} else {datoTil})} else {NULL},
    # if ((minald>0) | (maxald<110)) {
    #   paste0('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald},
    #          ' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år')},
    if (bekr %in% 0:1){paste(c('Bekreftede','Mistenkte')[bekr+1], 'tilfeller')},
    if (skjemaStatus %in% 0:5){paste('Skjemastatus:',
                                     c('ingen', 'kladd', 'ferdigstilt', '','slettet', 'returnert')[skjemaStatus+1])},
    #if (velgRHF %in% 0:1) {paste0('RHF: ', c('Kvinner', 'Menn')[erMann+1])},
    if (erMann %in% 0:1) {paste0('Kjønn: ', c('Kvinner', 'Menn')[erMann+1])},
    if (dodInt %in% 0:1) {paste0('Status ut fra intensiv: ', c('Levende','Død')[as.numeric(dodInt)+1])},
    if (velgAvd[1] != 0 & reshID==0) {'Viser valgte sykehus'}
  )

 UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt) #ind=ind, medSml=medSml, smltxt=smltxt, hovedgrTxt=hovedgrTxt, grTypeTxt=grTypeTxt,

 return(invisible(UtData))
}
