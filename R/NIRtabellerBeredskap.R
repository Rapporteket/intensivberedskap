#' Funksjoner for å lage tabeller
#'
#' Tabell med oversikt over tilstander som medfører økt risiko ved Coronasmitte
#'
#' @param RegData data
#' @param datoTil sluttdato
#' @param reshID enhetens resh
#' @param tidsenhet 'Dag', 'Uke' (standard)
#'
#' @export
#' @return
RisikofaktorerTab <- function(RegData, tidsenhet='Totalt', datoTil=Sys.Date(), reshID=0){
#Kvikk fix: Totalt gir nå totalen for 2020
  Tidsvariabel <- switch(tidsenhet,
    Uke = paste0('uke',RegData$UkeNr),
    Dag = RegData$Dag,
    Totalt = RegData$Aar)

  TabRisiko <- rbind(
      Kreft = tapply(RegData$Kreft, Tidsvariabel, FUN=sum, na.rm = T),
      'Nedsatt immunforsvar' = tapply(RegData$IsImpairedImmuneSystemIncludingHivPatient, Tidsvariabel, FUN=sum, na.rm = T),
      Diabetes	= tapply(RegData$Diabetes, Tidsvariabel, FUN=sum, na.rm = T),
      Hjertesykdom = tapply(RegData$IsHeartDiseaseIncludingHypertensionPatient, Tidsvariabel, FUN=sum, na.rm = T),
      'Fedme (KMI>30)' =	tapply(RegData$IsObesePatient, Tidsvariabel, FUN=sum, na.rm = T),
      Astma	= tapply(RegData$Astma, Tidsvariabel, FUN=sum, na.rm = T),
      'Kronisk lungesykdom' = tapply(RegData$IsChronicLungDiseasePatient, Tidsvariabel, FUN=sum, na.rm = T),
      Nyresykdom =	tapply(RegData$IsKidneyDiseaseIncludingFailurePatient, Tidsvariabel, FUN=sum, na.rm = T),
      Leversykdom = tapply(RegData$IsLiverDiseaseIncludingFailurePatient, Tidsvariabel, FUN=sum, na.rm = T),
      'Nevrologisk/nevromusk.' = tapply(RegData$IsChronicNeurologicNeuromuscularPatient, Tidsvariabel, FUN=sum, na.rm = T),
      Graviditet	= tapply(RegData$Graviditet, Tidsvariabel, FUN=sum, na.rm = T),
      'Røyker' =	tapply(RegData$IsActivSmoker, Tidsvariabel, FUN=sum, na.rm = T),
      'Opphold med risikofaktorer' = tapply(RegData$IsRiskFactor, Tidsvariabel, FUN=sum, na.rm = T)
    )
  TabRisiko <- as.table(addmargins(TabRisiko, margin = 2))
  if (tidsenhet=='Totalt'){TabRisiko <- as.matrix(TabRisiko[,"Sum"], ncol=1)}
    xtable::xtable(TabRisiko,
                   digits=0,
                   #align = c('l',rep('r'),
                   caption='Risikofaktorer')
    #return(TabRisiko)
}

#' Title
#'
#' @param RegData dataramme med preprossesserte data
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned'
#' @param enhetsNivaa 'RHF', 'HF', 'ShNavn'
#'
#' @return
#' @export
TabTidEnhet <- function(RegData, tidsenhet='dag', enhetsNivaa='RHF'){

  enhetsNivaa <- 'ShNavn'
  TidsVar <- switch (tidsenhet,
    dag = 'Dag',
    uke = 'UkeNr',
    maaned = 'MndAar')

  RegData$EnhetsNivaaVar <- RegData[ ,enhetsNivaa]
  #RegData$HF <- factor(RegData$HF, levels=unique(RegData$HF))

TabTidEnh <- ftable(RegData[ , c(TidsVar, enhetsNivaa, 'Korona')], row.vars =TidsVar)

navnEnh <- unique(RegData$EnhetsNivaaVar)
TabTidEnh <- as.matrix(TabTidEnh)
colnames(TabTidEnh) <- rep(c('M','B'), length(navnEnh)) #letters[1:8]

TabTidEnh <- addmargins(TabTidEnh, FUN=list(Totalt=sum, 'Hele landet' = sum), quiet=TRUE)

}
