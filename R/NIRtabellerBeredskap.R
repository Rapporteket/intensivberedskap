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
RisikofaktorerTab <- function(RegData, tidsenhet='Uke', datoTil=Sys.Date(), reshID=0){

  Tidsvariabel <- switch(tidsenhet,
    Uke = paste0('uke',RegData$UkeNr),
    Dag = RegData$Dag)

    TabRisiko <- cbind(
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

    xtable::xtable(addmargins(t(TabRisiko), margin = 2),
                   digits=0,
                   caption='Risikofaktorer')
    #return(TabRisiko)
}
