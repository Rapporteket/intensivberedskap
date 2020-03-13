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
    Dag = RegData$Dag,
    Aar = RegData$Aar)

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

#' Title
#'
#' @param RegData dataramme med preprossesserte data
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned'
#' @param enhetsNivaa
#'
#' @return
#' @export
TabTidEnhet <- function(RegData, tidsenhet='dag', enhetsNivaa='RHF'){

  TidsVar <- switch (tidsenhet,
    dag = 'Dag',
    uke = 'Uke',
    maaned = 'MndAar')

  RegData$EnhetsNivaaVar <- RegData[ ,enhetsNivaa]

  RegData$HF <- factor(RegData$HF, levels=unique(RegData$HF))

TabTidEnh <- ftable(RegData[ , c(TidsVar, 'RHF', 'Korona')], row.vars =TidsVar)

navnEnh <- levels(RegData$RHF)
TabTidEnh <- as.matrix(TabTidEnh)
colnames(TabTidEnh) <- rep(c('M','B'), length(navnEnh)) #letters[1:8]

TabTidEnh <- addmargins(TabTidEnh, FUN=list(Totalt=sum, 'Hele landet' = sum), quiet=TRUE)

add.to.row <- list(pos = list(-1), command = NULL)
command <- paste0(paste0('& \\multicolumn{2}{l}{', navnEnh, '} ', collapse=''), '\\\\\n')
# Funker ikke: c(paste0('& \\multicolumn{2}{l}{', navnEnh, '} ', collapse=''),
#            "\\n{\\footnotesize B - bekreftet, M - mistenkt}\n") # #, '{\\footnote}{B-bekreftet, M-mistenkt}')
# '& \\multicolumn{2}{c}{A} & \\multicolumn{2}{c}{B} & \\multicolumn{2}{c}{C} & \\multicolumn{2}{c}{D}  \\\\\n'
#comment$command <- "\\hline\n{\\footnotesize B - bekreftet, M - mistenkt}\n"
add.to.row$command <- command
print(xtable::xtable(TabTidEnh, digits=0, #method='compact', #align=c('l', rep('r', ncol(alderDIV))),
                     caption=paste0('Coronatilfeller per dag og region (B - bekreftet, M - mistenkt)')),
      #footnote= 'B-bekreftet, M-mistenkt',),
      add.to.row = add.to.row,
      sanitize.rownames.function = identity)
}
