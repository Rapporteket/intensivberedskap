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
RisikofaktorerTab <- function(RegData, tidsenhet='Totalt', datoTil=Sys.Date(), reshID=0,
                              erMann='', bekr=9, skjemaStatus=9,
                              dodInt=9, velgRHF=0, velgAvd=0){

  RegData <- NIRUtvalgBeredsk(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
                              bekr=bekr, skjemaStatus=skjemaStatus,dodInt=dodInt,
                              reshID=reshID)$RegData #velgRHF=velgRHF, velgAvd=velgAvd
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
  if (tidsenhet=='Totalt'){TabRisiko <- as.matrix(TabRisiko[,"Sum"], ncol=1)
  colnames(TabRisiko) <- 'Sum'}
  TabRisiko <- cbind(TabRisiko,
        'Andel' = paste0(sprintf('%.0f', 100*TabRisiko[,"Sum"]/dim(RegData)[1]),'%')
  )
    xtable::xtable(TabRisiko,
                   digits=0,
                   align = c('l',rep('r',ncol(TabRisiko))),
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
TabTidEnhet <- function(RegData, tidsenhet='dag', enhetsNivaa='RHF',erMann=9,
                        bekr=9, skjemaStatus=9, dodInt=9, velgRHF=0, velgAvd=0){

    RegData <- NIRUtvalgBeredsk(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
                                bekr=bekr, skjemaStatus=skjemaStatus,
                                dodInt=dodInt)$RegData #velgRHF=velgRHF, velgAvd=velgAvd

  TidsVar <- switch (tidsenhet,
    dag = 'Dag',
    uke = 'UkeNr',
    maaned = 'MndAar')

  RegData$EnhetsNivaaVar <- RegData[ ,enhetsNivaa]
  #RegData$HF <- factor(RegData$HF, levels=unique(RegData$HF))
  #TabRHF <- table(CoroData$Dag, CoroData$RHF)
  #xtable::xtable(addmargins(TabRHF), digits=0, caption='Coronatilfeller per uke i hvert RHF')

TabTidEnh <- table(RegData[ , c(TidsVar, enhetsNivaa)]) #ftable(RegData[ , c(TidsVar, enhetsNivaa, 'Korona')], row.vars =TidsVar)

navnEnh <- unique(RegData$EnhetsNivaaVar)
TabTidEnh <- as.matrix(TabTidEnh)
#colnames(TabTidEnh) <- rep(c('M','B'), length(navnEnh)) #letters[1:8]

TabTidEnh <- addmargins(TabTidEnh, FUN=list(Totalt=sum, 'Hele landet' = sum), quiet=TRUE)
#add.to.row <- list(pos = list(-1), command = NULL)
#add.to.row$command <- paste0(paste0('& \\multicolumn{2}{l}{', navnEnh, '} ', collapse=''), '\\\\\n')
TabTidEnh <- xtable::xtable(TabTidEnh, digits=0, #method='compact', #align=c('l', rep('r', ncol(alderDIV))),
               caption=paste0('Antall Coronatilfeller.'))

return(TabTidEnh)
}

#' Antall som er eller har vært i ECMO/respirator
#'
#' @param RegData beredskapsskjema
#'
#' @return
#' @export
#'
statusECMOrespTab <- function(RegData){

  N <- dim(RegData)[1]
  ##MechanicalRespirator Fått respiratorstøtte. Ja=1, nei=2,
AntBruktResp <- sum(RegData$MechanicalRespirator==1, na.rm=T)
AntBruktECMO <- sum(RegData$EcmoDurationInHours>0, na.rm=T)

AntIrespNaa <- sum(!(is.na(RegData$MechanicalRespiratorStart))) -
  sum(!(is.na(RegData$MechanicalRespiratorEnd)))
AntIECMONaa <- sum(!(is.na(RegData$EcmoStart))) - sum(!(is.na(RegData$EcmoEnd)))
AntPaaIntNaa <- N - sum(!(is.na(RegData$DateDischargedIntensive)))

TabHjelp <- rbind(
  'På respirator nå' = AntIrespNaa*(c(1, 100/AntPaaIntNaa)),
  'På ECMO nå' = AntIECMONaa*(c(1, 100/AntPaaIntNaa)),
  'På intensiv nå' = c(AntPaaIntNaa,100),
  'Brukt respirator'= AntBruktResp*c(1, 100/N),
  'Brukt ECMO'= AntBruktECMO*c(1, 100/N),
  'Brukt intesivseng' = c(N,100)
)
colnames(TabHjelp) <- c('Antall', 'Andel')
TabHjelp[,'Andel'] <- paste0(sprintf('%.0f', TabHjelp[,2]),'%')
xtable::xtable(TabHjelp,
               digits=0,
               align = c('l','r','r'),
               caption='Bruk av Respirator/ECMO.')
}
