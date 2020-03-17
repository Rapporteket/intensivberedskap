#' Funksjoner for å lage tabeller
#'
#' Tabell med oversikt over tilstander som medfører økt risiko ved Coronasmitte
#'
#' @param RegData data
#' @param datoTil sluttdato
#' @param reshID enhetens resh
#' @param tidsenhet 'Dag', 'Uke' (standard)
#' @param valgtRHF 'Alle' (standard), RHF-navn uten 'Helse '
#'
#' @export
#' @return
RisikofaktorerTab <- function(RegData, tidsenhet='Totalt', datoTil=Sys.Date(), reshID=0,
                              erMann='', bekr=9, skjemastatus=9,
                              dodInt=9, valgtRHF='Alle', velgAvd=0){

  RegData <- NIRUtvalgBeredsk(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
                              bekr=bekr, skjemastatus=skjemastatus,dodInt=dodInt,
                              reshID=reshID, valgtRHF=valgtRHF)$RegData #velgAvd=velgAvd
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
                        bekr=9, skjemastatus=9, dodInt=9, valgtRHF='Alle', velgAvd=0){

    RegData <- NIRUtvalgBeredsk(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
                                bekr=bekr, skjemastatus=skjemastatus,
                                dodInt=dodInt, valgtRHF=valgtRHF)$RegData #velgAvd=velgAvd

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
statusECMOrespTab <- function(RegData, valgtRHF='Alle'){

  RegData <- NIRUtvalgBeredsk(RegData=RegData, valgtRHF=valgtRHF)$RegData
                              # ,  datoFra=0, datoTil=0, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
                              # bekr=bekr, skjemastatus=skjemastatus,
                              # dodInt=dodInt)$RegData velgAvd=velgAvd

  N <- dim(RegData)[1]
  ##MechanicalRespirator Fått respiratorstøtte. Ja=1, nei=2,
AntIrespNaa <- sum(!(is.na(RegData$MechanicalRespiratorStart))) -
  sum(!(is.na(RegData$MechanicalRespiratorEnd)))
AntIECMONaa <- sum(!(is.na(RegData$EcmoStart))) - sum(!(is.na(RegData$EcmoEnd)))
AntPaaIntNaa <- N - sum(!(is.na(RegData$DateDischargedIntensive)))

TabHjelp <- rbind(
  'På respirator nå' = AntIrespNaa*(c(1, 100/AntPaaIntNaa)),
  'På ECMO nå' = AntIECMONaa*(c(1, 100/AntPaaIntNaa)),
  'På intensiv nå' = c(AntPaaIntNaa,'')
)
colnames(TabHjelp) <- c('Antall', 'Andel')
TabHjelp[1:2,'Andel'] <- paste0(sprintf('%.0f', as.numeric(TabHjelp[1:2,'Andel'])),'%')
xtable::xtable(TabHjelp,
               digits=0,
               align = c('l','r','r'),
               caption='Bruk av Respirator/ECMO.')
}


#' Antall som er eller har vært i ECMO/respirator
#'
#' @param RegData beredskapsskjema
#' @inheritParams NIRUtvalgBeredsk
#'
#' @return
#' @export
#'
oppsumLiggetiderTab <- function(RegData, valgtRHF='Alle'){

  RegData <- NIRUtvalgBeredsk(RegData=RegData, valgtRHF=valgtRHF,
                              skjemastatus=2,)$RegData
  # ,  datoFra=0, datoTil=0, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
  # bekr=bekr,
  # dodInt=dodInt)$RegData velgAvd=velgAvd

  N <- dim(RegData)[1]
  ##MechanicalRespirator Fått respiratorstøtte. Ja=1, nei=2,
  AntBruktResp <- sum(RegData$MechanicalRespirator==1, na.rm=T)
  AntBruktECMO <- sum(RegData$ECMOTid>0, na.rm=T)
  AntUtInt <- sum(RegData$DateDischargedIntensive>0, na.rm=T)
  Liggetid <- summary(RegData$liggetid, na.rm = T)
  RespTid <- summary(RegData$RespTid, na.rm = T)
  ECMOtid <- summary(RegData$ECMOTid, na.rm = T)

med_IQR <- function(x){
  c(sprintf('%.1f',x[3]), paste(sprintf('%.2f',x[2]), sprintf('%.2f',x[5]), sep='-'))}
# x <- Liggetid
# test <- sprintf('%.2f',c(x[2],x[5]))
#med_IQR(Liggetid)

TabLiggetider <- rbind(
    'ECMO-tid,' = c(med_IQR(ECMOtid), AntBruktECMO*(c(1, 100/AntUtInt))),
    'Respiratortid' = c(med_IQR(RespTid), AntBruktResp*(c(1, 100/AntUtInt))),
    'Liggetid' = c(med_IQR(Liggetid),AntUtInt, '')
    #Median respiratortid'
  )
  colnames(TabLiggetider) <- c('Median', 'IQR', 'Antall', 'Andel')
  TabLiggetider[1:2,'Andel'] <- paste0(sprintf('%.0f', as.numeric(TabLiggetider[1:2,'Andel'])),'%')
  xtable::xtable(TabLiggetider,
                 digits=0,
                 align = c('l','r','c', 'r','r'),
                 caption='Liggetider og ECMO/respiratorbruk, ferdigstilte opphold. \\
                 IQR (Inter quartile range) - 50% av oppholdene er i dette intervallet.')
}
