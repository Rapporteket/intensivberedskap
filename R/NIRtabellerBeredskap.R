#Fil med samling av funksjoner som lager tabeller for beredskapsskjema

#' Antall tilfeller for valgt tidsenhet og enhetsnivå. Filtreringer kan også gjøres.
#'
#' @param RegData dataramme med preprossesserte data
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned'
#' @param enhetsNivaa 'RHF', 'HF', 'ShNavn'
#' @inheritParams NIRUtvalgBeredsk
#'
#' @return
#' @export
TabTidEnhet <- function(RegData, tidsenhet='dag', erMann=9, #enhetsNivaa='RHF',
                        bekr=9, skjemastatus=9, dodInt=9, valgtRHF='Alle', velgAvd=0){

  UtData <- NIRUtvalgBeredsk(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
                              bekr=bekr, skjemastatus=skjemastatus,
                              dodInt=dodInt, valgtRHF=valgtRHF) #velgAvd=velgAvd
RegData <- UtData$RegData
  TidsVar <- switch (tidsenhet,
                     dag = 'Dag',
                     uke = 'UkeNr',
                     maaned = 'MndAar')

  enhetsNivaa <- ifelse(as.character(valgtRHF)=='Alle', 'RHF', 'HF')

  RegData$EnhetsNivaaVar <- RegData[ ,enhetsNivaa]
  #RegData$HF <- factor(RegData$HF, levels=unique(RegData$HF))
  #TabRHF <- table(CoroData$Dag, CoroData$RHF)
  #xtable::xtable(addmargins(TabRHF), digits=0, caption='Coronatilfeller per uke i hvert RHF')

  TabTidEnh <- table(RegData[ , c(TidsVar, enhetsNivaa)]) #ftable(RegData[ , c(TidsVar, enhetsNivaa, 'Korona')], row.vars =TidsVar)

  navnEnh <- unique(RegData$EnhetsNivaaVar)
  TabTidEnh <- as.matrix(TabTidEnh)
  #colnames(TabTidEnh) <- rep(c('M','B'), length(navnEnh)) #letters[1:8]

  TabTidEnh <- addmargins(TabTidEnh, FUN=list(Totalt=sum, 'Hele landet' = sum), quiet=TRUE)
  colnames(TabTidEnh)[ncol(TabTidEnh)] <- switch(enhetsNivaa,
                                                 RHF = 'Hele landet',
                                                 HF = paste0(valgtRHF, ', totalt'))
  #add.to.row <- list(pos = list(-1), command = NULL)
  #add.to.row$command <- paste0(paste0('& \\multicolumn{2}{l}{', navnEnh, '} ', collapse=''), '\\\\\n')
  TabTidEnh <- xtable::xtable(TabTidEnh, digits=0, #method='compact', #align=c('l', rep('r', ncol(alderDIV))),
                              caption=paste0('Antall Coronatilfeller.'))

  return(UtData <- list(TabTidEnh=TabTidEnh, UtData$utvalgTxt))
}




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

  UtData <- NIRUtvalgBeredsk(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
                              bekr=bekr, skjemastatus=skjemastatus,dodInt=dodInt,
                              reshID=reshID, valgtRHF=valgtRHF) #velgAvd=velgAvd
  RegData <- UtData$RegData
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
    # xtable::xtable(TabRisiko,
    #                digits=0,
    #                align = c('l',rep('r',ncol(TabRisiko))),
    #                caption='Risikofaktorer')
    return(UtData <- list(Tab=TabRisiko, utvalgTxt=UtData$utvalgTxt))
}

#' Antall som er eller har vært i ECMO/respirator
#'
#' @param RegData beredskapsskjema
#'
#' @return
#' @export
#'
statusECMOrespTab <- function(RegData, valgtRHF='Alle', erMann=9, bekr=9){

  UtData <- NIRUtvalgBeredsk(RegData=RegData, valgtRHF=valgtRHF,
                               erMann=erMann, bekr=bekr)
                              # dodInt=dodInt)$RegData velgAvd=velgAvd
RegData <- UtData$RegData
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
UtData <- list(Tab=TabHjelp, utvalgTxt=UtData$utvalgTxt)
return(UtData)
}


#' Liggetider og antall som ar vært i ECMO/respirator
#'
#' @param RegData beredskapsskjema
#' @inheritParams NIRUtvalgBeredsk
#'
#' @return
#' @export
#'
oppsumLiggetiderTab <- function(RegData, valgtRHF='Alle', bekr=9, erMann=9, dodInt=9){

  UtData <- NIRUtvalgBeredsk(RegData=RegData, valgtRHF=valgtRHF,
                             bekr = bekr,
                              skjemastatus=2)
RegData <- UtData$RegData
  N <- dim(RegData)[1]
  ##MechanicalRespirator Fått respiratorstøtte. Ja=1, nei=2,
  AntBruktResp <- sum(RegData$MechanicalRespirator==1, na.rm=T)
  AntBruktECMO <- sum(RegData$ECMOTid>0, na.rm=T)
  AntUtInt <- sum(RegData$DateDischargedIntensive>0, na.rm=T)
  Liggetid <- summary(RegData$liggetid, na.rm = T)
  RespTid <- summary(RegData$RespTid, na.rm = T)
  ECMOtid <- summary(RegData$ECMOTid, na.rm = T)
  Alder <- summary(RegData$Alder, na.rm = T)

med_IQR <- function(x){
  c(sprintf('%.1f',x[3]), paste(sprintf('%.f',x[2]), sprintf('%.1f',x[5]), sep=' - '))}
# x <- Liggetid
# test <- sprintf('%.2f',c(x[2],x[5]))
#med_IQR(Liggetid)

TabLiggetider <- rbind(
    'ECMO-tid,' = c(med_IQR(ECMOtid), AntBruktECMO*(c(1, 100/AntUtInt))),
    'Respiratortid' = c(med_IQR(RespTid), AntBruktResp*(c(1, 100/AntUtInt))),
    'Liggetid' = c(med_IQR(Liggetid),AntUtInt, ''),
    'Alder' = c(med_IQR(Alder),AntUtInt, '')
  )
  colnames(TabLiggetider) <- c('Median', 'IQR', 'Antall', 'Andel')
  TabLiggetider[1:2,'Andel'] <- paste0(sprintf('%.0f', as.numeric(TabLiggetider[1:2,'Andel'])),'%')
  xtable::xtable(TabLiggetider,
                 digits=0,
                 align = c('l','r','c', 'r','r'),
                 caption='Liggetider og ECMO/respiratorbruk, ferdigstilte opphold. \\
                 IQR (Inter quartile range) - 50% av oppholdene er i dette intervallet.')
  return(invisible(UtData <- list(Tab=TabLiggetider, utvalgTxt=UtData$utvalgTxt)))
}




#' Aldersfordeling, tabell
#'
#' @param RegData datatabell, beredskapsdata
#' @inheritParams NIRUtvalgBeredsk
#'
#' @return
#' @export
#'
#' @examples TabAlder(RegData=CoroData, enhetsNivaa='HF')
TabAlder <- function(RegData, valgtRHF='Alle', bekr=9, skjemastatus=9,dodInt=9,erMann=9){#enhetsNivaa='RHF'

  UtData <- NIRUtvalgBeredsk(RegData=RegData,
                              bekr=bekr,
                             dodInt = dodInt,
                             erMann = erMann,
                              skjemastatus=skjemastatus,
                              valgtRHF=valgtRHF)
  RegData <- UtData$RegData

  enhetsNivaa <- ifelse(as.character(valgtRHF)=='Alle', 'RHF', 'HF')
  RegData$EnhetsNivaaVar <- RegData[ , enhetsNivaa]
N <- dim(RegData)[1]
gr <- seq(0, 90, ifelse(N<100, 25, 10) )
RegData$AldersGr <- cut(RegData$Alder, breaks=c(gr, 110), include.lowest=TRUE, right=FALSE)
grtxt <- if(N<100){c('0-25', '25-50', "50-75", "75+")} else {
                c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')}
#grtxt <- c(levels(RegData$AldersGr)[-length(gr)], paste0(max(gr),'+'))#paste(gr,sep='-')
levels(RegData$AldersGr) <- grtxt #c(levels(RegData$AldersGr)[-length(gr)], paste0(max(gr),'+'))
TabAlder <- table(RegData$AldersGr, RegData$EnhetsNivaaVar)
TabAlder <- addmargins(TabAlder)
colnames(TabAlder)[ncol(TabAlder)] <- switch(enhetsNivaa,
                                               RHF = 'Totalt',
                                               HF = paste0(valgtRHF, ', totalt'))
return(invisible(UtData <- list(Tab=TabAlder, utvalgTxt=UtData$utvalgTxt)))
}


#' Nøkkeltalltabell - ikke klar
#'
#' @param RegData beredskapsskjema
#' @inheritParams NIRUtvalgBeredsk
#'
#' @return
#' @export
#'
oppsumAldKjTabIkkeKlar <- function(RegData, erMann=9, bekr=9, skjemastatus=9,
                           dodInt=9, reshID=0, valgtRHF='Alle'){

  RegData <- NIRUtvalgBeredsk(RegData=RegData, erMann=erMann, bekr=bekr,
                              skjemastatus=skjemastatus, dodInt=dodInt,
                              reshID=reshID, valgtRHF=reshID)$RegData

  N <- dim(RegData)[1]


  TabAldKj <- rbind(
    'Alder, gjsn.' <- mean(RegData$Alder, na.rm = T),
    'Andel kvinner' <- 100*mean(RegData$erMann, na.rm=T)
  )
  colnames(TabAldKj) <- c('Median', 'IQR', 'Antall', 'Andel')
  TabLiggetider[1:2,'Andel'] <- paste0(sprintf('%.0f', as.numeric(TabLiggetider[1:2,'Andel'])),'%')
  xtable::xtable(TabLiggetider,
                 digits=0,
                 align = c('l','r','c', 'r','r'),
                 caption='Liggetider og ECMO/respiratorbruk, ferdigstilte opphold. \\
                 IQR (Inter quartile range) - 50% av oppholdene er i dette intervallet.')
}
