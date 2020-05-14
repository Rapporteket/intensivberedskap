#Fil med samling av funksjoner som lager tabeller for beredskapsskjema

#' Antall tilfeller for valgt tidsenhet og enhetsnivå. Filtreringer kan også gjøres.
#'
#' @param RegData dataramme med preprossesserte data
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned'
#' @param enhetsNivaa 'RHF', 'HF', 'ShNavn'
#' @param datoFra Vis hendelser fra og med dato
#' @inheritParams NIRUtvalgBeredsk
#'
#' @return
#' @export
TabTidEnhet <- function(RegData, tidsenhet='dag', erMann=9, resp=9, datoFra=0,
                        bekr=9, skjemastatus=9, dodInt=9, valgtRHF='Alle', velgAvd=0){

  # RegData$TidsVar <- RegData[ ,switch (tidsenhet,
  #                                      dag = 'Dag',
  #                                      uke = 'UkeNr',
  #                                      maaned = 'MndAar')]

  UtData <- NIRUtvalgBeredsk(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
                             bekr=bekr, skjemastatus=skjemastatus, resp=resp,
                             dodInt=dodInt) #, valgtRHF=valgtRHF) #velgAvd=velgAvd

  RegDataAlle <- UtData$RegData
  if (datoFra != 0) {RegDataAlle <- RegDataAlle[which(RegDataAlle$InnDato >= datoFra), ]}
  RegDataAlle$TidsVar <- switch (tidsenhet,
                                 dag = factor(format(RegDataAlle$InnDato, '%d.%B'),
                                              levels = format(rev(seq(Sys.Date(), if (datoFra!=0) datoFra else min(RegDataAlle$InnDato),
                                                                      by=paste0('-1 day'))), '%d.%B')),
                                 uke = factor(paste0('Uke ', format(RegDataAlle$InnDato, '%V')),
                                              levels = paste0('Uke ', format(rev(seq(Sys.Date(), if (datoFra!=0) datoFra else min(RegDataAlle$InnDato),
                                                                                     by=paste0('-1 week'))), '%V'))),
                                 maaned = factor(format(RegDataAlle$InnDato, '%b.%Y'),
                                                 levels = format(rev(seq(Sys.Date(), if (datoFra!=0) datoFra else min(RegDataAlle$InnDato),
                                                                         by=paste0('-1 month'))), '%b.%Y')))
  RegDataAlle <- RegDataAlle[!is.na(RegDataAlle$TidsVar), ]
  RegData <- if(valgtRHF=='Alle') {RegDataAlle} else {RegDataAlle[RegDataAlle$RHF == valgtRHF, ]}
  Ntest <- dim(RegData)[1]

  if (valgtRHF == 'Ukjent'){
    TabTidEnh <- as.matrix(c(table(RegDataAlle$TidsVar), dim(RegDataAlle)[1]), ncol=1)
    colnames(TabTidEnh) <- 'Hele landet'
  } else {

    enhetsNivaa <- ifelse(as.character(valgtRHF)=='Alle', 'RHF', 'HF')

    RegData$EnhetsNivaaVar <- RegData[ ,enhetsNivaa]
    kolNavnSum <- switch(enhetsNivaa,
                         RHF = 'Hele landet',
                         HF = paste0(valgtRHF, ', totalt'))
    if (Ntest==0) {
      TabTidEnh <- matrix(0, ncol=1, nrow=length(levels(RegData$TidsVar)) + 1,
                          dimnames = list(c(levels(RegData$TidsVar), 'Totalt (fra 10.mars)'), valgtRHF)) #table(RegData$TidsVar)
    }else{
      TabTidEnh <- table(RegData[ , c('TidsVar', enhetsNivaa)]) #ftable(RegData[ , c(TidsVar, enhetsNivaa, 'Korona')], row.vars =TidsVar)
      TabTidEnh <- addmargins(TabTidEnh, FUN=list('Totalt'=sum, 'Hele landet' = sum), quiet=TRUE)
      colnames(TabTidEnh)[ncol(TabTidEnh)] <- kolNavnSum
    }
    if (valgtRHF != 'Alle'){
      TabTidEnh <- cbind(TabTidEnh,
                         'Hele landet'= c(table(RegDataAlle$TidsVar), dim(RegDataAlle)[1]))}
  }
  TabTidEnh_tidy <- tidyr::as_tibble(as.data.frame.matrix(TabTidEnh), rownames='Tid')
  TabTidEnh <- xtable::xtable(TabTidEnh, digits=0, #method='compact', #align=c('l', rep('r', ncol(alderDIV))),
                              caption='Antall Coronatilfeller.')

  return(UtData <- list(Tab=TabTidEnh, utvalgTxt=UtData$utvalgTxt, Ntest=dim(RegData)[1],
                        Tab_tidy=TabTidEnh_tidy))
}




#' Antall som er  i ECMO/respirator
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
  inneliggere <- is.na(RegData$DateDischargedIntensive)
  AntPaaIntNaa <- sum(inneliggere) #N - sum(!(is.na(RegData$DateDischargedIntensive)))
  LiggetidNaa <- as.numeric(difftime(Sys.Date(), RegData$FormDateSiste[inneliggere], units='days'))
  LiggetidNaaGjsn <- mean(LiggetidNaa[LiggetidNaa < 60], na.rm = T)

  respLiggere <- inneliggere & is.na(RegData$MechanicalRespiratorEnd) & !(is.na(RegData$MechanicalRespiratorStart) ) #Har antatt at respiratortid MÅ registreres
  AntIrespNaa <- sum(respLiggere)
  ResptidNaa <- as.numeric(difftime(Sys.Date(), RegData$MechanicalRespiratorStartSiste[respLiggere],
                                    units='days'))
  ResptidNaaGjsn <- mean(ResptidNaa[ResptidNaa < 60], na.rm=T)
  #sjekkLiggetidResp <- as.numeric(mean(difftime(Sys.Date(), RegData$Innleggelsestidspunkt[respLiggere], units='days')))

  ECMOLiggere <- inneliggere & is.na(RegData$EcmoEnd) & !(is.na(RegData$EcmoStart) )
  AntIECMONaa <- sum(ECMOLiggere) #sum(!(is.na(RegData$EcmoStart))) - sum(!(is.na(RegData$EcmoEnd)))
  ECMOtidNaa <- as.numeric(difftime(Sys.Date(), RegData$EcmoStart[ECMOLiggere],
                                    units='days'))
  ECMOtidNaaGjsn <- ifelse(AntIECMONaa==0, 0,
                           mean(ECMOtidNaa[ECMOtidNaa < 60], na.rm=T))

  TabHjelp <- rbind(
    'På ECMO nå' = c(AntIECMONaa*(c(1, 100/AntPaaIntNaa)), ECMOtidNaaGjsn),
    'På respirator nå' = c(AntIrespNaa*(c(1, 100/AntPaaIntNaa)), ResptidNaaGjsn),
    'På intensiv nå' = c(AntPaaIntNaa,'', LiggetidNaaGjsn)
  )
  colnames(TabHjelp) <- c('Antall', 'Andel', 'Liggetid (gj.sn.)')
  TabHjelp[1:2,'Andel'] <- paste0(sprintf('%.0f', as.numeric(TabHjelp[1:2,'Andel'])),'%')
  TabHjelp[1:3, 3] <- paste0(sprintf('%.1f', as.numeric(TabHjelp[1:3, 3])), ' døgn')
  xtable::xtable(TabHjelp,
                 digits=0,
                 align = c('l','r','r','r'),
                 caption='Bruk av Respirator/ECMO.')
  UtData <- list(Tab=TabHjelp, utvalgTxt=UtData$utvalgTxt, PaaIntensivNaa=inneliggere)
  return(UtData)
}



#' Ferdigstilte registreringer
#'
#' @param RegData beredskapsskjema
#' @inheritParams NIRUtvalgBeredsk
#'
#' @return
#' @export
#'
oppsumFerdigeRegTab <- function(RegData, valgtRHF='Alle', bekr=9, erMann=9, resp=9, dodInt=9){

  if (valgtRHF == 'Ukjent') {valgtRHF <- 'Alle'}
  UtData <- NIRUtvalgBeredsk(RegData=RegData, valgtRHF=valgtRHF,
                             bekr = bekr,
                             dodInt = dodInt,
                             resp=resp,
                             erMann = erMann,
                             skjemastatus=2)
  RegData <- UtData$RegData
  N <- dim(RegData)[1]
  ##MechanicalRespirator Fått respiratorstøtte. Ja=1, nei=2,
  AntBruktResp <- sum(RegData$MechanicalRespirator==1, na.rm=T)
  AntBruktECMO <- sum(RegData$ECMOTid>0, na.rm=T)
  #AntUtInt <- sum(RegData$DateDischargedIntensive>0, na.rm=T)
  Liggetid <- summary(RegData$Liggetid, na.rm = T)
  RespTid <- summary(RegData$RespTid, na.rm = T)
  ECMOtid <- summary(RegData$ECMOTid, na.rm = T)
  Alder <- summary(RegData$Alder, na.rm = T)
  AntDod <- sum(RegData$DischargedIntensivStatus==1, na.rm=T)

  med_IQR <- function(x){
    #x[is.na(x)]<-0
    c(sprintf('%.1f',x[4]), sprintf('%.1f',x[3]), paste(sprintf('%.1f',x[2]), sprintf('%.1f',x[5]), sep=' - '))
  }
  # x <- Liggetid
  #  test <- sprintf('%.2f',c(x[2],x[5]))
  # test <- med_IQR(ECMOtid)
  TabFerdigeReg <- rbind(
    'ECMO-tid (døgn)' = c(med_IQR(ECMOtid), AntBruktECMO*(c(1, 100/N))),
    'Respiratortid (døgn)' = c(med_IQR(RespTid), AntBruktResp*(c(1, 100/N))),
    'Liggetid (døgn)' = c(med_IQR(Liggetid), N, ''),
    'Alder (år)' = c(med_IQR(Alder), N, ''),
    'Døde' = c('','','',AntDod, paste0(sprintf('%.f',100*AntDod/N),'%'))
  )
  #TabFerdigeReg[TabFerdigeReg==NA]<-""
  colnames(TabFerdigeReg) <- c('Gj.sn', 'Median', 'IQR', 'Antall opphold', 'Andel opphold')
  TabFerdigeReg[c(1:2),'Andel opphold'] <-
    paste0(sprintf('%.0f', as.numeric(TabFerdigeReg[c(1:2),'Andel opphold'])),'%')
  xtable::xtable(TabFerdigeReg,
                 digits=0,
                 align = c('l','r','r','c', 'r','r'),
                 caption='Ferdigstilte opphold.
                 IQR (Inter quartile range) - 50% av oppholdene er i dette intervallet.')
  return(invisible(UtData <- list(Tab=TabFerdigeReg,
                                  utvalgTxt=UtData$utvalgTxt,
                                  Ntest=N)))
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
                              erMann='', bekr=9, skjemastatus=9, dodInt=9, valgtRHF='Alle',
                              resp=9, minald=0, maxald=110, velgAvd=0){

  UtData <- NIRUtvalgBeredsk(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
                             bekr=bekr, skjemastatus=skjemastatus,dodInt=dodInt,
                             minald=minald, maxald=maxald, resp=resp,
                             reshID=reshID, valgtRHF=valgtRHF) #velgAvd=velgAvd
  Ntest <- dim(UtData$RegData)[1]
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

  if (Ntest>3){
    TabRisiko <- as.table(addmargins(TabRisiko, margin = 2))
    if (tidsenhet=='Totalt'){TabRisiko <- as.matrix(TabRisiko[,"Sum"], ncol=1)
    colnames(TabRisiko) <- 'Sum'}
    TabRisiko <- cbind(TabRisiko,
                       'Andel' = paste0(sprintf('%.0f', 100*TabRisiko[,"Sum"]/dim(RegData)[1]),'%'))


    TabRisiko <- rbind(TabRisiko,
                       'Tot. antall (N)' = c(dim(RegData)[1], ''))

  }
  # xtable::xtable(TabRisiko,
  #                digits=0,
  #                align = c('l',rep('r',ncol(TabRisiko))),
  #                caption='Risikofaktorer')
  return(UtData <- list(Tab=TabRisiko, utvalgTxt=UtData$utvalgTxt, Ntest=Ntest))
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
TabAlder <- function(RegData, valgtRHF='Alle', bekr=9, skjemastatus=9,resp=9,
                     dodInt=9,erMann=9){#enhetsNivaa='RHF'

  #if (valgtRHF != 'Alle'){RegData$RHF <- factor(RegData$RHF, levels=unique(c(levels(as.factor(RegData$RHF)), valgtRHF)))}
  RegData$RHF <- as.factor(RegData$RHF)
  UtData <- NIRUtvalgBeredsk(RegData=RegData,
                             #valgtRHF=valgtRHF,
                             resp=resp,
                             bekr=bekr,
                             dodInt = dodInt,
                             erMann = erMann,
                             skjemastatus=skjemastatus
  )
  RegData <- UtData$RegData

  # enhetsNivaa <- ifelse(as.character(valgtRHF)=='Alle', 'RHF', 'RHF') #'HF')
  RegData$EnhetsNivaaVar <- RegData$RHF #RegData[ , enhetsNivaa]

  N <- dim(RegData)[1]
  gr <- seq(0, 90, ifelse(N<100, 25, 10) )
  RegData$AldersGr <- cut(RegData$Alder, breaks=c(gr, 110), include.lowest=TRUE, right=FALSE)
  grtxt <- if(N<100){c('0-24', '25-49', "50-74", "75+")} else {
    c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')}
  #grtxt <- c(levels(RegData$AldersGr)[-length(gr)], paste0(max(gr),'+'))#paste(gr,sep='-')
  levels(RegData$AldersGr) <- grtxt #c(levels(RegData$AldersGr)[-length(gr)], paste0(max(gr),'+'))
  TabAlder <- table(RegData$AldersGr, RegData$EnhetsNivaaVar)
  TabAlder <- addmargins(TabAlder) #switch(enhetsNivaa, RHF = 'Totalt', HF = paste0(valgtRHF, ', totalt'))
  TabAlderPst <-prop.table(TabAlder[-nrow(TabAlder),],2)*100
  #paste0(sprintf('%.0f', as.numeric(TabHjelp[1:2,'Andel'])),'%')

     TabAlderAlle <- cbind(
       'Antall, tot.' = TabAlder[,'Sum'],
       'Andel, tot.' = paste0(sprintf('%.0f', c(TabAlderPst[,'Sum'], 100)), ' %')
     )
     TabAlderUt <-  if (valgtRHF %in% levels(RegData$RHF)){
       TabAlderUt <- cbind(
         'Antall, eget' = TabAlder[ ,valgtRHF],
         'Andel, eget' = paste0(sprintf('%.0f', c(TabAlderPst[ ,valgtRHF], 100)), ' %'),
         TabAlderAlle)
       } else {TabAlderAlle}

  return(invisible(UtData <-
                     list(Tab=TabAlderUt,
                          utvalgTxt=c(UtData$utvalgTxt, paste0('Valgt RHF: ', valgtRHF)))))
}
# if (valgtRHF == 'Ukjent') {
#   TabAlder <- as.matrix(TabAlder[,ncol(TabAlder)], ncol=1)
# } else {
#   if (valgtRHF != 'Alle') {TabAlder <- TabAlder[,c(valgtRHF, 'Sum')]}}
# colnames(TabAlder)[ncol(TabAlder)] <- 'Hele landet'

