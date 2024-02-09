#Resultater til NiPars årsrapport for 2022
#Alle resultater skal baseres på opphold
#Legg tabeller inn i Overleaf, men send figurer til Eirik/Eivind

setwd('~/Aarsrappresultater/NiPar22/pandemi')

library(intensiv)
library(intensivberedskap)
#library(korona)
datoFra <- '2020-03-01' #Vi har pandemi fra 1.mars 2020
datoTil <- '2022-12-31'	#
datoFra1aar <- '2022-01-01'

BeredDataRaa <- NIRberedskDataSQL(datoTil = datoTil)
BeredData <- NIRPreprosessBeredsk(RegData = BeredDataRaa, aggPers = 0)

BeredIntDataRaa <- NIRberedskDataSQL(kobleInt = 1, datoTil = datoTil)
BeredIntDataRaa <- BeredIntDataRaa[BeredIntDataRaa$FormStatus==2, ] #Bare ferdigstilte
BeredIntData <- NIRPreprosessBeredsk(RegData = BeredIntDataRaa, kobleInt = 1, aggPers = 0)
BeredIntData1aar <- BeredIntData[which(BeredIntData$InnDato >= datoFra1aar),]
#sum(is.na(BeredIntData1aar$ShNavnInt))
N1aar <- dim(BeredIntData1aar)[1]

#test <- BeredIntData1aar[ ,c('DateAdmittedIntensive', 'InnDato')]
#test$test <- as.Date(BeredIntData1aar$DateAdmittedIntensive) - as.Date(BeredIntData1aar$InnDato)

BeredIntPers <- NIRPreprosessBeredsk(RegData=BeredIntDataRaa, kobleInt = 1, aggPers = 1, tellFlereForlop = 1)
BeredIntPers1aar <- BeredIntPers[which(BeredIntPers$InnDato >= datoFra1aar),]


#Sjekke inneliggende
# CoroDataRaa <- NIRberedskDataSQL(kobleInt = 0)
# CoroDataRaa$HovedskjemaGUID <- toupper(CoroDataRaa$HovedskjemaGUID)
# #Bruk resh før preprosesserer
# CoroData <- NIRPreprosessBeredsk(RegData = CoroDataRaa, aggPers = 1, tellFlereForlop = 1)
# BeredDataOpph <- NIRPreprosessBeredsk(RegData = CoroDataRaa, aggPers = 0)
# BeredIntRaa <- NIRberedskDataSQL(kobleInt = 1)
#
# inneliggere <- which(is.na(CoroDataRaa$DateDischargedIntensive))
# inneliggere <- which(is.na(CoroData$DateDischargedIntensive))
# CoroDataRaa[which(CoroDataRaa$FormStatus==1), c('DateDischargedIntensive')]
#
#
# status <- statusECMOrespTab(RegData=CoroData)
# status$Tab
#
# inneligg <- is.na(CoroData$DateDischargedIntensive)
# RegHF <- CoroData[inneligg,] %>% dplyr::group_by(RHFut, HFut) %>% dplyr::summarise(Antall = n(), .groups = 'keep')
# colnames(RegHF) <- c('RHF', 'HF', 'Antall')
# RegHF


#----------------Fordelingsfigurer, basert på opphold-----------------
variablerInt <- c( 'alder', 'frailtyIndex', 'inklKrit', 'respiratortidInvMoverf',
                   'komplikasjoner', 'NEMS24', 'respiratortid',
                   'respiratortidNonInv', 'SAPSII', 'spesTiltak')
variablerInt <-
for (valgtVar in variablerInt) {
  NIRFigAndeler(RegData=BeredIntData1aar,
                datoFra = datoFra1aar, #Tar høyde for avvik i inndato mellom intensiv-og beredskapsskjema
                valgtVar=valgtVar,
                velgDiag = 1,
                outfile = paste0('BeredFord_', valgtVar, '.pdf'))
}

variablerBer <- c('regForsinkelseInn', 'regForsinkelseUt')

for (valgtVar in variablerBer) {
  NIRberedskFigAndeler(RegData=BeredIntData1aar,
                       datoFra = datoFra1aar,
                       bekr = 1,
                          valgtVar=valgtVar,
                          outfile = paste0('BeredFord_', valgtVar, '.pdf'))
}

#-------Andel per enhet------------------
variablerInt <- c('dod30d', 'dodeIntensiv')

for (valgtVar in variablerInt) {
  for (grType in 2:3) {
    grTxt <- c('ls', 'ls', 'reg')[grType]
    NIRFigAndelerGrVar(RegData = BeredIntData1aar,
                   datoFra = datoFra1aar, #Tar høyde for avvik i inndato mellom intensiv-og beredskapsskjema
                   valgtVar=valgtVar,
                   grType = grType,
                   velgDiag = 1,
                   outfile = paste0('BeredAndelSh_', valgtVar, '_', grTxt, '.pdf')
                   )
}}

NIRFigAndelerGrVar(RegData = BeredIntData1aar,
                   datoFra = datoFra1aar,
                   valgtVar='dodeIntensiv',
                   velgDiag = 1,
                   outfile = paste0('BeredAndelSh_dodeIntensiv.pdf'))

#-------Andel per tid------------------
variablerAndelTid <- c('bukleie', 'invasivVent', 'respiratortidDod', 'trakeostomi', 'utenforVakttidUt')
for (valgtVar in variablerAndelTid) {
  NIRFigAndelTid(RegData=BeredIntData,
                 preprosess = 0,
                 datoFra = datoFra,
                 tidsenhet = 'Kvartal',
                 velgDiag = 1,
                 valgtVar=valgtVar,
                 outfile = paste0('BeredAndelTid_', valgtVar, '.pdf'))
}
BeredIntData$Aar <- as.numeric(as.character(BeredIntData$Aar))
NIRFigAndelTid(RegData=BeredIntData,  valgtVar='beredMpand_opph', preprosess = 0,
               tidsenhet = 'Kvartal', velgDiag = 1,
               outfile = paste0('BeredAndelTid_beredMpand_opph.pdf'))

  # NIRFigAndelTid(RegData=BeredIntPers,  valgtVar='beredMpand_pers', preprosess = 0,
  #                tidsenhet = 'Kvartal',velgDiag = 1,
  #                outfile = paste0('BeredAndelTid_beredMpand_smForl.pdf'))

#-------Gjsn per tid------------------
variablerAndelGjsn <- c('liggetid', 'NEMS', 'NEMS24', 'SAPSII',
                        'respiratortidInvMoverf', 'respiratortidNonInv')
valgtMaal <- 'Med'
for (valgtVar in variablerAndelGjsn) {
  NIRFigGjsnTid(RegData=BeredIntData,
                 datoFra = datoFra,
                 tidsenhet = 'Kvartal',
                 valgtMaal = valgtMaal,
                 velgDiag = 1,
                 valgtVar=valgtVar,
                 outfile = paste0('Bered', valgtMaal, 'Tid_', valgtVar, '.pdf'))
}


#----------------Tabeller----------------------------

#Har alle beredskapsskjema intensivskjema? Nei
# test <- ManglerIntSkjema(datoFra=datoFra1aar, datoTil = datoTil)
# utils::write.table(test, file = 'BeredUint.csv', row.names = F, fileEncoding = 'UTF-8', sep = ';')

#Nøkkeltall.
tab <- intensivberedskap::oppsumFerdigeRegTab(BeredIntData1aar)
xtable::xtable(tab$Tab, label = 'tab:berkey',
               caption = paste0('Nøkkeltal basert på opphald for intensivpasientar med covid-19, N=', tab$Ntest))
#Legg til nyreerstattende behandling
#Nyreerstattende behandling, \\ kontinuerlig &  &  &  &
sum(BeredIntData1aar$Kontinuerlig == TRUE) #&
round(100*sum(BeredIntData1aar$Kontinuerlig == TRUE)/N1aar,1) #\\
#Nyreerstattende behandling, \\ intermitterende &  &  &  &
sum(BeredIntData1aar$Intermitterende == TRUE) #&
round(100*sum(BeredIntData1aar$Intermitterende == TRUE)/N1aar, 1) #\\


#Testing
  # BeredIntData2020 <- BeredIntData[which(BeredIntData$InnDato <= '2020-12-31'),]
  # N1aar <- dim(BeredIntData2020)[1]
  # sum(BeredIntData2020$Kontinuerlig == TRUE) #&
  # round(100*sum(BeredIntData2020$Kontinuerlig == TRUE)/N1aar,1) #\\
  # #Nyreerstattende behandling, \\ intermitterende &  &  &  &
  # sum(BeredIntData2020$Intermitterende == TRUE) #&
  # round(100*sum(BeredIntData2020$Intermitterende == TRUE)/N1aar, 1) #\\



tab <- intensivberedskap::RisikofaktorerTab(RegData = BeredIntData1aar)
txt <- paste0('Dei ', N1aar, ' opphalda er generert av ',
              length(unique(BeredIntData1aar$PasientID)), ' pasientar.')
xtable::xtable(tab$Tab, label = 'tab:berrisk',
               caption = txt)



