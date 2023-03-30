#Resultater til NiPars årsrapport for 2021
#Alle resultater skal baseres på opphold

setwd('../speil/aarsrapp/beredskap')

library(intensiv)
library(intensivberedskap)
library(korona)
datoFra <- '2020-03-01' #Vi har pandemi fra 1.mars 2020
datoTil <- '2021-12-31'	#
datoFra1aar <- '2021-01-01'

BeredDataRaa <- NIRberedskDataSQL(datoTil = datoTil)
BeredData <- NIRPreprosessBeredsk(RegData = BeredDataRaa, aggPers = 0)

BeredIntDataRaa <- NIRberedskDataSQL(kobleInt = 1, datoTil = datoTil)
BeredIntData <- NIRPreprosessBeredsk(RegData = BeredIntDataRaa, kobleInt = 1, aggPers = 0)
BeredIntData1aar <- BeredIntData[which(BeredIntData$InnDato >= datoFra1aar),]
#sum(is.na(BeredIntData1aar$ShNavnInt))
N1aar <- dim(BeredIntData1aar)[1]

#----------------Fordelingsfigurer, basert på opphold-----------------
variablerInt <- c( 'alder', 'frailtyIndex', 'inklKrit', 'respiratortidInvMoverf',
                   'komplikasjoner', 'respiratortid', 'NEMS24',
                   'respiratortidNonInv', 'SAPSII')

for (valgtVar in variablerInt) {
  NIRFigAndeler(RegData=BeredIntData1aar,
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

#-------Andel per tid------------------
variablerAndelTid <- c('bukleie', 'invasivVent', 'respiratortidDod', 'trakeostomi', 'utenforVakttidUt')
for (valgtVar in variablerAndelTid) {
  NIRFigAndelTid(RegData=BeredIntData,
                 datoFra = datoFra,
                 tidsenhet = 'Kvartal',
                 velgDiag = 1,
                 valgtVar=valgtVar,
                 outfile = paste0('BeredAndelTid_', valgtVar, '.pdf'))
}

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

#Har alle beredskapsskjema intensivskjema? JA
#ManglerIntSkjema(datoFra=datoFra1aar, datoTil = datoTil)

tab <- intensivberedskap::oppsumFerdigeRegTab(BeredIntData1aar)
xtable::xtable(tab$Tab, label = 'tab:berkey',
               caption = 'Nøkkeltal for intensivpasientar med covid-19')

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



