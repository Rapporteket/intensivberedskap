#Husk neste gang oppdaterer pakken intensiv:
#Felles preprosessfil, felles hentSamlerapport (Rpakke som innparameter)


library(knitr)
library(intensivberedskap)
# CoroData <- read.table('C:/ResultattjenesteGIT/ReadinessFormDataContract2020-03-18.csv', sep=';',
#                        stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
#CoroData <- NIRberedskDataSQL()
valgtRHF <- 'Alle'
valgtRHF <- as.character(NIRPreprosessBeredsk(NIRberedskDataSQL())$RHF[3])
#setwd('C:/ResultattjenesteGIT/intensivberedskap/')
#knit('C:/ResultattjenesteGIT/intensivberedskap/inst/BeredskapCorona.Rnw', encoding = 'UTF-8')
#tools::texi2pdf(file='BeredskapCorona.tex')
knitr::knit2pdf('~/intensivberedskap/inst/BeredskapCorona.Rnw') #, encoding = 'UTF-8')

#/home/rstudio/rap_config/autoReport.yml

#CoroData <- read.table('C:/ResultattjenesteGIT/ReadinessFormDataContract2020-03-18.csv', sep=';',
#                                  stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
library(intensivberedskap)
CoroData <- NIRberedskDataSQL()
RegData <- NIRPreprosessBeredsk(CoroData)
CoroData <- RegData

Data <- RegData[!(is.na(RegData$DateDischargedIntensive)), c("FormStatus", "Bekreftet")]

minald <- 20
maxald <- 55
reshID=0
erMann=0
bekr=1
skjemastatus=2
dodInt=9
valgtRHF <-RegData$RHF[3] #'Alle' #
valgtRHF <- 'Alle'
valgtRHF <- 'Nord'
tidsenhet='dag'
velgAvd=0
reshID <- 102090


TabTidEnhet(RegData=RegData, skjemastatus = 2, erMann=0, valgtRHF ='Nord')$Tab
TabAlder(RegData, valgtRHF='Ukjent')$Tab

oppsumFerdigeRegTab(RegData)$Tab
statusECMOrespTab(RegData)$Tab
statusECMOrespTab(RegData=RegData)
test <- oppsumFerdigeRegTab(RegData)
test <- TabAlder(RegData) #, valgtRHF = valgtRHF)


statusECMOrespTab(CoroData)
RisikofaktorerTab(RegData, erMann = 1)

ut <- TabTidEnhet(RegData=CoroData, tidsenhet='uke', enhetsNivaa='HF')
