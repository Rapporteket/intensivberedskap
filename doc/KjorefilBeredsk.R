#Husk neste gang oppdaterer pakken intensiv:
#Felles preprosessfil, felles hentSamlerapport (Rpakke som innparameter)


library(knitr)
library(intensivberedskap)
CoroData <- read.table('C:/ResultattjenesteGIT/ReadinessFormDataContract2020-03-18.csv', sep=';',
                       stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
valgtRHF <- as.character(NIRPreprosessBeredsk(CoroData)$RHF[3])
setwd('C:/ResultattjenesteGIT/intensivberedskap/inst/')
#knit('C:/ResultattjenesteGIT/intensivberedskap/inst/BeredskapCorona.Rnw', encoding = 'UTF-8')
#tools::texi2pdf(file='BeredskapCorona.tex')
knitr::knit2pdf('C:/ResultattjenesteGIT/intensivberedskap/inst/BeredskapCorona.Rnw') #, encoding = 'UTF-8')



CoroData <- read.table('C:/ResultattjenesteGIT/ReadinessFormDataContract2020-03-18.csv', sep=';',
                                  stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
#CoroData <- NIRberedskDataSQL()
RegData <- NIRPreprosessBeredsk(CoroData)
CoroData <- RegData


reshID=0
erMann=9
bekr=9
skjemastatus=9
dodInt=9
valgtRHF=RegData$RHF[3] #'Alle' #
tidsenhet='dag'
velgAvd=0

test <- TabAlder(RegData) #, valgtRHF = valgtRHF)
TabTidEnhet(RegData, tidsenhet='dag', valgtRHF =valgtRHF )

statusECMOrespTab(CoroData)

ut <- TabTidEnhet(RegData=CoroData, tidsenhet='uke', enhetsNivaa='HF')
