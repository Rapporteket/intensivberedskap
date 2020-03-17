#Husk neste gang oppdaterer pakken intensiv:
#Felles preprosessfil, felles hentSamlerapport (Rpakke som innparameter)


library(knitr)
library(intensivberedskap)

setwd('./inst')
#knit('BeredskapCorona.Rnw', encoding = 'UTF-8')
#tools::texi2pdf(file='BeredskapCorona.tex')
knitr::knit2pdf('BeredskapCorona.Rnw') #, encoding = 'UTF-8')


#RisikofaktorerTab(RegData=CoroData, tidsenhet='Aar')
CoroData <- read.table('C:/ResultattjenesteGIT/ReadinessFormDataContract2020-03-17.csv', sep=';',
                                  stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
#CoroData <- NIRberedskDataSQL()
CoroData <- NIRPreprosessBeredsk(CoroData)
RegData <- CoroData


reshID=0
erMann=9
bekr=9
skjemastatus=9
dodInt=9
valgtRHF=RegData$RHF[3] #'Alle' #
tidsenhet='dag'
velgAvd=0

TabTidEnhet(RegData, tidsenhet='dag', valgtRHF =valgtRHF )

statusECMOrespTab(CoroData)

ut <- TabTidEnhet(RegData=CoroData, tidsenhet='uke', enhetsNivaa='HF')
