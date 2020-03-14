#Husk neste gang oppdaterer pakken intensiv:
#Felles preprosessfil, felles hentSamlerapport (Rpakke som innparameter)


library(knitr)
library(intensivberedskap)

setwd('./inst')
#knit('BeredskapCorona.Rnw', encoding = 'UTF-8')
#tools::texi2pdf(file='BeredskapCorona.tex')
knitr::knit2pdf('BeredskapCorona.Rnw') #, encoding = 'UTF-8')


#RisikofaktorerTab(RegData=CoroData, tidsenhet='Aar')
CoroData <- NIRberedskDataSQL()
CoroData <- NIRPreprosessBeredsk(CoroData)
RegData <- CoroData


reshID=0
erMann=1
bekr=9
skjemaStatus=9
dodInt=9
velgRHF=0
velgAvd=0

statusECMOrespTab(CoroData)

ut <- TabTidEnhet(RegData=CoroData, tidsenhet='uke', enhetsNivaa='HF')
