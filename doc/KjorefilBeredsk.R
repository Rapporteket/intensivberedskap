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

statusECMOrespTab(CoroData)

ut <- TabTidEnhet(RegData=CoroData, tidsenhet='uke', enhetsNivaa='HF')
