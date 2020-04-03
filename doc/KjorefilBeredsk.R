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

minald <- 0
maxald <- 110
reshID=0
erMann=0
bekr=9
skjemastatus=9
dodInt=9
valgtRHF <-RegData$RHF[3] #'Alle' #
valgtRHF <- 'Alle'
valgtRHF <- 'Nord'
tidsenhet='dag'
reshID <- 102090

oppsumFerdigeRegTab <- function(RegData, valgtRHF='Alle', bekr=9, erMann=9, dodInt=9)

TabTidEnhet(RegData=RegData, skjemastatus = 2, erMann=0, valgtRHF ='Nord')$Tab
TabAlder(RegData, valgtRHF=valgtRHF)$Tab

oppsumFerdigeRegTab(RegData)$Tab
statusECMOrespTab(RegData)$Tab
statusECMOrespTab(RegData=RegData)
test <- oppsumFerdigeRegTab(RegData)
test <- TabAlder(RegData) #, valgtRHF = valgtRHF)


statusECMOrespTab(CoroData)
RisikofaktorerTab(RegData, erMann = 1)

ut <- TabTidEnhet(RegData=CoroData, tidsenhet='uke', enhetsNivaa='HF')


#--------------------Overføringer-------------------------
library(intensivberedskap)
CoroData <- NIRberedskDataSQL()
RegData <- NIRPreprosessBeredsk(CoroData)

Nopph <- dim(RegData)[1]
Npas <- length(unique(RegData$PasientID))
AntOpphPas <- table(RegData$PasientID)
AntOpphPas[AntOpphPas>1]
EkstraOpph <- Nopph-Npas

#liggetid, #DeadPatientDuring24Hours MoreThan24Hours MovedPatientToAnotherIntensivDuring24Hours
#[41] Municipal                                  MunicipalNumber#Diagnosis
#LastUpdate Dod30 Dod90 [53] Korona
#[57] ECMOTid                                    RespTid
#IsEcmoTreatmentAdministered


#Eksempel på bruk av gruppering I dplyr (tidyverse).
library(tidyverse)
#Kan slå sammen hver enkelt variabel:
tmp <- RegData %>% group_by(PasientID) %>% summarise(mindato = min(InnDato), maxdato = max(InnDato), N=n())

#Jeg lagde forresten denne lille snutten for å ramse opp variablene som har mer enn 1 verdi på samme pasientID:
lengde_unik <- function(x){length(unique(x))}
aux <- RegData %>% group_by(PasientID) %>% summarise_all(lengde_unik)
names(colSums(aux[, -1])[colSums(aux[, -1]) > dim(aux)[1]])

library(intensivberedskap)
library(lubridate)
library(tidyverse)

RegData <- NIRberedskDataSQL()

RegData$Tid <- factor(format(RegData$FormDate, "%Y-%m-%d %H:%M:%S"),
                      levels = format(seq(min(RegData$FormDate), max(RegData$FormDate), by="min"), "%Y-%m-%d %H:%M:%S"))
min(RegData$Tid)
RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$FormDate, tz= 'UTC',
                                            format="%Y-%m-%d %H:%M:%S" )
sort(RegData$Innleggelsestidspunkt)
# test <- lubridate::ymd_hms(RegData$DateDischargedIntensive)
#
RegDataRed <- RegData %>% group_by(PatientInRegistryGuid) %>%
  summarise(sort(DateDischargedIntensive)[1])

testData <- NIRberedskDataSQL()

RegDataRed <- testData %>% group_by(PatientInRegistryGuid) %>%
  summarise(min(format.Date(DateDischargedIntensive, tz='UTC'), na.rm = T))
I
summarise(DateDischargedIntensive = max(ymd_hms(DateDischargedIntensive), na.rm = T))

#sm <- function(x){first(x, order_by = InnDato)}





