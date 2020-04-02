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

#"liggetid", #"DeadPatientDuring24Hours" "MoreThan24Hours" "MovedPatientToAnotherIntensivDuring24Hours"
#[41] "Municipal"                                  "MunicipalNumber"#"Diagnosis"
#"LastUpdate" "Dod30" "Dod90" [53] "Korona"
#[57] "ECMOTid"                                    "RespTid"
"AgeAdmitted"
"Astma"
"DateAdmittedIntensive"
"DateDischargedIntensive"
"Diabetes"
"DischargedIntensivStatus"
[11] "EcmoEnd"
"EcmoStart"
[13] "FormDate"
"FormStatus"
[15] "FormTypeId"
"Graviditet"
[17] "Helseenhet"
"HelseenhetKortnavn"
[19] "HF"
"HovedskjemaGUID"
[21] "IsActivSmoker"                              "IsChronicLungDiseasePatient"
[23] "IsChronicNeurologicNeuromuscularPatient"    "IsEcmoTreatmentAdministered"
[25] "IsHeartDiseaseIncludingHypertensionPatient" "IsImpairedImmuneSystemIncludingHivPatient"
[27] "IsKidneyDiseaseIncludingFailurePatient"     "IsLiverDiseaseIncludingFailurePatient"
[29] "IsObesePatient"                             "IsRiskFactor"
[31] "Kreft"
[33] "MajorVersion"                               "MechanicalRespirator"
[35] "MechanicalRespiratorEnd"                    "MechanicalRespiratorStart"
[37] "MinorVersion"
[39] "MorsdatoOppdatert"
[43] "Alder"                                      "PatientGender"
[45] "PasientID"                                  "RHF"
[47] "ShNavn"                                     "Sykehus"
[49] "Overf"                                      "ReshId"
[51] "erMann"                                     "Kjonn"
 "Bekreftet"

 Følger innleggelsestidspunkt:

[55] "InnDato"
[59] "MndNum"                                     "MndAar"
[61] "Kvartal"                                    "Halvaar"
[63] "Aar"                                        "UkeNr"
[65] "Dag"


 RegData <- korona::KoronaPreprosesser(korona::KoronaDataSQL())

 RegData$Vekt


