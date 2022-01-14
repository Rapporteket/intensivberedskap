#Husk neste gang oppdaterer pakken intensiv:
#Felles preprosessfil, felles hentSamlerapport (Rpakke som innparameter)


library(knitr)
library(intensivberedskap)
# CoroData <- read.table('C:/ResultattjenesteGIT/ReadinessFormDataContract2020-03-18.csv', sep=';',
#                        stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
#CoroData <- NIRberedskDataSQL()
# valgtRHF <- 'Alle'
# valgtRHF <- as.character(NIRPreprosessBeredsk(NIRberedskDataSQL())$RHF[3])
# 9.jan. 2022: Endrer til parametre reshID og enhetsNivaa
reshID <- 107717 #700720 #
enhetsNivaa <- 'HF'
#setwd('C:/ResultattjenesteGIT/intensivberedskap/')
#knit('C:/ResultattjenesteGIT/intensivberedskap/inst/BeredskapCorona.Rnw', encoding = 'UTF-8')
tools::texi2pdf(file='BeredskapCorona.tex')
knitr::knit('~/intensivberedskap/inst/BeredskapCorona.Rnw') #, encoding = 'UTF-8')
knitr::knit2pdf('~/intensivberedskap/inst/BeredskapCorona.Rnw') #, encoding = 'UTF-8')

#CoroData <- read.table('C:/ResultattjenesteGIT/ReadinessFormDataContract2020-03-18.csv', sep=';',
#                                  stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
library(intensivberedskap)

PasientID = "3644198B-F158-EC11-A978-00155D0B4D1A"


RegDataRaa <- NIRberedskDataSQL(kobleInt = 1)
RegData <- NIRPreprosessBeredsk(RegDataRaa, kobleInt = 1)
CoroDataRaa <- NIRberedskDataSQL(kobleInt = 0)
CoroData <- NIRPreprosessBeredsk(RegData = CoroDataRaa)

test <- CoroDataRaa[CoroDataRaa$PatientInRegistryGuid == "4442C54B-848C-EB11-A970-00155D0B4E21", ]
first(test$CreationDate, order_by = test$FormDate)

ReinnNaar = max(which(difftime(sort(test$FormDate)[2:2],
                               test$DateDischargedIntensive[order(test$FormDate)][1],
                                                   units = 'hours') > 12))+1
ReinnNaar = ifelse(Reinn==0  , 1,
                   ifelse(sum(is.na(test$DateDischargedIntensive))>0,
                     max(which(difftime(sort(FormDate)[2:AntRegPrPas],
                                                   DateDischargedIntensive[order(FormDate)][1:(AntRegPrPas-1)],
                                                   units = 'hours') > 12))+1))

test <- CoroData[!is.na(CoroData$EcmoStart) & CoroData$RHF=='Vest',
                 c("RHF", 'HF',"ShNavn",'HFut', "ShNavnUt", "FormStatus", "FormDate", "DateDischargedIntensive")]

first(RegData$CreationDate, order_by = RegData$FormDate)



unique(RegData[ ,c('ReshId', 'ShNavn')])

RegDataOpph <- NIRPreprosessBeredsk(RegData = NIRberedskDataSQL(), aggPers = 0)
tab <- tabRegForsinkelse(RegData=RegDataOpph, pst = 0) #datoFra='2020-03-01', datoTil=Sys.Date())


table(table(RegDataRaa$PatientInRegistryGuid))
table(RegData$ReinnNaar)
table(RegData$ReinnNaarTest)
pasFeil <- 'E6C9B5FD-661F-EB11-A96D-00155D0B4D16'
pas <- c(RegData$PasientID[RegData$Reinn==1], pasFeil)

testRaa <- RegDataRaa[which(RegDataRaa$PatientInRegistryGuid %in% pas), ]
testPp <- RegData[which(RegData$PasientID %in% pas), ]
table(testRaa$PatientInRegistryGuid)
testRaa[order(testRaa$PatientInRegistryGuid, testRaa$FormDate),
        c("PatientInRegistryGuid", "FormDate", "DateDischargedIntensive", "FormStatus")]

AntRegPrPas <- 2
ReinnNaar <- max(which(ReshId[order(test$FormDate)][2:AntRegPrPas] ==
                               ReshId[order(FormDate)][1:AntRegPrPas-1]))+1


CoroData <- NIRberedskDataSQL(kobleInt = 1)
RegData <- NIRPreprosessBeredsk(RegData = CoroData, kobleInt = 1)

 CoroSjekk <- CoroData[which(CoroData$PatientInRegistryGuid == "C47F1C66-2F11-EB11-A96D-00155D0B4D16"),
                       c("DateAdmittedIntensive", "DateDischargedIntensive", 'FormDate',
                         "PatientInRegistryGuidInt", "PersonId")]
 BeredDataRaa[which(BeredDataRaa$PatientInRegistryGuid == "C47F1C66-2F11-EB11-A96D-00155D0B4D16"),
                      c("DateAdmittedIntensive", "DateDischargedIntensive", 'FormDate',
                        "PersonId")]
 IntDataRaa[which(IntDataRaa$PatientInRegistryGuid == "C47F1C66-2F11-EB11-A96D-00155D0B4D16"),
            c("DateAdmittedIntensive", "DateDischargedIntensive", 'FormDate', "PersonId")]

PersonId <-  '0x2EE6AD0CF8F2F06EC10EAD46C08B2FA5F965C63215A332A2E0707ABDF9E5A33E'
RegData$PersonId==PersonId
which(RegData$PersonId[is.na(RegData$PatientInRegistryGuidInt)]== PersonId)
RegData$PatientInRegistryGuid[21]


RegData <- CoroData

RegData[which(RegData$PatientInRegistryGuid == "C47F1C66-2F11-EB11-A96D-00155D0B4D16"),
         c("DateAdmittedIntensive", "DateDischargedIntensive", 'FormDate', "PatientInRegistryGuidInt")]

as.data.frame(RegDataRed[RegDataRed$PasientID == "C47F1C66-2F11-EB11-A96D-00155D0B4D16", ])

IntDataRaa[IntDataRaa$PatientInRegistryGuid == "C47F1C66-2F11-EB11-A96D-00155D0B4D16",
           c("DateAdmittedIntensive", "DateDischargedIntensive", 'FormDate')]

RegData <- NIRPreprosessBeredsk(RegData <- CoroData)
RegData[which(is.na(RegData$DateDischargedIntensive) & RegData$ShNavn == 'Lovisenberg'), ]




data <- RegData[(RegData$Reinn==1) | (RegData$ReinnGml==1) ,c("PasientID", "ShNavn", "ShNavnUt", "FormDate", "DateDischargedIntensive", "Reinn", "ReinnGml", "ReinnNaar", "ReinnTid")]
pas <- RegData$PasientID[RegData$Reinn==1 | RegData$ReinnGml==1]
dataRaa <- CoroData[CoroData$PatientInRegistryGuid %in% pas ,c("PatientInRegistryGuid", "FormDate", "HelseenhetKortnavn", "DateDischargedIntensive")]
dataRaa <- dataRaa[order(dataRaa$PatientInRegistryGuid, dataRaa$FormDate), ]

data <- NIRUtvalgBeredsk(RegData=RegData, datoTil = '2020-04-01')$RegData
inneliggere <- is.na(data$DateDischargedIntensive)
inne <- sum(inneliggere)
range(data$Liggetid, na.rm = T)
data[inneliggere, c('FormDate', "PasientID", "ShNavnUt")]

pas <- data$PasientID[inneliggere]
sjekkSkjema <- CoroData[which(CoroData$PatientInRegistryGuid %in% pas),
                        c("HelseenhetKortnavn", 'PatientInRegistryGuid', "FormDate", "DateDischargedIntensive","SkjemaGUID")]
sjekkSkjema[order(sjekkSkjema$HelseenhetKortnavn, sjekkSkjema$PatientInRegistryGuid, sjekkSkjema$FormDate),]

# sjekkSkjema <- CoroData[which(is.na(CoroData$DateDischargedIntensive) & as.Date(CoroData$FormDate)<'2020-04-01'),
#                         c("HelseenhetKortnavn","PatientInRegistryGuid", "FormDate", "SkjemaGUID")]
# sjekkSkjema[order(sjekkSkjema$HelseenhetKortnavn, sjekkSkjema$PatientInRegistryGuid, sjekkSkjema$FormDate),]


bekr <- RegData$Bekreftet==1
min(RegData$FormDate[inneliggere & bekr], na.rm=T)
min(RegData$FormDate, na.rm=T)
test <- RegData[inneliggere & bekr, ]

sort(RegData$RespReinnTid)
pas <- RegData$PasientID[which(RegData$ReinnTid > 25)] #ReinnTid< -10 =dobbeltregistrering
RegData$AntRegPas[which(RegData$PasientID %in% pas)]
data <- CoroData[which(CoroData$PatientInRegistryGuid %in% pas), ]
data[order(data$PatientInRegistryGuid, data$DateAdmittedIntensive),]

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
library(tidyverse)
#risikofaktorer per aldersgruppe
RegData <- NIRberedskDataSQL()
RegData <- NIRPreprosessBeredsk()
RegData <- NIRUtvalgBeredsk(RegData = RegData, bekr = 1)$RegData
gr <- c(0, 30,40,50,60,70,80) #seq(30, 90, 10))


#--------------------Overføringer-------------------------
library(intensivberedskap)
library(tidyverse)
library(lubridate)
RegData <- NIRberedskDataSQL()
RegData <- NIRPreprosessBeredsk(RegData)

Nopph <- dim(RegData)[1]
Npas <- length(unique(RegData$PasientID))
AntOpphPas <- table(RegData$PasientID)
AntOpphPas[AntOpphPas>1]
EkstraOpph <- Nopph-Npas



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
test <- RegData[3:4,
  c("MechanicalRespiratorStart", "MechanicalRespiratorEnd")]

Ut <-

#RegData$PatientInRegistryGuid %in% c('013F0C5E-7B6E-EA11-A96B-00155D0B4F09','03254625-EF73-EA11-A96B-00155D0B4F09'),

min(test[2:4])
RegData$Tid <- factor(format(RegData$FormDate, "%Y-%m-%d %H:%M:%S"),
                      levels = format(seq(min(RegData$FormDate), max(RegData$FormDate), by="min"), "%Y-%m-%d %H:%M:%S"))
min(RegData$Tid)
RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$FormDate, tz= 'UTC',
                                            format="%Y-%m-%d %H:%M:%S" )
sort(RegData$Innleggelsestidspunkt)
RegDataRed <- RegData %>% group_by(PatientInRegistryGuid) %>%
  summarise(sort(DateDischargedIntensive)[1])

testData <- NIRberedskDataSQL()

RegDataRed <- testData %>% group_by(PatientInRegistryGuid) %>%
  summarise(min(format.Date(DateDischargedIntensive, tz='UTC'), na.rm = T))
summarise(DateDischargedIntensive = max(ymd_hms(DateDischargedIntensive), na.rm = T))

RegData <- NIRberedskDataSQL()
antPer <- table(RegData$PatientInRegistryGuid)
PID <- names(antPer[antPer==2])
test <- RegData[RegData$PatientInRegistryGuid %in% PID,
                c('PatientInRegistryGuid', 'FormDate',"DateDischargedIntensive",
                  "MechanicalRespiratorStart", "MechanicalRespiratorEnd", 'AgeAdmitted')]


#------------------Sjekk beregning antall inneliggende---------------------------------

#Benytter aggregerte data ved beregning. Kan få avvik når personer er ute av intensiv og tilbake
library(intensivberedskap)
library(tidyverse)
CoroDataRaa <- NIRberedskDataSQL()
CoroDataRaa$HovedskjemaGUID <- toupper(CoroDataRaa$HovedskjemaGUID)
CoroData <- NIRPreprosessBeredsk(RegData = CoroDataRaa)

CoroData$UtDato <- as.Date(CoroData$DateDischargedIntensive, tz= 'UTC', format="%Y-%m-%d")
sum(is.na(CoroData$UtDato)) #Ingen variabel som heter UtDato...
#Evt. hent data koblet med intensivdata

# erInneliggende1 <- function(datoer, regdata){
#   auxfunc <- function(x) {
#     (x >  regdata$InnDato & x <= regdata$UtDato) | (x >  regdata$InnDato & is.na( regdata$UtDato))}
#   map_df(datoer, auxfunc)
# }

erInneliggende <- function(datoer, regdata){
  auxfunc <- function(x) {
    x >  regdata$InnDato & ((x <= regdata$UtDato) | is.na(regdata$UtDato))}
  map_df(datoer, auxfunc)
}

erInneliggendeMut <- function(datoer, regdata){
  regdata <- regdata[!is.na(regdata$UtDato),]
  auxfunc <- function(x) {
    (x >  regdata$InnDato) & (x <= regdata$UtDato)}
  map_df(datoer, auxfunc)
}

  datoer <- seq(as.Date('2020-03-01', tz= 'UTC', format="%Y-%m-%d"), Sys.Date(), by="day")
  names(datoer) <- format(datoer, '%Y-%m-%d') #'%d.%B')
    aux <- erInneliggende(datoer = datoer, regdata = CoroData)
    auxUt <- erInneliggendeMut(datoer = datoer, regdata = CoroData)

    inneliggende <- t(rbind(Dato = names(datoer),
                           Inneliggende = colSums(aux),
                           InneliggendeMut = colSums(auxUt)))

    write.table(inneliggende, file = 'data-raw/inneliggende.csv', sep = ';',  row.names = F, fileEncoding = 'UTF-8')



    #Diagnosis
    # -1 = Velg verdi
    # 100 = Påvist SARS-CoV-2
    # 101 = Påvist SARS-CoV-2 med pneumoni
    # 102 = Påvist SARS-CoV-2 med annen luftveissykdom
    # 103 = Påvist SARS-CoV-2 med annen organmanifestasjon
    # 104 = Mistenkt SARS-CoV-2
    # 105 = Mistenkt SARS-CoV-2 med pneumoni
    # 106 = Mistenkt SARS-CoV-2 med annen luftveissykdom
    # 107 = Mistenkt SARS-CoV-2 med annen organmanifestasjon
    #"Når ein opprettar eit Coronaskjema har ein per def. Mistanke om Corona. Vi meiner difor at skjema med verdi -1 også bør tellast med som mistenkt Corona." Antar dette også gjelder Corona.

