
#' Funksjon som produserer rapporten som skal sendes til mottager.
#'
#' @param rnwFil Navn på fila som skal kjøres. Angis uten ending (\emph{dvs uten  ".Rnw"})
#' @param reshID Aktuell reshid
#' @param enhetsNivaa Enhetsnivå, 'Alle'-hele landet, 'RHF', 'HF'
#' @param filnavn dummy
#'
#' @return Full path of file produced
#' @export

henteSamlerapporterBered <- function(filnavn, rnwFil, #Rpakke='intensivberedskap', Flyttet 12.jan 2022
                                     reshID=0,
                                     enhetsNivaa = 'Alle'){

  reshID <- as.numeric(reshID)
  tmpFile <- paste0('tmp',rnwFil)
  src <- normalizePath(system.file(rnwFil, package='intensivberedskap'))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  setwd(tempdir())
  file.copy(src, tmpFile, overwrite = TRUE)
  knitr::knit2pdf(tmpFile)

  gc() #Opprydning gc-"garbage collection"
  file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), filnavn)
}


#' Funksjon som produserer rapporten som skal sendes til mottager.
#' Nødvendig info til Covid-rapporten er reshID og enhetsNivaa
#'
#' @param rnwFil Navn på fila som skal kjøres. Angis MED filending (\emph{dvs "filnavn.Rnw"})
#' @param Rpakke hvilken R-pakke fila som lager rapporten ligger i
#' @param valgtRHF Fases ut, men beholdes for at gamle abonnement skal kunne kjøre.
#' @param reshID Bestillers resh-id. Benyttes for abonnement, ikke utsendinger
#' @param enhetsNivaa Enhetsnivå: "Alle" (= hele landet), 'HF', 'RHF'
#' @param nivaaNavn navn på det aktuelle HF/RHF, evt. "Alle" (= hele landet). Benyttes for utsendinger.
#'
#' @return Full path of file produced
#' @export
abonnementBeredsk <- function(rnwFil,
                              brukernavn='beredskap',
                              valgtRHF = 'tom',  #'Alle', #Brukes ikke fra ca 12.jan. 2022
                              reshID=0,  #Beregnes ut fra HF/RHF-navn for utsending, men benyttes direkte for abonnement
                              enhetsNivaa = 'Alle',  #For abonnement
                              nivaaNavn = 'tom',  #Benyttes for utsending
                              Rpakke='intensivberedskap') {

  if (rnwFil %in% c('Alle_BeredskapCorona.Rnw', 'RHF_BeredskapCorona.Rnw', 'HF_BeredskapCorona.Rnw')) {
    dum <- strsplit(rnwFil,'_')
    rnwFil <- 'BeredskapCorona.Rnw'
    enhetsNivaa <- dum[[1]][1]
  }

  filbase <- substr(rnwFil, 1, nchar(rnwFil)-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(brukernavn), '.Rnw')
  src <- normalizePath(system.file(rnwFil, package=Rpakke))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  setwd(tempdir())
  dir <- getwd()
  file.copy(src, tmpFile, overwrite = TRUE)
  knitr::knit2pdf(input=tmpFile)

  #gc() #Opprydning gc-"garbage collection"
  utfil <- paste0(dir, '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')

  return(utfil)
}


#' Koble med intensivdata. Ferdigstilte intensivdata. Kan inneholde beredskapsdata i kladd
#'
#' @return Koblet datasett intensiv- og beredskapsskjema
#' @export
BeredskIntensivData <- function(){

BeredskRaa <- NIRberedskDataSQL()
forsteReg <- min(as.Date(BeredskRaa$FormDate))
IntDataRaa <- intensiv::NIRRegDataSQL(datoFra = forsteReg) #Kun ferdigstilte intensivdata på Rapporteket
#Felles variabler som skal hentes fra intensiv (= fjernes fra beredskap)
varFellesInt <- c('DateAdmittedIntensive', 'DateDischargedIntensive',	'DaysAdmittedIntensiv',
                  'DeadPatientDuring24Hours',	'MechanicalRespirator',	'RHF', 'TransferredStatus',
                  'VasoactiveInfusion',	'MoreThan24Hours',	'Morsdato',
                  'MovedPatientToAnotherIntensivDuring24Hours',	'PatientAge',	'PatientGender',
                  'UnitId') # PatientInRegistryGuid', 'FormStatus', 'ShNavn',
BeredRaa <- BeredskRaa[ ,-which(names(BeredskRaa) %in% varFellesInt)]


#Endrer navn på variabler fra int som fortsatt er med i beredskap
varFellesInt <- intersect(sort(names(IntDataRaa)), sort(names(BeredRaa)))
hvilkeIntvar <- which(names(IntDataRaa) %in% varFellesInt)
names(IntDataRaa)[hvilkeIntvar] <- paste0(names(IntDataRaa)[hvilkeIntvar], 'Int')

BeredIntRaa1 <- merge(BeredRaa, IntDataRaa, #suffixes = c('','Int'),
                      by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = F, all.y=F)
varMed <- c('Age', 'AgeAdmitted', 'IsAsthmaticPatient', 'Bilirubin', 'Birthdate', 'BrainDamage',
            'Bukleie', 'ChronicDiseases', 'IsDiabeticPatient', 'Diagnosis', 'DischargedIntensiveStatus',
            'EcmoEcla', 'EcmoEnd', 'EcmoStart', 'ExtendedHemodynamicMonitoring', 'FrailtyIndex',
            'Glasgow', 'IsPregnant', 'Hco3', 'HeartRate',
            'HovedskjemaGUID', 'Impella', 'Intermitterende', 'IntermitterendeDays',
            'InvasivVentilation', 'IsActiveSmoker', 'IsChronicLungDiseasePatient',
            'IsChronicNeurologicNeuromuscularPatient', 'IsEcmoTreatmentAdministered',
            'IsHeartDiseaseIncludingHypertensionPatient', 'IsImpairedImmuneSystemIncludingHivPatient',
            'IsKidneyDiseaseIncludingFailurePatient', 'IsLiverDiseaseIncludingFailurePatient',
            'IsObesePatient', 'IsolationDaysTotal', 'IsRiskFactor', 'KidneyReplacingTreatment',
            'Kontinuerlig', 'KontinuerligDays', 'IsCancerPatient', 'Leukocytes', 'MechanicalRespirator',
            'MechanicalRespiratorEnd', 'MechanicalRespiratorStart', 'MvOrCpap', 'Nems',
            'NonInvasivVentilation', 'PatientTransferredFromHospital', 'PatientTransferredFromHospitalName',
            'PatientTransferredToHospital', 'PatientTransferredToHospitalName', 'Potassium',
            'PrimaryReasonAdmitted', 'ReshID', 'Respirator', 'Saps2Score', 'Saps2ScoreNumber',
            'SerumUreaOrBun', 'ShType', 'SkjemaGUID', 'Sodium', 'SystolicBloodPressure',
            'Temperature', 'Trakeostomi', 'TypeOfAdmission', 'UrineOutput',
            'PatientInRegistryGuid', 'PersonId') #'ShNavn',
beregnVar <- c('Birthdate', 'FormDate', 'FormStatus', 'HF', 'HealthUnitShortName')
BeredIntRaa <- BeredIntRaa1[ ,c(varMed, varFellesInt, beregnVar)] #c()]

return(UtData <- BeredIntRaa)
}


#' Lag staging data for intensivberedskap
#'
#' This function makes queries and pre-processing of registry data before
#' storing relevant staging data. Running this function may take a while so use
#' with care!
#'
#' @return Character vector of staging files, invisibly
#' @export

lagStagingData <- function() {

   CoroDataRaa <- NIRberedskDataSQL(kobleInt = 0) #OK
   CoroDataRaa$HovedskjemaGUID <- toupper(CoroDataRaa$HovedskjemaGUID)

   CoroData <- NIRPreprosessBeredsk(RegData = CoroDataRaa, aggPers = 1, tellFlereForlop = 1) #TESTES
   BeredDataOpph <- NIRPreprosessBeredsk(RegData = CoroDataRaa, aggPers = 0) #OK

   BeredIntRaa <- NIRberedskDataSQL(kobleInt = 1) #OK
   BeredIntPas <- if (dim(BeredIntRaa)[1]>0) { #OK
     NIRPreprosessBeredsk(RegData = BeredIntRaa, kobleInt = 1, aggPers = 1, tellFlereForlop = 1)
   } else {0}

   InfluData <- NIRsqlPreInfluensa() #OK
   InfluIntData <- NIRsqlPreInfluensa(kobleInt = 1) #OK

  regNavn <- "intensivberedskap"
  rapbase::saveStagingData(registryName = regNavn, "CoroDataRaa", CoroDataRaa)
  rapbase::saveStagingData(regNavn, "CoroData", CoroData)
  rapbase::saveStagingData(regNavn, "BeredDataOpph", BeredDataOpph)
  rapbase::saveStagingData(regNavn, "BeredIntRaa", BeredIntRaa)
  rapbase::saveStagingData(regNavn, "BeredIntPas", BeredIntPas)
  rapbase::saveStagingData(regNavn, "InfluData", InfluData)
  rapbase::saveStagingData(regNavn, "InfluIntData", InfluIntData)

  invisible(rapbase::listStagingData(regNavn))
}
