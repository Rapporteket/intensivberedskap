
#' Funksjon som produserer rapporten som skal sendes til mottager.
#'
#' @param rnwFil Navn på fila som skal kjøres. Angis uten ending (\emph{dvs uten  ".Rnw"})
#' @param reshID Aktuell reshid
#' @param filnavn dummy
#' @param datoFra dato
#' @param Rpakke hvilken R-pakke fila som lager rapporten ligger i
#' @param parametre Liste med valgfrie parametre, avhengig av type rapport
#'
#' @return Full path of file produced
#' @export

henteSamlerapporterBered <- function(filnavn, rnwFil, reshID=0, Rpakke='intensivberedskap',
                                     valgtRHF = 'Alle', #rolle='LU',
                                datoFra=Sys.Date()-180, datoTil=Sys.Date()) {
  #valgtRHF <- ifelse(rolle=='SC', valgtRHF=valgtRHF, }
  tmpFile <- paste0('tmp',rnwFil)
  src <- normalizePath(system.file(rnwFil, package=Rpakke))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  #owd <-
  setwd(tempdir())
  file.copy(src, tmpFile, overwrite = TRUE)

  knitr::knit2pdf(tmpFile)

  gc() #Opprydning gc-"garbage collection"
  file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), filnavn)
  # file.rename(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
}

#' Funksjon som produserer rapporten som skal sendes til mottager.
#' (The actual call to this function is made through do.call and
#' has the effect of providing the parameters as class
#' \emph{list}. Verdier gis inn som listeparametre
#'
#' @param rnwFil Navn på fila som skal kjøres. Angis MED filending (\emph{dvs "filnavn.Rnw"})
#' @param reshID Aktuell reshid
#' @param datoFra dato
#' @param Rpakke hvilken R-pakke fila som lager rapporten ligger i
#' @param parametre Liste med valgfrie parametre, avhengig av type rapport
#'
#' @return Full path of file produced
#' @export
abonnementBeredsk <- function(rnwFil, brukernavn='beredskap', reshID=0,
                              valgtRHF = 'Alle',
                       #datoFra=Sys.Date()-180, datoTil=Sys.Date(),
                       Rpakke='intensivberedskap') {

  #function(baseName, reshId, registryName,author, hospitalName, type) {
valgtRHF <- valgtRHF[[1]]
  #datoFra <- datoFra[[1]]
  #datoTil <- datoTil[[1]]
  reshID <- reshID[[1]]
  raplog::subLogger(author = brukernavn, registryName = 'NIR - Beredskap',
                    reshId = reshID,
                    msg = "starter Abonnement: Corona-rapport")
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
  raplog::subLogger(author = brukernavn, registryName = 'NIR - beredskap',
                    reshId = reshID,
                    msg = paste("Leverer: ", utfil))
  return(utfil)
}


#' Koble med intensivdata. Ferdigstilte intensivdata. Kan inneholdeberedskapsdata i kladd
#'
#' @return
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
#names(IntDataRaa) #Enders når vi har bestemt hvilke variabler vi skal ha med
#varIKKEmed <- CerebralCirculationAbolished	CerebralCirculationAbolishedReasonForNo	CurrentMunicipalNumber	DistrictCode	Eeg	FormStatus	FormTypeId	HF	HFInt	Hyperbar	Iabp	Icp	Isolation	LastUpdate	Leverdialyse	MajorVersion	MinorVersion	MorsdatoOppdatert	Municipal	MunicipalNumber	Nas	No	OrganDonationCompletedReasonForNoStatus	OrganDonationCompletedStatus	Oscillator	PIM_Probability	PIM_Score	PostalCode	RHF	Sykehus	TerapetiskHypotermi	UnitIdInt
BeredIntRaa1 <- merge(BeredRaa, IntDataRaa, suffixes = c('','Int'),
                      by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = F, all.y=F)
#intvar <- names(BeredIntRaa)[grep('Int', names(BeredIntRaa))]
varMed <- c('Age', 'AgeAdmitted', 'Astma', 'Bilirubin', 'Birthdate', 'BrainDamage',
            'Bukleie', 'ChronicDiseases', 'Diabetes', 'Diagnosis', 'DischargedIntensiveStatus',
            'EcmoEcla', 'EcmoEnd', 'EcmoStart', 'ExtendedHemodynamicMonitoring', 'FrailtyIndex',
            'Glasgow', 'Graviditet', 'Hco3', 'HeartRate',
            'HovedskjemaGUID', 'Impella', 'Intermitterende', 'IntermitterendeDays',
            'InvasivVentilation', 'IsActiveSmoker', 'IsChronicLungDiseasePatient',
            'IsChronicNeurologicNeuromuscularPatient', 'IsEcmoTreatmentAdministered',
            'IsHeartDiseaseIncludingHypertensionPatient', 'IsImpairedImmuneSystemIncludingHivPatient',
            'IsKidneyDiseaseIncludingFailurePatient', 'IsLiverDiseaseIncludingFailurePatient',
            'IsObesePatient', 'IsolationDaysTotal', 'IsRiskFactor', 'KidneyReplacingTreatment',
            'Kontinuerlig', 'KontinuerligDays', 'Kreft', 'Leukocytes', 'MechanicalRespirator',
            'MechanicalRespiratorEnd', 'MechanicalRespiratorStart', 'MvOrCpap', 'Nems',
            'NonInvasivVentilation', 'PatientTransferredFromHospital', 'PatientTransferredFromHospitalName',
            'PatientTransferredToHospital', 'PatientTransferredToHospitalName', 'Potassium',
            'PrimaryReasonAdmitted', 'ReshID', 'Respirator', 'Saps2Score', 'Saps2ScoreNumber',
            'SerumUreaOrBun', 'ShType', 'SkjemaGUID', 'Sodium', 'SystolicBloodPressure',
            'Temperature', 'Trakeostomi', 'TypeOfAdmission', 'UrineOutput',
            'PatientInRegistryGuid', 'PersonId') #'Helseenhet', 'HelseenhetID','ShNavn',
beregnVar <- c('Birthdate', 'FormDate', 'FormStatus', 'HF', 'HelseenhetKortnavn')
BeredIntRaa <- BeredIntRaa1[ ,c(varMed, varFellesInt, beregnVar)] #c()]

return(UtData <- BeredIntRaa)
}
