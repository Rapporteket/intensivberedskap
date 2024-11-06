
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
  # file.rename(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
  # file.copy('tmpNIRinfluensa.pdf', '~/intensivberedskap/tmpNIRinfluensa.pdf')
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
  # rapbase::autLogger(user = brukernavn, registryName = 'NIR - Beredskap', fun = 'abonnementBeredsk',
  #                   reshId = reshID, name = brukernavn, pkg = 'intensivberedskap',
  #                   param = c('RHF_BeredskapCorona.Rnw', 'reshID'),
  #                   msg = "starter Abonnement: Corona-rapport")
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
  # raplog::subLogger(author = brukernavn, registryName = 'NIR - beredskap',
  #                   reshId = reshID,
  #                   msg = paste("Leverer: ", utfil, 'enhetsnivå: ', enhetsNivaa))
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


#Endrer navn på variabler fra int som fortsatt er med i beredskap
varFellesInt <- intersect(sort(names(IntDataRaa)), sort(names(BeredRaa)))
hvilkeIntvar <- which(names(IntDataRaa) %in% varFellesInt)
names(IntDataRaa)[hvilkeIntvar] <- paste0(names(IntDataRaa)[hvilkeIntvar], 'Int')

BeredIntRaa1 <- merge(BeredRaa, IntDataRaa, #suffixes = c('','Int'),
                      by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = F, all.y=F)
#intvar <- names(BeredIntRaa)[grep('Int', names(BeredIntRaa))]
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
            'PatientInRegistryGuid', 'PersonId') #'Helseenhet', 'HelseenhetID','ShNavn',
beregnVar <- c('Birthdate', 'FormDate', 'FormStatus', 'HF', 'HelseenhetKortnavn')
BeredIntRaa <- BeredIntRaa1[ ,c(varMed, varFellesInt, beregnVar)] #c()]

return(UtData <- BeredIntRaa)
}


#' Funksjon som henter filer som skal sendes til FHI. To filer fra intensivopphold
#' og to filer fra sykehusopphold. Dvs. Ei fil for hvert opphold og ei aggregert til
#' person, for hvert register. IKKE i bruk på beredskap. Filer sendes fra Pandemi...
#'
#' @param zipFilNavn Navn på fila som skal kjøres. DataFHIPanBered, Testfil
#' @param brukernavn Innlogget brukernavn
#' @return Filsti til fil med filsti til zip...
#' @export

sendInfluDataFHI <- function(zipFilNavn='Testfil', brukernavn = 'testperson'){ #

  # brukernavn <- brukernavn[[1]]
  # zipFilNavn <- zipFilNavn[[1]]
  regNavnlog <- 'NIRberedskap'

  raplog::subLogger(author = brukernavn, registryName = regNavnlog, reshId = 0,
                    msg = paste0("Vil lage filer for dataoverføring: ", zipFilNavn))

  #opprKat <- getwd()
  opprKat <- setwd(tempdir())
  kat <- getwd()

  #zipFilNavn <- paste0(zipFilNavn, Sys.Date())
  if (zipFilNavn == 'InfluDataFHI') {
    Filer <- intensivberedskap::lagInfluDataFHI()

    raplog::subLogger(author = brukernavn, registryName = regNavnlog, reshId = 0,
                      msg = paste0("Har hentet ekte filer for sending til FHI"))

    datasett <- c('InfluDataFHI')
    for (fil in datasett){
      Fil <- Filer[[fil]]
      write.table(Fil, file = paste0(fil, '.csv'),
                  fileEncoding = 'UTF-8', row.names=F, sep=';', na='')}

    raplog::subLogger(author = brukernavn, registryName = regNavnlog, reshId = 0,
                      msg = paste0("Har lagret ekte filer for sending til FHI"))

    #utils::zip(zipfile = zipFilNavn, files = paste0(datasett, '.csv')) #'PandemiBeredskapTilFHI'

    zip::zipr(zipfile = paste0(zipFilNavn, '.zip'), files = paste0(datasett, '.csv'))

  }

  if (zipFilNavn == 'Testfil') {

    Testfil1 <- data.frame('Test1'=1:5, 'Test2'=letters[1:5])
    Testfil2 <- data.frame('Hei' = c(pi, 3,1), 'Nei' = c(log(2), 200, 3))
    write.table(Testfil1, file = paste('Testfil1.csv'),
                fileEncoding = 'UTF-8', row.names=F, sep=';', na='')
    write.table(Testfil2, file = paste('Testfil2.csv'),
                fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

    raplog::subLogger(author = brukernavn, registryName = regNavnlog, reshId = 0,
                      msg = paste0("Har lagret testfiler"))
    #utils::zip(zipfile = paste0(zipFilNavn), files = c('Testfil1.csv', 'Testfil2.csv'))
    #utils::zip(zipfile = file.path(kat, zipFilNavn), files = c(file.path(kat, 'Testfil1.csv'), file.path(kat, 'Testfil2.csv')))

    zip::zipr(zipfile = paste0(zipFilNavn, '.zip'), files = c('Testfil1.csv', 'Testfil2.csv'))


    #file.info(c(paste0(zipFilNavn, '.zip'), 'Testfil1.csv', 'Testfil2.csv'))['size']
    #unzip(paste0(zipFilNavn, '.zip'), list = FALSE) #list	If TRUE, list the files and extract none
  }
  zipfilSti <- paste0(kat, '/', zipFilNavn, '.zip')

  raplog::subLogger(author = brukernavn, registryName = regNavnlog, reshId = 0,
                    msg = paste0("Har laget zip-fil: ", zipfilSti))

  #For each recipient a list of available vessels (transport methods) is defined and must include relevant credentials.
  #Functions used here rely on local configuration (sship.yml - må oppdateres av hn-ikt) to access such credentials.
  sship::sship(content=zipfilSti,
               recipient = 'nhn', #Character string: user name uniquely defining the recipient both in terms of the public
               #key used for securing the content and any identity control upon docking
               pubkey_holder = 'file', #Character string: the holder of the (recipient's) public key. Per nå kun github?
               vessel = 'sftp', # ut fra beskrivelsen bare ftp
               declaration = paste0("HerErJeg_hilsen_", zipFilNavn))
  # test <- warnings()
  # if (length(test) >0 ){
  # raplog::subLogger(author = brukernavn, registryName = regNavnlog, reshId = 0,
  #                  msg = warnings()) #, utfil))}
  raplog::subLogger(author = brukernavn, registryName = regNavnlog, reshId = 0,
                    msg = paste("Har levert data til NHN/FHI ")) #, utfil))
  write.table(zipfilSti, file = 'zipfilSti.csv',fileEncoding = 'UTF-8')
  utfilsti <- paste0(kat, '/', 'zipfilSti.csv')

  #Fjern filer.. unntatt filstifila
  if (zipFilNavn == 'Testfil') {
    dum <- file.remove(c('Testfil1.csv', 'Testfil2.csv', 'Testfil.zip')) }
  if (zipFilNavn == 'InfluDataFHI') {
    dum <- file.remove(paste0(zipFilNavn, '.zip'), paste0(datasett, '.csv'))
  }

  setwd(opprKat)

  return(utfilsti)
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

  # CoroDataRaa <- NIRberedskDataSQL(kobleInt = 0) #OK
  # CoroDataRaa$HovedskjemaGUID <- toupper(CoroDataRaa$HovedskjemaGUID)
  #
  # CoroData <- NIRPreprosessBeredsk(RegData = CoroDataRaa, aggPers = 1, tellFlereForlop = 1)
  # BeredDataOpph <- NIRPreprosessBeredsk(RegData = CoroDataRaa, aggPers = 0)
  #
   BeredIntRaa <- NIRberedskDataSQL(kobleInt = 1) #OK
   BeredIntPas <- if (dim(BeredIntRaa)[1]>0) {
     NIRPreprosessBeredsk(RegData = BeredIntRaa, kobleInt = 1, aggPers = 1, tellFlereForlop = 1)
   } else {0}

   # test1 <- data.frame(matrix(1:15, nrow = 5, ncol = 3, dimnames = list(row_names = 1:5, colnames = c('id', 'a', 'b'))))
   # test2 <- data.frame(matrix(c(1:5, 101:110), nrow = 5, ncol = 3, dimnames = list(row_names = 1:5, colnames = c('id', 'c', 'd'))))
   # merge(test1, test2, by='id')

   InfluData <- NIRsqlPreInfluensa() #OK
   InfluIntData <- NIRsqlPreInfluensa(kobleInt = 1) #OK

  regNavn <- "intensivberedskap"
  # rapbase::saveStagingData(registryName = regNavn, "CoroDataRaa", CoroDataRaa)
  # rapbase::saveStagingData(regNavn, "CoroData", CoroData)
  # rapbase::saveStagingData(regNavn, "BeredDataOpph", BeredDataOpph)
  # rapbase::saveStagingData(regNavn, "BeredIntRaa", BeredIntRaa)
  # rapbase::saveStagingData(regNavn, "BeredIntPas", BeredIntPas)
  rapbase::saveStagingData(regNavn, "InfluData", InfluData)
  rapbase::saveStagingData(regNavn, "InfluIntData", InfluIntData)

  invisible(rapbase::listStagingData(regNavn))
}
