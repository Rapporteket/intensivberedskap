#' Hente data fra beredskapsskjema
#'
#' Henter data fra beredskapsskjema registrert for Intensiv og kobler til
#' noen variabler fra hovedskjema.
#'
#' @param datoFra fra og med, innleggelsesdato
#' @param datoTil til og med, innleggelsesdato
#' @param kobleInt koble på data fra intensivskjema 0 - nei (standard), 1 - ja
#'
#' @return Henter dataramme
#' @export
#'
NIRberedskDataSQL <- function(datoFra = '2020-03-01', datoTil = Sys.Date(), kobleInt=0 ) {

  varBeredsk <- c("
  UPPER(SkjemaGUID) AS SkjemaGUID
,AgeAdmitted
, IsAsthmaticPatient
-- ,Birthdate
-- ,CurrentMunicipalNumber
,CreationDate
,DateAdmittedIntensive
,DateDischargedIntensive
,DaysAdmittedIntensiv
,DeadPatientDuring24Hours
,IsDiabeticPatient
,Diagnosis
-- ,DischargedIntensivStatus
,DischargedIntensiveStatus
-- ,DistrictCode
,EcmoEnd
,EcmoStart
,FirstTimeClosed
,FormDate
,FormStatus
,FormTypeId
,IsPregnant
-- ,HealthUnitName
,HealthUnitShortName
,HF
,UPPER(HovedskjemaGUID) AS HovedskjemaGUID
-- ,IsActivSmoker Endret fra MRS oppdatering 12.juni 2020
,IsActiveSmoker
,IsChronicLungDiseasePatient
,IsChronicNeurologicNeuromuscularPatient
,IsEcmoTreatmentAdministered
,IsHeartDiseaseIncludingHypertensionPatient
,IsImpairedImmuneSystemIncludingHivPatient
,IsKidneyDiseaseIncludingFailurePatient
,IsLiverDiseaseIncludingFailurePatient
,IsObesePatient
,IsRiskFactor
,IsCancerPatient
,LastUpdate
,MechanicalRespirator
,MechanicalRespiratorEnd
,MechanicalRespiratorStart
,MechanicalrespiratorType  #Lagt til på Rapp 13.des -21. Vaskes lengre ned
,MoreThan24Hours
,Morsdato
,MorsdatoOppdatert
,MovedPatientToAnotherIntensivDuring24Hours
,Municipal
,MunicipalNumber
-- ,PatientAge
,PatientGender
,PasientGUID AS PasientID
 -- ,PatientInRegistryGuid
 -- ,PersonId
 -- ,PersonIdBC19Hash
 -- ,PostalCode
,RHF
,ShNavn
,Hospital
,TransferredStatus
,UnitId AS ReshId
")

  registryName = 'data' # "nir"

    query <- paste0('SELECT ',
                    varBeredsk,
                    ' FROM beredskap Q
                      WHERE cast(FormDate as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
    #query <- 'SELECT * from beredskap'
    BeredDataRaa <- rapbase::loadRegData(registryName=registryName, query=query, dbType="mysql")

# 1 er benyttet som standardverdi for MechanicalRespiratorType og vi må følgelig fjerne de som ikke har vært på respirator.
    BeredDataRaa$MechanicalrespiratorType[BeredDataRaa$MechanicalRespirator==2] <- -1

# Riktig format på boolske variabler:
    LogVar <-
    c('IsEcmoTreatmentAdministered', 'IsRiskFactor', 'IsActiveSmoker',
      'IsImpairedImmuneSystemIncludingHivPatient', 'IsCancerPatient',
      'IsDiabeticPatient', 'IsHeartDiseaseIncludingHypertensionPatient', 'IsObesePatient',
      'IsAsthmaticPatient', 'IsChronicLungDiseasePatient', 'IsKidneyDiseaseIncludingFailurePatient',
      'IsLiverDiseaseIncludingFailurePatient', 'IsChronicNeurologicNeuromuscularPatient', 'IsPregnant')
    endreVar <- intersect(names(BeredDataRaa), LogVar)
    BeredDataRaa[, endreVar] <- apply(BeredDataRaa[, endreVar], 2, as.numeric)
    BeredDataRaa[, endreVar] <- apply(BeredDataRaa[, endreVar], 2, as.logical)


  if (kobleInt == 1){
    # BeredDataRaa$HovedskjemaGUID <- toupper(BeredDataRaa$HovedskjemaGUID)


    #Koble på intensivdata.
    forsteReg <- min(as.Date(BeredDataRaa$FormDate))
    # queryInt <- paste0('select * from intensivopphold
    #   WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra=forsteReg, '\' AND \'', datoTil=datoTil, '\'') #datoTil=Sys.Date(), '\'')
    # IntDataRaa <- rapbase::loadRegData(registryName = registryName, query=queryInt, dbType="mysql")
    IntDataRaa <- intensiv::NIRRegDataSQL(datoFra = forsteReg, datoTil = datoTil)

    #Felles variabler som skal hentes fra intensiv (= fjernes fra beredskap)
    #Ved overføringer, kan det ene skjemaet være lagt inn i intensiv og det andre ikke. Vi får da trøbbel i aggregeringa.
    #Velger derfor å ta flest mulig fra beredskapsskjema.
    #Tar bort: (apr. 21, legger til...) 'DateAdmittedIntensive', 'DateDischargedIntensive',
    varFellesInt <- c('AgeAdmitted', 'DaysAdmittedIntensiv',
                      'DischargedIntensiveStatus',
                      'DeadPatientDuring24Hours',	'MechanicalRespirator',	'RHF', 'TransferredStatus',
                      'VasoactiveInfusion',	'MoreThan24Hours',	'Morsdato',
                      'MovedPatientToAnotherIntensivDuring24Hours',	'PatientGender',
                      'DateAdmittedIntensive', 'DateDischargedIntensive',
                      'ReshId')


    #Tar bort variabler som skal hentes fra intensivskjema
    BeredDataRaa <- BeredDataRaa[ ,-which(names(BeredDataRaa) %in% c(varFellesInt))] #, 'DischargedIntensiveStatus'

    #Endrer navn på variabler fra int som fortsatt er med i beredskap
    varFellesInt <- intersect(sort(names(IntDataRaa)), sort(names(BeredDataRaa)))
    hvilkeIntvar <- which(names(IntDataRaa) %in% varFellesInt)
    names(IntDataRaa)[hvilkeIntvar] <- paste0(names(IntDataRaa)[hvilkeIntvar], 'Int')

    BeredIntRaa <- merge(BeredDataRaa, IntDataRaa, #suffixes = c('','Int'),
                          by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUIDInt', all.x = T, all.y=F)
    RegData <-  BeredIntRaa

  } else {
    RegData <- BeredDataRaa
  }


    return(RegData)
}
