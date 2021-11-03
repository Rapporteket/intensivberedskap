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


  varBeredsk <- c("UPPER(SkjemaGUID) AS SkjemaGUID
-- ,AddressQuality
,AgeAdmitted
, IsAsthmaticPatient
,Birthdate
-- ,CurrentMunicipalNumber
,CreationDate
,DateAdmittedIntensive
,DateDischargedIntensive
,DaysAdmittedIntensiv
,DeadPatientDuring24Hours
, IsDiabeticPatient
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
, IsPregnant
,Helseenhet
-- ,HelseenhetID
,HelseenhetKortnavn
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
, IsCancerPatient
,LastUpdate
,MajorVersion
,MechanicalRespirator
,MechanicalRespiratorEnd
,MechanicalRespiratorStart
,MinorVersion
,MoreThan24Hours
,Morsdato
,MorsdatoOppdatert
,MovedPatientToAnotherIntensivDuring24Hours
,Municipal
,MunicipalNumber
,PatientAge
,PatientGender
-- ,PasientGUID
 ,PatientInRegistryGuid
 ,PersonId
 ,PersonIdBC19Hash
-- ,PostalCode
,RHF
,ShNavn
,Sykehus
,TransferredStatus
,UnitId")

    query <- paste0('SELECT ',
                    varBeredsk,
                    ' FROM ReadinessFormDataContract Q
                      WHERE cast(FormDate as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
    #query <- 'SELECT * from ReadinessFormDataContract'
    BeredDataRaa <- rapbase::loadRegData(registryName="nir", query=query, dbType="mysql")

  if (kobleInt == 1){
    BeredDataRaa$HovedskjemaGUID <- toupper(BeredDataRaa$HovedskjemaGUID)


    #Koble på intensivdata.
    forsteReg <- min(as.Date(BeredDataRaa$FormDate))
    queryInt <- paste0('select * from MainFormDataContract
      WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra=forsteReg, '\' AND \'', datoTil=Sys.Date(), '\'')
    IntDataRaa <- rapbase::loadRegData(registryName= "nir", query=queryInt, dbType="mysql")

    #Felles variabler som skal hentes fra intensiv (= fjernes fra beredskap)
    #Ved overføringer, kan det ene skjemaet være lagt inn i intensiv og det andre ikke. Vi får da trøbbel i aggregeringa.
    #Velger derfor å ta flest mulig fra beredskapsskjema.
    #Tar bort: (apr. 21, legger til...) 'DateAdmittedIntensive', 'DateDischargedIntensive',
    varFellesInt <- c('DaysAdmittedIntensiv',
                      'DischargedIntensiveStatus',
                      'DeadPatientDuring24Hours',	'MechanicalRespirator',	'RHF', 'TransferredStatus',
                      'VasoactiveInfusion',	'MoreThan24Hours',	'Morsdato',
                      'MovedPatientToAnotherIntensivDuring24Hours',	'PatientAge',	'PatientGender',
                      'DateAdmittedIntensive', 'DateDischargedIntensive',
                      # 'FormStatus', 'ShNavn', 'PatientInRegistryGuid',
                      'UnitId')

    #varFellesInt <- intersect(sort(names(BeredDataRaa)), sort(names(IntDataRaa)))

    #Tar bort variabler som skal hentes fra intensivskjema
    BeredDataRaa <- BeredDataRaa[ ,-which(names(BeredDataRaa) %in% c(varFellesInt))] #, 'DischargedIntensiveStatus'

    BeredIntRaa <- merge(BeredDataRaa, IntDataRaa[,-which(names(IntDataRaa) == 'ReshId')], suffixes = c('','Int'),
                          by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = T, all.y=F)
    #varIKKEmed <- CerebralCirculationAbolished	CerebralCirculationAbolishedReasonForNo	CurrentMunicipalNumber	DistrictCode	Eeg	FormStatus	FormTypeId	HF	HFInt	Hyperbar	Iabp	Icp	Isolation	LastUpdate	Leverdialyse	MajorVersion	MinorVersion	MorsdatoOppdatert	Municipal	MunicipalNumber	Nas	No	OrganDonationCompletedReasonForNoStatus	OrganDonationCompletedStatus	Oscillator	PIM_Probability	PIM_Score	PostalCode	RHF	Sykehus	TerapetiskHypotermi	UnitIdInt
    # varMed <- c('Age', 'AgeAdmitted', 'Astma', 'Bilirubin', 'Birthdate', 'BrainDamage',
    #             'Bukleie', 'ChronicDiseases', 'Diabetes', 'Diagnosis',
    #             'EcmoEcla', 'EcmoEnd', 'EcmoStart', 'ExtendedHemodynamicMonitoring', 'FrailtyIndex',
    #             'Glasgow', 'Graviditet', 'Hco3', 'HeartRate',
    #             'HovedskjemaGUID', 'Impella', 'Intermitterende', 'IntermitterendeDays',
    #             'InvasivVentilation', 'IsActiveSmoker', 'IsChronicLungDiseasePatient',
    #             'IsChronicNeurologicNeuromuscularPatient', 'IsEcmoTreatmentAdministered',
    #             'IsHeartDiseaseIncludingHypertensionPatient', 'IsImpairedImmuneSystemIncludingHivPatient',
    #             'IsKidneyDiseaseIncludingFailurePatient', 'IsLiverDiseaseIncludingFailurePatient',
    #             'IsObesePatient', 'Isolation', 'IsolationDaysTotal', 'IsRiskFactor', 'KidneyReplacingTreatment',
    #             'Kontinuerlig', 'KontinuerligDays', 'Kreft', 'Leukocytes', 'MechanicalRespirator',
    #             'MechanicalRespiratorEnd', 'MechanicalRespiratorStart', 'Municipal','MunicipalNumber',
    #             'MvOrCpap', 'Nas', 'Nems', 'NonInvasivVentilation',
    #             'PatientTransferredFromHospital', 'PatientTransferredFromHospitalName',
    #             'PatientTransferredFromHospitalReason',
    #             'PatientTransferredToHospital', 'PatientTransferredToHospitalName',
    #             'PatientTransferredToHospitalReason','Potassium',
    #             'PrimaryReasonAdmitted', 'Respirator', 'Saps2Score', 'Saps2ScoreNumber',
    #             'SerumUreaOrBun', 'ShType', 'SkjemaGUID', 'Sodium', 'SystolicBloodPressure',
    #             'Temperature', 'Trakeostomi', 'TypeOfAdmission', 'UrineOutput',
    #             'PersonId',  # 'PatientInRegistryGuid',
    #             'TerapetiskHypotermi',  'Iabp', 'Oscillator', 'No', 'Leverdialyse', 'Eeg')
    # 'Helseenhet', 'HelseenhetID','ShNavn', 'ReshId',
    # beregnVar <- c('Birthdate', 'FormDate', 'FormStatus', 'HF', 'HelseenhetKortnavn',
    #                'ICD10_1', 'ICD10_2', 'ICD10_3', 'ICD10_4', 'ICD10_5')
    RegData <-  BeredIntRaa #[ ,c(varMed, varFellesInt, beregnVar)] #c()]

    #setdiff(c(varMed, varFellesInt, beregnVar), names(BeredIntRaa1))

  } else {
    RegData <- BeredDataRaa
  }

    return(RegData)
}
