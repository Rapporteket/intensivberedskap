#Samling av funksjoner som tilrettelegger datafiler, samt funksjon for oversendelse

#' Henter influensadata og velger variabler for overføring til FHI
#'
#' @return #Influensadata tilrettelagt for FHI
#' @export
#'
lagInfluDataFHI <- function(personIDvar='PersonIdBC19Hash'){
#Rådata

 influVar <- paste0(personIDvar,'\n' ,
                ',PatientAge
                ,PatientGender
                ,HF
                ,RHF
                ,MunicipalNumber
                ,CreationDate
                ,DateAdmittedIntensive
                ,DateDischargedIntensive
                ,DaysAdmittedIntensiv
                ,DischargedIntensiveStatus
                ,FormDate
                ,FormStatus
                ,ICD10_1
                ,IsEcmoTreatmentAdministered
                ,IsRiskFactor
                ,IsAsthmaticPatient
                ,IsCancerPatient
                ,IsChronicLungDiseasePatient
                ,IsChronicNeurologicNeuromuscularPatient
                ,IsDiabeticPatient
                ,IsHeartDiseaseIncludingHypertensionPatient
                ,IsImpairedImmuneSystemIncludingHivPatient
                ,IsKidneyDiseaseIncludingFailurePatient
                ,IsLiverDiseaseIncludingFailurePatient
                ,IsObesePatient
                ,IsPregnant
                ,MechanicalRespirator
                ,Morsdato
                ,RiskFactor')

 queryInflu <- paste0('SELECT ', influVar, ' FROM influensaformdatacontract')
InfluDataRaa <-  rapbase::loadRegData(registryName = "nir", query = queryInflu, dbType = "mysql")
  #setdiff(names(InfluDataAlle), names(InfluDataRaa))

UtData <- InfluDataRaa

return(UtData)
}



#' Henter intensivberdskapsdata og velger variabler for overføring til FHI
#'
#' @inheritParams lagDatafilerTilFHI
#' @return Beredskapsdata tilrettelagt for FHI
#' @export
#'
hentBeredDataFHI <- function(personIDvar='PersonIdBC19Hash', raa=1, aggP=1){

   RegDataRaa <- NIRberedskDataSQL() #BeredskapData

      varFHIraa <- c(
       personIDvar
      ,'PatientAge'
      ,'PatientGender'
      ,'MunicipalNumber'
      ,'HF'
      ,'RHF'
      ,'DateAdmittedIntensive'
      ,'DateDischargedIntensive'
      ,'DaysAdmittedIntensiv'
      ,'Diagnosis'
      ,'IsCancerPatient'
      ,'IsImpairedImmuneSystemIncludingHivPatient'
      ,'IsDiabeticPatient'
      ,'IsHeartDiseaseIncludingHypertensionPatient'
      ,'IsObesePatient'
      ,'IsAsthmaticPatient'
      ,'IsChronicLungDiseasePatient'
      ,'IsKidneyDiseaseIncludingFailurePatient'
      ,'IsLiverDiseaseIncludingFailurePatient'
      ,'IsChronicNeurologicNeuromuscularPatient'
      ,'IsPregnant'
      ,'IsActiveSmoker'
      ,'MechanicalRespirator'
      ,'MechanicalRespiratorStart'
      ,'MechanicalRespiratorEnd'
      ,'IsEcmoTreatmentAdministered'
      ,'EcmoStart'
      ,'EcmoEnd'
      ,'Morsdato'
      ,'DischargedIntensiveStatus'
      ,'FormStatus'
      ,'FormDate'
      ,'AgeAdmitted'
      ,'CreationDate'
      ,'FirstTimeClosed'
   ) #De nye variablene må enten legges til i varBort, eller FHI må varsles om at de kommer på ny plass i den aggregerte fila

      BeredskapDataRaaFHI <- RegDataRaa[,varFHIraa]
      BeredskapDataRaaFHI <- dplyr::rename(BeredskapDataRaaFHI, Astma=IsAsthmaticPatient )
      BeredskapDataRaaFHI <- dplyr::rename(BeredskapDataRaaFHI, Diabetes=IsDiabeticPatient )
      BeredskapDataRaaFHI <- dplyr::rename(BeredskapDataRaaFHI, Graviditet=IsPregnant )
      BeredskapDataRaaFHI <- dplyr::rename(BeredskapDataRaaFHI, Kreft=IsCancerPatient )



# if (aggP==1) {
#    RegData <- intensivberedskap:: NIRPreprosessBeredsk(RegData=RegDataRaa, tellFlereForlop = 1)
#    varBort <- c('AgeAdmitted','PatientAge', 'PatientGender', 'Diagnosis', 'DateAdmittedIntensive',
#                 'CreationDate', 'FirstTimeClosed', 'DaysAdmittedIntensiv') #'PatientInRegistryGuid',
#    varNy <- c('Alder', 'erMann', 'Bekreftet', 'Liggetid', 'ReinnResp', 'RespTid') #'PersonId',
#    varFHIpp <- c(varNy, varFHIraa[-which(varFHIraa %in% varBort)],
#                  'FormDateSiste', 'Reinn', 'AntRegPrPas', 'ReinnTid', 'ReinnNaar',
#                  'ReinnRespTid', 'ReinnRespNaar', 'MechanicalRespiratorStartSiste',
#                  'AgeAdmitted')
#    BeredskapDataPpFHI <- RegData[ ,varFHIpp]
#    BeredskapDataPpFHI <- dplyr::rename(BeredskapDataPpFHI, Astma=IsAsthmaticPatient )
#    BeredskapDataPpFHI <- dplyr::rename(BeredskapDataPpFHI, Diabetes=IsDiabeticPatient )
#    BeredskapDataPpFHI <- dplyr::rename(BeredskapDataPpFHI, Graviditet=IsPregnant )
#    BeredskapDataPpFHI <- dplyr::rename(BeredskapDataPpFHI, Kreft=IsCancerPatient )
#
#    #setdiff(varFHIpp, sort(names(RegData)))
#    # write.table(BeredskapDataPpFHI, file = paste0('BeredskapDataPpFHI', Sys.Date(), '.csv'),
#    #             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')
#}
      UtData <- NULL
      if (raa==1){
      UtData <- append(UtData,
                       list(BeredskapDataRaaFHI = BeredskapDataRaaFHI))}
      if (aggP==1) {
         UtData <- append(UtData,
                          list(BeredskapDataPpFHI = BeredskapDataPpFHI))}
   return(UtData)
}


#' Henter data tilrettelagt for oversendelse til FHI
#'
#' @param personIDvar angi variabel for personid
#' 'PersonIdBC19Hash' - standard, 'PersonId' - alternativ, PatientInRegistryGuid - overvåkning
#' @param bered hente beredskapsdata? 1-ja (standard), 0-nei
#' @param influ hente influensadata? 1-ja (standard), 0-nei
#' @param raa hente rådata= 1-ja (standard), 0-nei
#' @param aggP hente persondata aggregert til smitteforløp? 1-ja (standard), 0-nei
#'
#' @return datafiler samlet i ei liste
#' @export
lagDatafilerTilFHI <- function(personIDvar='PersonIdBC19Hash',
                               bered=1, influ=1,
                               raa=1, aggP=1){

  UtData <- NULL
  if (bered==1) {
    dataBered <- hentBeredDataFHI(personIDvar=personIDvar, raa=raa, aggP=aggP)
    UtData <- append(UtData,
                     dataBered)
  }
  if (influ==1) {
    InfluensaDataRaaFHI <- lagInfluDataFHI(personIDvar=personIDvar)
    UtData <- append(UtData,
                     list(InfluensaDataRaaFHI=InfluensaDataRaaFHI)
    )
    }

  return(UtData)
}




