

#' Henter data og velger variabler for overføring til FHI
#'
#' @return
#' @export
#'
lagInfluDataFHI <- function(){
#Rådata
library(intensivberedskap) #  library(tidyverse) #

  queryInflu <- 'SELECT
              PersonIdBC19Hash
                ,PatientInRegistryGuid
                ,PatientAge
                ,PatientGender
                ,MunicipalNumber
                ,HF
                ,RHF
                ,DateAdmittedIntensive
                ,DateDischargedIntensive
                ,DaysAdmittedIntensiv
                ,ICD10_1
                ,RiskFactor
                ,MechanicalRespirator
                ,Morsdato
                ,DischargedIntensiveStatus
                ,FormStatus
                ,FormDate
                ,CreationDate
                # ,IsEcmoTreatmentAdministered
                # ,IsRiskFactor
                # ,IsAsthmaticPatient
                # ,IsCancerPatient
                # ,IsChronicLungDiseasePatient
                # ,IsChronicNeurologicNeuromuscularPatient
                # ,IsDiabeticPatient
                # ,IsHeartDiseaseIncludingHypertensionPatient
                # ,IsImpairedImmuneSystemIncludingHivPatient
                # ,IsKidneyDiseaseIncludingFailurePatient
                # ,IsLiverDiseaseIncludingFailurePatient
                # ,IsObesePatient
                # ,IsPregnant
 FROM InfluensaFormDataContract'
  #queryInflu <- 'select * from InfluensaFormDataContract'
InfluDataRaa <-  rapbase::loadRegData(registryName = "nir", query = queryInflu, dbType = "mysql")


UtData <- InfluDataRaa #list(InfluDataFHI = InfluDataRaa)

return(UtData)
}
