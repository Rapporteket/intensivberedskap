

#' Henter data og tilrettelegger filer for overføring til FHI
#'
#' @return
#' @export
#'
InfluensadataTilFHI <- function(){
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
   FROM InfluensaFormDataContract'
  #queryInflu <- 'select * from InfluensaFormDataContract'
InfluDataRaa <-  rapbase::loadRegData(registryName = "nir", query = queryInflu, dbType = "mysql")



UtData <- list(InfluDataRaa = InfluDataRaa)
return(UtData)

}
