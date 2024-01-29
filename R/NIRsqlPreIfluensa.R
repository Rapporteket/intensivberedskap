#' Preprosesser data fra Intensivregisteret
#'
#' Funksjonen henter influensadata og evt. navner om variabler og beregner nye.
#'
#' @param datoFra startdato 'yyyy-mm-dd'
#' @param datoTil sluttdato 'yyyy-mm-dd'
#' @param preprosess 0-rådata, 1-preprosesser
#'
#' @return Dataramme med influensadata fra intensivregisteret
#'
#' @export
#'
NIRsqlPreInfluensa <- function(datoFra = '2018-01-01', datoTil = Sys.Date(), preprosess=1, kobleInt=0) {


    query <- paste0('SELECT *
                  # ShNavn,
                  # RHF,
                  # PatientInRegistryGuid,
                  # FormDate,
                  # ICD10_1,
                  # FormStatus
            FROM InfluensaFormDataContract
            WHERE cast(FormDate as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
    #WHERE cast(DateAdmittedIntensive as date) >= \'', datoFra, '\' AND DateAdmittedIntensive <= \'', datoTil, '\'')

    RegDataInf <- rapbase::loadRegData(registryName = "nir", query = query, dbType = "mysql")

    #startOfMonth<- function(x) {as.Date(format(x, "%Y-%m-01")) }
    if (kobleInt == 1){
      RegDataInf$HovedskjemaGUID <- toupper(RegDataInf$HovedskjemaGUID)
      #Koble på intensivdata.
      forsteReg <- min(as.Date(RegDataInf$FormDate))
      queryInt <- paste0('select * from MainFormDataContract
      WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra=forsteReg, '\' AND \'', datoTil=datoTil, '\'') #datoTil=Sys.Date(), '\'')
      IntDataRaa <- rapbase::loadRegData(registryName= "nir", query=queryInt, dbType="mysql")
      IntDataRaa <- dplyr::rename(.data = IntDataRaa, RespiratortidInt = Respirator)

      #Felles variabler som skal hentes fra intensiv (= fjernes fra beredskap)
      #Ved overføringer, kan det ene skjemaet være lagt inn i intensiv og det andre ikke. Vi får da trøbbel i aggregeringa.
      #Velger derfor å ta flest mulig fra beredskapsskjema.
      #Tar bort: (apr. 21, legger til...) 'DateAdmittedIntensive', 'DateDischargedIntensive',
      # varFellesInt <- c('DaysAdmittedIntensiv',
      #                   'DischargedIntensiveStatus',
      #                   'DeadPatientDuring24Hours',	'MechanicalRespirator',	'RHF', 'TransferredStatus',
      #                   'VasoactiveInfusion',	'MoreThan24Hours',	'Morsdato',
      #                   'MovedPatientToAnotherIntensivDuring24Hours',	'PatientAge',	'PatientGender',
      #                   'DateAdmittedIntensive', 'DateDischargedIntensive',
      #                   # 'FormStatus', 'ShNavn', 'PatientInRegistryGuid',
      #                   'UnitId')
      varFellesInt <-c("DateAdmittedIntensive", "DateDischargedIntensive",
                       "DaysAdmittedIntensiv", "DeadPatientDuring24Hours", "DischargedIntensiveStatus",
                       "ICD10_1", "ICD10_2",
                       "MechanicalRespirator", "MoreThan24Hours",
                       "Morsdato", "MorsdatoOppdatert", "MovedPatientToAnotherIntensivDuring24Hours",
                       "PatientAge", "PatientGender",
                       "TransferredStatus", "UnitId", "VasoactiveInfusion")
      #varFellesInt <- intersect(sort(names(RegDataInf)), sort(names(IntDataRaa)))

      #Tar bort variabler som skal hentes fra intensivskjema
      RegDataInf <- RegDataInf[ ,-which(names(RegDataInf) %in% c(varFellesInt))] #, 'DischargedIntensiveStatus'

      RegData <-  merge(RegDataInf, IntDataRaa[,-which(names(IntDataRaa) == 'ReshId')], suffixes = c('','Int'),
                           by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = T, all.y=F)

      #setdiff(c(varMed, varFellesInt, beregnVar), names(BeredIntRaa1))

    } else {
      RegData <- RegDataInf
    }


    #Fjerner  skjema uten intensivskjema
    # if (kobleInt==1){
    #   pasUint <- unique(RegData$PersonId[is.na(RegData$PatientInRegistryGuidInt)])
    #   skjemaUint <- unique(RegData$SkjemaGUID[is.na(RegData$PatientInRegistryGuidInt)])
    #   indManglerIntSkjema <- which(RegData$SkjemaGUID %in% skjemaUint)
    #   #test <- RegData[indManglerIntSkjema, c('SkjemaGUID', "FormDate", "ShNavn")]
    #   if (length(indManglerIntSkjema)) {RegData <- RegData[-indManglerIntSkjema, ]}
    #   }

    if (preprosess == 1) {

      # Endre variabelnavn
      names(RegData)[which(names(RegData) == 'AgeAdmitted')] <- 'Alder'
      #dplyr::rename(RegData, Diabetes=IsDiabeticPatient )


#Endre boolske variabler til boolske.. (Kommer inn som tekst)
        LogVarSjekk <- names(RegData)[unique(which(RegData[1,] %in% c('True','False')), which(RegData[15,] %in% c('True','False')))]
        LogVar <- unique(c(LogVarSjekk,
                           "IsAsthmaticPatient", "IsDiabeticPatient", "IsPregnant", "IsActiveSmoker", "IsChronicLungDiseasePatient",
                           "IsChronicNeurologicNeuromuscularPatient", "IsEcmoTreatmentAdministered",
                           "IsHeartDiseaseIncludingHypertensionPatient", "IsImpairedImmuneSystemIncludingHivPatient",
                           "IsKidneyDiseaseIncludingFailurePatient", "IsLiverDiseaseIncludingFailurePatient",
                           "IsObesePatient", "IsRiskFactor", "IsCancerPatient",
                           'Impella', 'Intermitterende', 'Kontinuerlig', 'No'))

        RegData[, intersect(names(RegData), LogVar)] <-
          apply(RegData[, intersect(names(RegData), LogVar)], 2, as.logical)


        RegData$ECMOTid <- as.numeric(difftime(RegData$EcmoEnd,
                                               RegData$EcmoStart,
                                               units = 'days'))
        RegData$RespTid <- as.numeric(difftime(RegData$MechanicalRespiratorEnd,
                                               RegData$MechanicalRespiratorStart,
                                               units = 'days'))
        RegData$Liggetid <- as.numeric(difftime(RegData$DateDischargedIntensive,
                                                RegData$DateAdmittedIntensive,
                                                units = 'days'))
    #Diagnoser: ICD10_1
    # -1 = Velg verdi
    # 9 = J10 Influensa som skyldes identifisert sesongvariabelt influensavirus
    # 10 = J10.0 Påvist influensavirus med pneumoni
    # 11 = J10.1 Påvist influensavirus med annen luftveissykdom
    # 12 = J10.8 Påvist influensavirus med annen organmanifestasjon
    # 13 = J11 Influensa som skyldes uidentifisert virus (Klinisk mistanke)
    # 14 = J11.0 Mistenkt influensavirus med pneumoni
    # 15 = J11.1 Mistenkt influensavirus med annen luftveissykdom
    # 16 = J11.8 Mistenkt influensavirus med annen organmanifestasjon
    # 17 = Annet
    #Når ein opprettar eit influensaskjema har ein per def. Mistanke om influensa.
    #Vi meiner difor at skjema med verdi -1 også bør tellast med som mistenkt influensa.

    RegData$Influensa <- factor(NA, levels = c('Mistenkt', 'Bekreftet'))
    #--Identifiser J10 og J11 i ICD10-variablene.
    indBekreftet <- which(RegData$ICD10_1 %in% c(9:12))
    indMistenkt <- which(RegData$ICD10_1 %in% c(-1,13:16))
    RegData$Influensa[indMistenkt] <- 'Mistenkt'
    RegData$Influensa[indBekreftet] <- 'Bekreftet'

    RegData$Bekreftet <- as.numeric(RegData$Influensa)-1
    RegData$erMann <- ifelse(RegData$PatientGender==1, 1, 0)


    # Enhetsnivånavn
    RegData$RHF <- factor(RegData$RHF,
                            levels= c('Helse Nord', 'Helse Midt-Norge', 'Helse Vest', 'Helse Sør-Øst', 'Privat'),
                            labels = c('Nord', 'Midt', 'Vest', 'Sør-Øst', 'Privat'))


    #Riktig format på datovariable:
    #Benytter FormDate i stedet for DateAdmitted. De er like men FormDate er alltid utfylt.
    RegData$InnDato <- as.Date(RegData$FormDate, tz= 'UTC', format='%Y-%m-%d') #DateAdmittedIntensive
    RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$FormDate, tz= 'UTC',
                                                format='%Y-%m-%d %H:%M:%S' ) #DateAdmittedIntensive
    RegData$DateDischargedIntensive <- as.POSIXlt(RegData$DateDischargedIntensive, tz= 'UTC',
                                                  format='%Y-%m-%d %H:%M:%S' )



    #Legge på tidsenheter. Bruk factor hvis vil ha med tidsenheter uten registreringer - ikke standard!
    RegData$Dag <- factor(format(RegData$InnDato, '%d.%m.%y'),
                          levels = format(seq(min(RegData$InnDato), max(RegData$InnDato), by='day'), '%d.%m.%y'))
    RegData$Aar <- format(RegData$InnDato, '%Y')
    RegData$UkeNr <- format(RegData$InnDato, '%V')
    RegData$UkeAar <- format(RegData$InnDato, '%G.%V') #%G -The week-based year, %V - Week of the year as decimal number (01–53) as defined in ISO 8601
    RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$Innleggelsestidspunkt, tz= 'UTC',
                                                format='%Y-%m-%d %H:%M:%S' )
    RegData$MndAar <- format(RegData$Innleggelsestidspunkt, '%b%y')
    #RegData$Kvartal <- ceiling(RegData$MndNum/3)
    #RegData$Halvaar <- ceiling(RegData$MndNum/6)
    # RegData$Dag <- factor(format(RegData$InnDato, '%d.%m.%y'),
    #                       levels = format(seq(min(RegData$InnDato), max(RegData$InnDato), by='day'), '%d.%m.%y'))



    #Legg på sesong
    RegData$Sesong <- NA
    startU40 <- c('2017-10-02', '2018-10-01', '2019-09-30', '2020-09-28', '2021-10-04', '2022-10-03',
                  '2023-10-02', '2024-09-30', '2025-09-29', '2026-09-28', '2027-10-04', '2028-10-02')
    sluttU20 <- c('2018-05-20', '2019-05-19', '2020-05-17', '2021-05-23', '2022-05-22', '2023-05-21',
                  '2024-05-19', '2025-05-18', '2026-05-17', '2027-05-23', '2028-05-21', '2029-05-20')
    sesonger <- paste0(2017:2028,'-',18:29)
    for (s in 1:length(sesonger)){
      ind <- which(RegData$InnDato >= as.Date(startU40[s]) & RegData$InnDato <= as.Date(sluttU20[s]))
        RegData$Sesong[ind] <- sesonger[s]
    }

}
   return(invisible(RegData))
}
