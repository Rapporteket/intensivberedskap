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
NIRsqlInfluensa <- function(datoFra = '2018-01-01', datoTil = Sys.Date(), preprosess=1) {


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

    RegData <- rapbase::loadRegData(registryName = "nir", query = query, dbType = "mysql")

    #startOfMonth<- function(x) {as.Date(format(x, "%Y-%m-01")) }

    if (preprosess == 1) {

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

    RegData$Bekr <- as.numeric(RegData$Influensa)-1

    # Enhetsnivånavn
    RegData$RHF <- factor(RegData$RHF)
    #RegData$RHF <- sub('Helse ', '', RegData$RHF)
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

    # RegData$MechanicalRespiratorStart <- as.POSIXlt(RegData$MechanicalRespiratorStart,
    #                                                 tz= 'UTC', format='%Y-%m-%d %H:%M:%S')
    # RegData$MechanicalRespiratorEnd <- as.POSIXlt(RegData$MechanicalRespiratorEnd,
    #                                               tz= 'UTC', format='%Y-%m-%d %H:%M:%S')



    #Legge på tidsenheter. Bruk factor hvis vil ha med tidsenheter uten registreringer - ikke standard!
    RegData$Aar <- format(RegData$InnDato, '%Y')
    # RegData$Aar <- factor(format(RegData$InnDato, '%Y'),
    #                       levels = min(as.numeric(format(RegData$InnDato, '%Y'))):max(as.numeric(format(RegData$InnDato, '%Y'))))
    RegData$UkeNr <- format(RegData$InnDato, '%V')
    # RegData$UkeNr <- factor(format(RegData$InnDato, '%V.%Y'),
    #                         levels = min(as.numeric(format(RegData$InnDato, '%V.%Y'))):max(as.numeric(format(RegData$InnDato, '%V.%Y'))))
    RegData$UkeAar <- format(RegData$InnDato, '%G.%V') #%G -The week-based year, %V - Week of the year as decimal number (01–53) as defined in ISO 8601
    #RegData$UkeAar <- as.factor(RegData$UkeAar)
    RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$Innleggelsestidspunkt, tz= 'UTC',
                                                format='%Y-%m-%d %H:%M:%S' )
    #RegData$MndNum <- RegData$Innleggelsestidspunkt$mon +1
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
    #table(RegData$Sesong)


   #    #Konvertere boolske variable fra tekst til boolske variable...
   # LogVarSjekk <- names(RegData)[unique(which(RegData[1,] %in% c('True','False')), which(RegData[15,] %in% c('True','False')))]
   # LogVar <- unique(c(LogVarSjekk,
   #                    "Astma", "Diabetes", "Graviditet", "IsActiveSmoker", "IsChronicLungDiseasePatient",
   #                    "IsChronicNeurologicNeuromuscularPatient", "IsEcmoTreatmentAdministered",
   #                    "IsHeartDiseaseIncludingHypertensionPatient", "IsImpairedImmuneSystemIncludingHivPatient",
   #                    "IsKidneyDiseaseIncludingFailurePatient", "IsLiverDiseaseIncludingFailurePatient",
   #                    "IsObesePatient", "IsRiskFactor", "Kreft",
   #                    'Impella', 'Intermitterende', 'Kontinuerlig', 'No'))
   #
   # RegData[, intersect(names(RegData), LogVar)] <-
   #    apply(RegData[, intersect(names(RegData), LogVar)], 2, as.logical)
   #
}
   return(invisible(RegData))
}