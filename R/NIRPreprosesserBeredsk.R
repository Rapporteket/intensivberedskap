#' Preprosesser data fra Intensivregisteret
#'
#' Denne funksjonen navner om variabler og beregner evt. nye.
#' Funksjonen lager også et offentlig datasett som kan benyttes til beregning
#' av kvalitetsindikatorer og som kan legges ved pakken
#'
#' @param RegData Beredskapsskjema
#' @param kobleInt koble på data fra intensivskjema. Hvis koblede data, filtreres registreringer uten intensivskjema bort.
#' Kobling skjer før eventuell aggregering til forløp.
#' 'bered': beredskap (covid), 'influ': influensa
#'
#' @return Data En liste med det prosesserte datasettet (og sykehusnavnet som tilsvarer reshID, ikke pt)
#'
#' @export
#'
NIRPreprosessBeredsk <- function(RegData=RegData, kobleInt=0, aggPers=1, tellFlereForlop=0)	#, reshID=reshID)
{
   # Bør legge inn sjekk som endrer kobleInt til 1 hvis det opplagt er med variabler fra intensivskjema
   # eller gi feilmelding om at her ser det ut til å være intensivvariabler.

   # Endre variabelnavn:
   RegData$Alder <- RegData$AgeAdmitted
   names(RegData)[which(names(RegData) == 'Respirator')] <- 'respiratortid'
   names(RegData)[which(names(RegData) == 'TransferredStatus')] <- 'Overf'

   #Diagnoser:
   RegData$Bekreftet <- 0
   RegData$Bekreftet[which(RegData$Diagnosis %in% 100:103)] <- 1

   # Enhetsnivånavn
   RegData$ShNavn <- trimws(as.character(RegData$ShNavn)) #Fjerner mellomrom (før) og etter navn
   RegData$RHF <- sub('Helse ', '', RegData$RHF) #factor()
   # Kode om fra Haraldsplass til RHF Vest og Lovisenberg diakonhjemmet til RHF Øst, fra priv
   RegData$RHF[RegData$ReshId == 100180] <- 'Vest' #Haraldsplass
   RegData$RHF[RegData$ReshId == 42088921] <- 'Sør-Øst' #Lovisenberg Diakonale
   RegData$RHF[RegData$ReshId == 108897] <- 'Sør-Øst' #Diakonhjemmet

   #Tomme sykehusnavn får resh som navn:
   indTom <- which(is.na(RegData$ShNavn) | RegData$ShNavn == '')
   RegData$ShNavn[indTom] <- RegData$ReshId[indTom]

   #Sjekker om alle resh har egne enhetsnavn
   dta <- unique(RegData[ ,c('ReshId', 'ShNavn')])
   duplResh <- names(table(dta$ReshId)[which(table(dta$ReshId)>1)])
   duplSh <- names(table(dta$ShNavn)[which(table(dta$ShNavn)>1)])

   if (length(c(duplSh, duplResh)) > 0) {
     ind <- union(which(RegData$ReshId %in% duplResh), which(RegData$ShNavn %in% duplSh))
     RegData$ShNavn[ind] <- paste0(RegData$ShNavn[ind],' (', RegData$ReshId[ind], ')')
   }


   #Liggetider
   RegData$ECMOTid <- as.numeric(difftime(RegData$EcmoEnd,
                                          RegData$EcmoStart,
                                          units = 'days'))
   RegData$RespTid <- as.numeric(difftime(RegData$MechanicalRespiratorEnd,
                                          RegData$MechanicalRespiratorStart,
                                          units = 'days'))
   RegData$Liggetid <- as.numeric(difftime(RegData$DateDischargedIntensive,
                                           RegData$DateAdmittedIntensive,
                                           units = 'days'))


   if (kobleInt==1){
      #Fjerner  skjema uten intensivskjema
      pasUint <- unique(RegData$PasientID[is.na(RegData$PasientIDInt)])
      skjemaUint <- unique(RegData$SkjemaGUID[is.na(RegData$PasientIDInt)])
      indManglerIntSkjema <- which(RegData$SkjemaGUID %in% skjemaUint)
      #test <- RegData[indManglerIntSkjema, c('SkjemaGUID', "FormDate", "ShNavn")]
      if (length(indManglerIntSkjema)) {RegData <- RegData[-indManglerIntSkjema, ]}

      if (aggPers == 1){ #Fjerner pasienter som mangler ett eller flere intensivskjema
         indManglerIntPas <- which(RegData$PersonId %in% pasUint)
         if (length(indManglerIntPas)>0) {RegData <- RegData[-indManglerIntPas, ]}
      }}


   #------SLÅ SAMMEN TIL PER PASIENT
   #Respiratortider skal hentes fra intensivskjema
   #sum(grepl('J80', BeredIntRaa[ ,c('ICD10_1', 'ICD10_2', 'ICD10_3', 'ICD10_4', 'ICD10_5')]))

   #Identifisere pasienter med flere innleggelser
   if (tellFlereForlop==1) { #Tar med flere forløp for hver pasient

     RegData$Dato <- as.Date(RegData$FormDate)

     #Identifiserer inntil 4 forløp
     PasFlere <- RegData %>% dplyr::group_by(PasientID) %>%
       dplyr::reframe(SkjemaGUID = SkjemaGUID,
                        InnNr2 = ifelse(Dato-min(Dato)>90, 2, 1),
                        InnNr3 = ifelse(InnNr2 > 1, ifelse(Dato - min(Dato[InnNr2==2])>90, 3, 2), 1),
                        InnNr   =   ifelse(InnNr3>2, ifelse(Dato - min(Dato[InnNr3==3])>90, 4, 3), InnNr3),
                        PasientID = paste0(PasientID, '_', InnNr)
                        #Tid = as.numeric(Dato-min(Dato))
       )

     RegData <- RegData[ ,-which(names(RegData)=="PasientID")]
     RegData <- merge(RegData, PasFlere, by='SkjemaGUID')
     #which(RegData$InnNr==2)
     #Test <- RegData[c(1:10, which(RegData$InnNr==2)),c("PasientID", "PasientIDny")]
     #For testing: RegData$Dato[RegData$PasientID=='EAC1F8C2-B10F-EC11-A974-00155D0B4D1A'][3:4] <- as.Date(c('2023-01-02', '2024-01-03'))
   }


   if (aggPers == 1) {
      #NB: Tidspunkt endres til en time før selv om velger tz='UTC' hvis formaterer først
      #  På respirator antar man at hvis de ligger på respirator når de overflyttes
      #PasientID=="EE983306-AE04-EB11-A96D-00155D0B4D16"
      RegDataRed <- RegData %>% dplyr::group_by(PasientID) %>% #Pasienter med flere forløp har nå forløp angitt med xx_forløpsnr
         dplyr::summarise(
                   Alder = Alder[1],
                   AgeAdmitted = AgeAdmitted[1],
                   PatientGender = PatientGender[1],
                   Morsdato = Morsdato[1], #sort(Morsdato)[1],
                   DischargedIntensiveStatus = sort(DischargedIntensiveStatus, decreasing = T)[1], #max(DischargedIntensiveStatus, na.rm = T), #0-levende, 1-død. Endret navn i MRS
                   Overf = max(Overf),
                   IsAsthmaticPatient  = sum(IsAsthmaticPatient)>0,
                   IsDiabeticPatient = sum(IsDiabeticPatient)>0,
                   IsActiveSmoker  = sum(IsActiveSmoker)>0, #Opprinnelig navn fra MRS: IsActivSmoker
                   IsChronicLungDiseasePatient = sum(IsChronicLungDiseasePatient)>0,
                   IsChronicNeurologicNeuromuscularPatient = sum(IsChronicNeurologicNeuromuscularPatient)>0,
                   IsEcmoTreatmentAdministered = sum(IsEcmoTreatmentAdministered)>0, #Fjernes fra datadump. Tas inn. FHI har den på lista
                   IsHeartDiseaseIncludingHypertensionPatient  = sum(IsHeartDiseaseIncludingHypertensionPatient)>0,
                   IsImpairedImmuneSystemIncludingHivPatient = sum(IsImpairedImmuneSystemIncludingHivPatient)>0,
                   IsKidneyDiseaseIncludingFailurePatient  = sum(IsKidneyDiseaseIncludingFailurePatient)>0,
                   IsLiverDiseaseIncludingFailurePatient = sum(IsLiverDiseaseIncludingFailurePatient)>0,
                   IsObesePatient = sum(IsObesePatient)>0,
                   IsRiskFactor = sum(IsRiskFactor)>0,
                   IsPregnant = sum(IsPregnant)>0,
                   IsCancerPatient = sum(IsCancerPatient)>0,
                   Bekreftet = max(Bekreftet),
                   AntRegPrPas = dplyr::n(),
                   CreationDate = dplyr::first(CreationDate, order_by = FormDate),
                   FormStatus = min(FormStatus), #1-kladd, 2-ferdigstilt
                   FirstTimeClosed = dplyr::first(FirstTimeClosed, order_by = FormDate),
                   #Justering av liggetid mht. reinnleggelse:
                   ReinnTid = ifelse((AntRegPrPas > 1) & sum(is.na(DateDischargedIntensive))>0, #(FormStatus==2), #Tid mellom utskrivning og neste innleggelse.
                                     sort(difftime(sort(FormDate)[2:AntRegPrPas], #sort hopper over NA
                                                   DateDischargedIntensive[order(FormDate)][1:(AntRegPrPas-1)],
                                                   units = 'hours'), decreasing = T)[1],
                                     0),
                   InnSmResh = ifelse(AntRegPrPas > 1,
                                      sum(ReshId[order(FormDate)][2:AntRegPrPas] == ReshId[order(FormDate)][1:AntRegPrPas-1]),
                                      0),
                   ReinnKval = ifelse((InnSmResh > 0) & (ReinnTid < 72 & ReinnTid > 0),  1, 0),
                   Reinn = ifelse(ReinnTid > 12,  1, 0), #Brukes til å få riktig startpunkt for nye innleggelser.
                   AntReinn = sum(ReinnTid>12), #Antall reinnleggelser
                   # ReinnNaar = ifelse(Reinn==0, 1, #0-nei, 1-ja NB: Gir feil når reinnleggelse mellom to ulike resh. Kan bare benyttes for ReinnKval!
                   #                    max(which(ReshId[order(FormDate)][2:AntRegPrPas] ==
                   #                                 ReshId[order(FormDate)][1:(AntRegPrPas-1)]))+1),
                   #Oppholdet etter lengst utetid velges som reinnleggelse. Mister andre reinn hvis flere.
                   ReinnNaar = ifelse(Reinn==0 | sum(is.na(DateDischargedIntensive))>0, 1,
                                      max(which(difftime(sort(FormDate)[2:AntRegPrPas],
                                                      DateDischargedIntensive[order(FormDate)][1:(AntRegPrPas-1)],
                                                      units = 'hours') > 12))+1), #Hvilke opphold som er reinnleggelse
                   FormDateSiste = dplyr::nth(FormDate, ReinnNaar, order_by = FormDate),
                   #Justering av respiratortid mht. reinnleggelse. NB: Kan være reinnlagt på respirator selv om ikke reinnlagt på intensiv.
                   AntRespPas = length(MechanicalRespiratorStart)-sum(is.na(MechanicalRespiratorStart)), #sum(MechanicalRespirator==1, na.rm=T), #
                   ReinnRespTid = ifelse((AntRespPas > 1) & (FormStatus==2), #Tid mellom utskrivning og neste innleggelse.
                                         sort(difftime(MechanicalRespiratorStart[order(MechanicalRespiratorStart)][2:AntRespPas], #sort hopper over NA
                                                       MechanicalRespiratorEnd[order(MechanicalRespiratorStart)][1:(AntRespPas-1)],
                                                       units = 'hours'), decreasing = T)[1],
                                         0),
                   ReinnResp = ifelse(ReinnRespTid > 12, 1, 0),
                   ReinnRespNaar = ifelse(ReinnResp==0, 0, #0-nei, 1-ja
                                          max(which(difftime(MechanicalRespiratorStart[order(MechanicalRespiratorStart)][2:AntRespPas],
                                                             MechanicalRespiratorEnd[order(MechanicalRespiratorStart)][1:(AntRespPas-1)],
                                                             units = 'hours') > 12))), #Hvilket opphold som er siste reinnleggelse på respirator
                   MechanicalRespiratorStartSiste = dplyr::nth(MechanicalRespiratorStart, ReinnRespNaar+1, order_by = MechanicalRespiratorStart),
                   #NB: MechanicalRespiratorStart/End finnes ikke i intensivskjema.

                   MT1 = sum(MechanicalrespiratorType==1)>0,
                   MT2 = sum(MechanicalrespiratorType==2)>0,
                   #NB: Må ta bort de som ikke har vært på respirator...
                   #Type: -1:ikke utfylt, 1-invasiv, 2-non-invasiv
                   InvNonIBegge = ifelse(MT1&MT2, 3,
                                                ifelse(MT2, 2,
                                                       ifelse(MT1, 1, -1))),
                   MechanicalrespiratorTypeSiste = dplyr::last(MechanicalrespiratorType, order_by = FormDate),
                   MechanicalRespiratorStart = dplyr::first(MechanicalRespiratorStart, order_by = MechanicalRespiratorStart),
                   MechanicalRespirator = min(MechanicalRespirator), #1-ja, 2-nei
                   EcmoStart = sort(EcmoStart)[1], #sort tar ikke med NA-verdier.
                   DateDischargedIntensive = dplyr::last(DateDischargedIntensive, order_by = FormDate), #max(DateDischargedIntensive), # sort(DateDischargedIntensive, decreasing = T)[1],
                   MechanicalRespiratorEnd = dplyr::last(MechanicalRespiratorEnd, order_by = FormDate),
                   EcmoEnd = sort(EcmoEnd, decreasing = T)[1], #sort(NA) gir tom, men sort(NA)[1] gir NA
                   ECMOTid = ifelse(sum(!is.na(ECMOTid))>0, sum(ECMOTid, na.rm = T), NA), #Ellers får vi 0 på tomme
                   Municipal = dplyr::first(Municipal, order_by = FormDate),
                   MunicipalNumber = dplyr::first(MunicipalNumber, order_by = FormDate),
                   ReshId = dplyr::first(ReshId, order_by = FormDate),
                   RHFut = dplyr::last(RHF, order_by = FormDate),
                   RHF = dplyr::first(RHF, order_by = FormDate),
                   HFut = dplyr::last(HF, order_by = FormDate),
                   HF = dplyr::first(HF, order_by = FormDate),
                   ShNavnUt = dplyr::last(HealthUnitShortName, order_by = FormDate),
                   ShNavn = dplyr::first(HealthUnitShortName, order_by = FormDate),
                   SykehusUt = dplyr::last(Hospital, order_by = FormDate),
                   Sykehus = dplyr::first(Hospital, order_by = FormDate),
                   FormDateUt = dplyr::last(FormDate, order_by = FormDate),
                   FormDate = dplyr::first(FormDate, order_by = FormDate),
                   RespTid = ifelse(sum(RespTid, na.rm = T) > 0, sum(RespTid, na.rm = T), NA),
                  #    ifelse(ReinnResp==0 ,
                   #                  difftime(MechanicalRespiratorEnd, MechanicalRespiratorStart, units = 'days'),
                   #                  difftime(MechanicalRespiratorEnd, MechanicalRespiratorStart, units = 'days') - ReinnRespTid/24),
                   Liggetid = sum(Liggetid, na.rm = T),
                     # ifelse(Reinn==0,
                     #                 difftime(DateDischargedIntensive, FormDate, units = 'days'),
                     #                 difftime(DateDischargedIntensive, FormDate, units = 'days') - ReinnTid/24)
         )
   } #aggPers


   if (kobleInt==1){
      # #Fjerner  uten intensivskjema
      pasUint <- unique(RegData$PersonId[is.na(RegData$PatientInRegistryGuidInt)])
      skjemaUint <- unique(RegData$SkjemaGUID[is.na(RegData$PatientInRegistryGuidInt)])
      indManglerIntSkjema <- which(RegData$SkjemaGUID %in% skjemaUint)
      test <- RegData[indManglerIntSkjema, c('SkjemaGUID', "FormDate", "ShNavn")]
      if (length(indManglerIntSkjema)) {RegData <- RegData[-indManglerIntSkjema, ]}

      if (aggPers == 1){

         RegDataRedInt <- RegData %>%
           dplyr::group_by(PasientID) %>%
           dplyr::summarise(
             ExtendedHemodynamicMonitoring = dplyr::first(ExtendedHemodynamicMonitoring, order_by=FormDate),
             ARDS = sum(grepl('J80', c(ICD10_1, ICD10_2, ICD10_3, ICD10_4, ICD10_5)))>0,
             Bilirubin = dplyr::first(Bilirubin, order_by=FormDate),
             BrainDamage = dplyr::first(BrainDamage, order_by=FormDate),
             Bukleie = sum(Bukleie, na.rm=T),
             ChronicDiseases = dplyr::first(ChronicDiseases, order_by=FormDate),
             DaysAdmittedIntensiv = sum(DaysAdmittedIntensiv, na.rm=T),
             Diagnosis = dplyr::first(Diagnosis, order_by=FormDate),
             FrailtyIndex = mean(FrailtyIndex, na.rm = T),
             Glasgow = dplyr::first(Glasgow, order_by=FormDate),
             Hco3 = dplyr::first(Hco3, order_by=FormDate),
             Iabp = dplyr::first(Iabp, order_by=FormDate),
              HeartRate = dplyr::first(HeartRate, order_by=FormDate),
             Intermitterende = sum(Intermitterende, na.rm = T), #Hvis ja på en: ja
             IntermitterendeDays = sum(IntermitterendeDays, na.rm = T),
             InvasivVentilation = sum(InvasivVentilation, na.rm = T),
             #Isolation = first(Isolation, order_by=FormDate), #Hvis ja på en: ja. Ikke mulig 1-nei, 2-5 ulike årsaker
             IsolasjonLuft = ifelse(sum(Isolation==3)>0, 1,0), #ifelse(Isolation , -1, ifelse(Isolation==3, 1,0)), #3 - dråpesmitte
             IsolasjonDagerLuft = ifelse(IsolasjonLuft==1, sum(IsolationDaysTotal), 0),
             #IsolationDaysTotal = sum(IsolationDaysTotal, na.rm = T),
             KidneyReplacingTreatment = min(KidneyReplacingTreatment), # 1-ja, 2-nei, -1 ikke svart
             Kontinuerlig = sum(Kontinuerlig), #Logisk variabel
             KontinuerligDays = sum(KontinuerligDays, na.rm = T),
             # hvis ja på en Leverdialyse = first(Leverdialyse, order_by=FormDate), #Ta ut av datadump
             MvOrCpap = dplyr::first(MvOrCpap, order_by=FormDate),
             Nas = sum(Nas),
             NEMS = sum(Nems),
             NO = sum(No), #Hvis ja: ja, logisk var
             NonInvasivVentilation = sum(NonInvasivVentilation, na.rm=T),
             Potassium = dplyr::first(Potassium, order_by=FormDate),
             #PersonId = PersonId[1],
             PrimaryReasonAdmitted = dplyr::first(PrimaryReasonAdmitted, order_by=FormDate),
             RespiratortidInt = sum(respiratortid, na.rm = T),
             Saps2Score = dplyr::first(Saps2Score, order_by=FormDate),
             Saps2ScoreNumber = dplyr::first(Saps2ScoreNumber, order_by=FormDate),
             SerumUreaOrBun = dplyr::first(SerumUreaOrBun, order_by=FormDate),
             ShType = dplyr::first(ShType, order_by=FormDate),
             Sodium = dplyr::first(Sodium, order_by=FormDate),
             SystolicBloodPressure = dplyr::first(SystolicBloodPressure, order_by=FormDate),
             Temperature = dplyr::first(Temperature, order_by=FormDate),
             Trakeostomi = ifelse(sum(Trakeostomi %in% 2:3)>0, #Trakeostomitype reg. først.
                                  Trakeostomi[which(Trakeostomi>1)], 1),
             TypeOfAdmission = dplyr::first(TypeOfAdmission, order_by=FormDate),
             UrineOutput = dplyr::first(UrineOutput, order_by=FormDate),
             VasoactiveInfusion = min(VasoactiveInfusion), #1-ja, 2-nei. Hvis ja på en:ja
           )
         RegData <- data.frame(cbind(RegDataRed, RegDataRedInt))
      } #aggPers
   } else {
      if (aggPers == 1) {RegData <- data.frame(RegDataRed)}
     }

   #----------------------------





   RegData$Covid <- factor(RegData$Bekreftet, levels= 0:1, labels= c('M', 'B'))

   #Kjønn
   RegData$erMann <- NA #1=Mann, 2=Kvinne, 0=Ukjent
   RegData$erMann[RegData$PatientGender == 1] <- 1
   RegData$erMann[RegData$PatientGender == 2] <- 0
   RegData$Kjonn <- factor(RegData$erMann, levels=0:1, labels=c('kvinner','menn'))



   #Riktig format på datovariable:
   #Benytter FormDate i stedet for DateAdmitted. De er like men FormDate er alltid utfylt.
   RegData$InnDato <- as.Date(RegData$FormDate, tz= 'UTC', format='%Y-%m-%d') #DateAdmittedIntensive
   RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$FormDate, tz= 'UTC',
                                               format='%Y-%m-%d %H:%M:%S' ) #DateAdmittedIntensive
   RegData$DateDischargedIntensive <- as.POSIXlt(RegData$DateDischargedIntensive, tz= 'UTC',
                                                 format='%Y-%m-%d %H:%M:%S' )

   RegData$MechanicalRespiratorStart <- as.POSIXlt(RegData$MechanicalRespiratorStart,
                                                   tz= 'UTC', format='%Y-%m-%d %H:%M:%S')
   RegData$MechanicalRespiratorEnd <- as.POSIXlt(RegData$MechanicalRespiratorEnd,
                                                 tz= 'UTC', format='%Y-%m-%d %H:%M:%S')
   RegData$Morsdato <- as.POSIXlt(RegData$Morsdato, tz= 'UTC', format='%Y-%m-%d')
   #De som har Morsdato før utskriving fra intensiv:
   ind <- which(as.Date(RegData$Morsdato) <= as.Date(RegData$DateDischargedIntensive))
   RegData$DischargedIntensiveStatus[ind] <- 1



   # Nye tidsvariable:
   RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$Innleggelsestidspunkt, tz= 'UTC',
                                               format='%Y-%m-%d %H:%M:%S' )
   RegData$MndNum <- RegData$Innleggelsestidspunkt$mon +1
   RegData$MndAar <- format(RegData$Innleggelsestidspunkt, '%b%y')
   RegData$Kvartal <- ceiling(RegData$MndNum/3)
   RegData$Halvaar <- ceiling(RegData$MndNum/6)
   RegData$Aar <- factor(format(RegData$InnDato, '%Y'),
                         levels = min(as.numeric(format(RegData$InnDato, '%Y'))):max(as.numeric(format(RegData$InnDato, '%Y'))))
   RegData$UkeNr <- factor(format(RegData$InnDato, '%V.%G'), #G - angir ukebasert år slik at blir riktig for uke 1.
                           levels = min(as.numeric(format(RegData$InnDato, '%V.%G'))):max(as.numeric(format(RegData$InnDato, '%V.%G'))))
   RegData$Dag <- factor(format(RegData$InnDato, '%d.%m.%y'),
                         levels = format(seq(min(RegData$InnDato), max(RegData$InnDato), by='day'), '%d.%m.%y'))



   #En overlever: Person som er i live 30 dager etter innleggelse.
   RegData$Dod30 <- 0
   RegData$Dod30[which(difftime(as.Date(RegData$Morsdato, format='%Y-%m-%d %H:%M:%S'),
                                as.Date(RegData$InnDato), units='days')< 30)] <- 1
   RegData$Dod90 <- 0
   RegData$Dod90[which(difftime(as.Date(RegData$Morsdato, format='%Y-%m-%d %H:%M:%S'),
                                as.Date(RegData$InnDato), units='days')< 90)] <- 1




   return(invisible(RegData))
}
