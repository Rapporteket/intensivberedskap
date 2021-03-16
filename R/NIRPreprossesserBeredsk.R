#' Preprosesser data fra Intensivregisteret
#'
#' Denne funksjonen navner om variabler og beregner evt. nye.
#' Funksjonen lager også et offentlig datasett som kan benyttes til beregning
#' av kvalitetsindikatorer og som kan legges ved pakken
#'
#' @param RegData Beredskapsskjema
#' @param kobleInt koble på data fra intensivskjema. Hvis koblede data, filtreres registreringer uten intensivskjema bort.
#' 'bered': beredskap (korona), 'influ': influensa
#'
#' @return Data En liste med det filtrerte datasettet (og sykehusnavnet som tilsvarer reshID, ikke pt)
#'
#' @export
#'
NIRPreprosessBeredsk <- function(RegData=RegData, kobleInt=0, aggPers=1)	#, reshID=reshID)
{
   # Bør legge inn sjekk som endrer kobleInt til 1 hvis det opplagt er med variabler fra intensivskjema
   # eller gi feilmelding om at her ser det ut til å være intensivvariabler.


   # Endre variabelnavn:
   #names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
   RegData$Alder <- lubridate::time_length(difftime(as.Date(RegData$FormDate), as.Date(RegData$Birthdate)), 'years')
   #names(RegData)[which(names(RegData) == 'AgeAdmitted')] <- 'Alder' #PatientAge
   names(RegData)[which(names(RegData) == 'Respirator')] <- 'respiratortid'
   names(RegData)[which(names(RegData) == 'TransferredStatus')] <- 'Overf'
   names(RegData)[which(names(RegData) == 'UnitId')] <- 'ReshId'
   names(RegData)[
      names(RegData) %in% c('PatientInRegistryGuid', 'PasientGUID')] <- 'PasientID'


   #Diagnoser:
   RegData$Bekreftet <- 0
   RegData$Bekreftet[which(RegData$Diagnosis %in% 100:103)] <- 1


   #Konvertere boolske variable fra tekst til boolske variable...

   #    TilLogiskeVar <- function(Skjema){
   #    verdiGML <- c('True','False')
   #    verdiNY <- c(TRUE,FALSE)
   #    mapping <- data.frame(verdiGML,verdiNY)
   #    LogVarSjekk <- names(Skjema)[unique(which(Skjema[1,] %in% verdiGML), which(Skjema[15,] %in% verdiGML))]
   #    LogVar <- unique(c(LogVarSjekk,
   #                     "Astma", "Diabetes", "Graviditet", "IsActiveSmoker", "IsChronicLungDiseasePatient",
   #                     "IsChronicNeurologicNeuromuscularPatient", "IsEcmoTreatmentAdministered",
   #                     "IsHeartDiseaseIncludingHypertensionPatient", "IsImpairedImmuneSystemIncludingHivPatient",
   #                     "IsKidneyDiseaseIncludingFailurePatient", "IsLiverDiseaseIncludingFailurePatient",
   #                     "IsObesePatient", "IsRiskFactor", "Kreft"))
   #    if (length(LogVar)>0) {
   #       for (k in 1:length(LogVar)) {
   #          Skjema[,LogVar[k]] <- mapping$verdiNY[match(Skjema[,LogVar[k]], mapping$verdiGML)]
   #       }}
   #    return(Skjema)
   # }
   # RegData <- TilLogiskeVar(RegData)

   LogVarSjekk <- names(RegData)[unique(which(RegData[1,] %in% c('True','False')), which(RegData[15,] %in% c('True','False')))]
   LogVar <- unique(c(LogVarSjekk,
                      "Astma", "Diabetes", "Graviditet", "IsActiveSmoker", "IsChronicLungDiseasePatient",
                      "IsChronicNeurologicNeuromuscularPatient", "IsEcmoTreatmentAdministered",
                      "IsHeartDiseaseIncludingHypertensionPatient", "IsImpairedImmuneSystemIncludingHivPatient",
                      "IsKidneyDiseaseIncludingFailurePatient", "IsLiverDiseaseIncludingFailurePatient",
                      "IsObesePatient", "IsRiskFactor", "Kreft",
                      'Impella', 'Intermitterende', 'Kontinuerlig', 'No'))

   RegData[, intersect(names(RegData), LogVar)] <-
      apply(RegData[, intersect(names(RegData), LogVar)], 2, as.logical)

   #------SLÅ SAMMEN TIL PER PASIENT
   #Respiratortider skal hentes fra intensivskjema
   #sum(grepl('J80', BeredIntRaa[ ,c('ICD10_1', 'ICD10_2', 'ICD10_3', 'ICD10_4', 'ICD10_5')]))

   if (aggPers == 1) {
      #NB: Tidspunkt endres til en time før selv om velger tz='UTC' hvis formaterer først
      #  På respirator antar man at hvis de ligger på respirator når de overflyttes
      #PasientID=="EE983306-AE04-EB11-A96D-00155D0B4D16"
      RegDataRed <- RegData %>% group_by(PasientID) %>%
         summarise(PersonId = PersonId[1],
                   PersonIdBC19Hash = PersonIdBC19Hash[1],
                   Alder = Alder[1],
                   AgeAdmitted = AgeAdmitted[1],
                   PatientGender = PatientGender[1],
                   Morsdato = sort(Morsdato)[1],
                   DischargedIntensiveStatus = sort(DischargedIntensiveStatus, decreasing = T)[1], #max(DischargedIntensiveStatus, na.rm = T), #0-levende, 1-død. Endret navn i MRS
                   Overf = max(Overf),
                   Astma  = sum(Astma)>0,
                   Diabetes = sum(Diabetes)>0,
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
                   Graviditet = sum(Graviditet)>0,
                   Kreft = sum(Kreft)>0,
                   Bekreftet = max(Bekreftet),
                   CreationDate = first(CreationDate, order_by = FormDate),
                   FormStatus = min(FormStatus), #1-kladd, 2-ferdigstilt
                   FirstTimeClosed = first(FirstTimeClosed, order_by = FormDate),
                   #Justering av liggetid mht. reinnleggelse:
                   AntRegPrPas = n(),
                   ReinnTid = ifelse((AntRegPrPas > 1) & (FormStatus==2), #Tid mellom utskrivning og neste innleggelse.
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
                   ReinnNaar = ifelse(Reinn==0, 1, max(which(difftime(sort(FormDate)[2:AntRegPrPas],
                                                                      DateDischargedIntensive[order(FormDate)][1:(AntRegPrPas-1)],
                                                                      units = 'hours') > 12))+1), #Hvilke opphold som er reinnleggelse
                   FormDateSiste = nth(FormDate, ReinnNaar, order_by = FormDate),
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
                   MechanicalRespiratorStartSiste = nth(MechanicalRespiratorStart, ReinnRespNaar+1, order_by = MechanicalRespiratorStart),
                   #NB: MechanicalRespiratorStart/End finnes ikke i intensivskjema.
                   MechanicalRespiratorStart = first(MechanicalRespiratorStart, order_by = MechanicalRespiratorStart),
                   MechanicalRespirator = min(MechanicalRespirator), #1-ja, 2-nei
                   EcmoStart = sort(EcmoStart)[1], #sort tar ikke med NA-verdier.
                   DateDischargedIntensive = last(DateDischargedIntensive, order_by = FormDate), #max(DateDischargedIntensive), # sort(DateDischargedIntensive, decreasing = T)[1],
                   MechanicalRespiratorEnd = last(MechanicalRespiratorEnd, order_by = FormDate),
                   EcmoEnd = sort(EcmoEnd, decreasing = T)[1], #sort(NA) gir tom, men sort(NA)[1] gir NA
                   Municipal = first(Municipal, order_by = FormDate),
                   MunicipalNumber = first(MunicipalNumber, order_by = FormDate),
                   ReshId = first(ReshId, order_by = FormDate),
                   RHF = first(RHF, order_by = FormDate),
                   HFut = last(HF, order_by = FormDate),
                   HF = first(HF, order_by = FormDate),
                   ShNavnUt = last(HelseenhetKortnavn, order_by = FormDate),
                   ShNavn = first(HelseenhetKortnavn, order_by = FormDate),
                   FormDate = first(FormDate, order_by = FormDate),
                   RespTid = ifelse(ReinnResp==0 ,
                                    difftime(MechanicalRespiratorEnd, MechanicalRespiratorStart, units = 'days'),
                                    difftime(MechanicalRespiratorEnd, MechanicalRespiratorStart, units = 'days') - ReinnRespTid/24),
                   Liggetid = ifelse(Reinn==0,
                                     difftime(DateDischargedIntensive, FormDate, units = 'days'),
                                     difftime(DateDischargedIntensive, FormDate, units = 'days') - ReinnTid/24)
         )
   } #aggPers

   if (kobleInt==1){
      #Fjerner  uten intensivskjema
      pasUint <- unique(RegData$PersonId[is.na(RegData$PatientInRegistryGuidInt)])
      indManglerIntSkjema <- which(RegData$PersonId %in% pasUint)
      if (length(indManglerIntSkjema)) {RegData <- RegData[-indManglerIntSkjema, ]}
      indManglerIntPas <- which(RegDataRed$PersonId %in% pasUint)
      if (length(indManglerIntPas)>0) {RegDataRed <- RegDataRed[-indManglerIntPas, ]}

      if (aggPers == 1){

         RegDataRedInt <- RegData %>% group_by(PasientID) %>%
            summarise(
               ExtendedHemodynamicMonitoring = first(ExtendedHemodynamicMonitoring, order_by=FormDate),
               ARDS = sum(grepl('J80', c(ICD10_1, ICD10_2, ICD10_3, ICD10_4, ICD10_5)))>0,
               Bilirubin = first(Bilirubin, order_by=FormDate),
               BrainDamage = first(BrainDamage, order_by=FormDate),
               Bukleie = sum(Bukleie, na.rm=T),
               ChronicDiseases = first(ChronicDiseases, order_by=FormDate),
               DaysAdmittedIntensiv = sum(DaysAdmittedIntensiv, na.rm=T),
               Diagnosis = first(Diagnosis, order_by=FormDate),
               Eeg = first(Eeg, order_by=FormDate), #Fjernes fra datadump
               FrailtyIndex = mean(FrailtyIndex, na.rm = T),
               Glasgow = first(Glasgow, order_by=FormDate),
               Hco3 = first(Hco3, order_by=FormDate),
               Iabp = first(Iabp, order_by=FormDate),
               #Icp = first(Icp, order_by=FormDate), #Fjernes fra datadump
               #Hyperbar = first(Hyperbar, order_by=FormDate), #Fjernes fra datadump
               HeartRate = first(HeartRate, order_by=FormDate),
               #Impella = sum(Impella, na.rm = T), #Hvis ja på en: ja, #Logisk variabel. Alle har False eller tom
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
               #Leukocytes = first(Leukocytes, order_by=FormDate),
               MvOrCpap = first(MvOrCpap, order_by=FormDate),
               Nas = sum(Nas),
               NEMS = sum(Nems),
               NO = sum(No), #Hvis ja: ja, logisk var
               NonInvasivVentilation = sum(NonInvasivVentilation, na.rm=T),
               #Oscillator = first(Oscillator, order_by=FormDate),
               Potassium = first(Potassium, order_by=FormDate),
               #PersonId = PersonId[1],
               PrimaryReasonAdmitted = first(PrimaryReasonAdmitted, order_by=FormDate),
               RespiratortidInt = sum(respiratortid, na.rm = T),
               Saps2Score = first(Saps2Score, order_by=FormDate),
               Saps2ScoreNumber = first(Saps2ScoreNumber, order_by=FormDate),
               SerumUreaOrBun = first(SerumUreaOrBun, order_by=FormDate),
               ShType = first(ShType, order_by=FormDate),
               Sodium = first(Sodium, order_by=FormDate),
               SystolicBloodPressure = first(SystolicBloodPressure, order_by=FormDate),
               Temperature = first(Temperature, order_by=FormDate),
               #TerapetiskHypotermi = first(TerapetiskHypotermi, order_by=FormDate),
               Trakeostomi = ifelse(sum(Trakeostomi %in% 2:3)>0, #Trakeostomitype reg. først.
                                    Trakeostomi[which(Trakeostomi>1)], 1),
               TypeOfAdmission = first(TypeOfAdmission, order_by=FormDate),
               UrineOutput = first(UrineOutput, order_by=FormDate),
               VasoactiveInfusion = min(VasoactiveInfusion), #1-ja, 2-nei. Hvis ja på en:ja
            )
      } #aggPers
      RegData <- data.frame(cbind(RegDataRed, RegDataRedInt))
   } #kobletInt
   else {
      if (aggPers == 1) {RegData <- data.frame(RegDataRed)}}

   #----------------------------





   RegData$Korona <- factor(RegData$Bekreftet, levels= 0:1, labels= c('M', 'B'))

   #Kjønn
   RegData$erMann <- NA #1=Mann, 2=Kvinne, 0=Ukjent
   RegData$erMann[RegData$PatientGender == 1] <- 1
   RegData$erMann[RegData$PatientGender == 2] <- 0
   #RegData$erMann <- factor(RegData$PatientGender, levels=1:2, labels=1:0)
   RegData$Kjonn <- factor(RegData$erMann, levels=0:1, labels=c('kvinner','menn'))


   # Enhetsnivånavn
   RegData$ShNavn <- trimws(as.character(RegData$ShNavn)) #Fjerner mellomrom (før) og etter navn
   RegData$RHF <- sub('Helse ', '', RegData$RHF) #factor()
   # Kode om fra Haraldsplass til RHF Vest og Lovisenberg diakonhjemmet til RHF Øst, fra priv
   RegData$RHF[RegData$ReshId == 100180] <- 'Vest' #Haraldsplass
   RegData$RHF[RegData$ReshId == 42088921] <- 'Sør-Øst' #Lovisenberg Diakonale
   RegData$RHF[RegData$ReshId == 108897] <- 'Sør-Øst' #Diakonhjemmet

   #unique(RegData[RegData$RHF=='Privat',c(ShNavn, UnitId, RHF)])

   #Riktig format på datovariable:
   #Benytter FormDate i stedet for DateAdmitted. De er like men FormDate er alltid utfylt.
   RegData$InnDato <- as.Date(RegData$FormDate, tz= 'UTC', format='%Y-%m-%d') #DateAdmittedIntensive
   RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$FormDate, tz= 'UTC',
                                               format='%Y-%m-%d %H:%M:%S' ) #DateAdmittedIntensive
   RegData$DateDischargedIntensive <- as.POSIXlt(RegData$DateDischargedIntensive, tz= 'UTC',
                                                 format='%Y-%m-%d %H:%M:%S' )
   # RegData$FirstTimeClosed
   # RegData$Creat

   RegData$MechanicalRespiratorStart <- as.POSIXlt(RegData$MechanicalRespiratorStart,
                                                   tz= 'UTC', format='%Y-%m-%d %H:%M:%S')
   RegData$MechanicalRespiratorEnd <- as.POSIXlt(RegData$MechanicalRespiratorEnd,
                                                 tz= 'UTC', format='%Y-%m-%d %H:%M:%S')
   RegData$Morsdato <- as.POSIXlt(RegData$Morsdato, tz= 'UTC', format='%Y-%m-%d')
   #De som har Morsdato før utskriving fra intensiv:
   ind <- which(as.Date(RegData$Morsdato) <= as.Date(RegData$DateDischargedIntensive))
   RegData$DischargedIntensiveStatus[ind] <- 1


   #Liggetider
   #names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
   RegData$ECMOTid <- as.numeric(difftime(RegData$EcmoEnd,
                                          RegData$EcmoStart,
                                          units = 'days'))
   # RegData$RespTid <- as.numeric(difftime(RegData$MechanicalRespiratorEnd,
   #                                        RegData$MechanicalRespiratorStart,
   #                                        units = 'days'))
   # RegData$Liggetid <- as.numeric(difftime(RegData$DateDischargedIntensive,
   #                                         RegData$Innleggelsestidspunkt,
   #                                         units = 'days'))

   # Nye tidsvariable:
   RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$Innleggelsestidspunkt, tz= 'UTC',
                                               format='%Y-%m-%d %H:%M:%S' )
   RegData$MndNum <- RegData$Innleggelsestidspunkt$mon +1
   RegData$MndAar <- format(RegData$Innleggelsestidspunkt, '%b%y')
   RegData$Kvartal <- ceiling(RegData$MndNum/3)
   RegData$Halvaar <- ceiling(RegData$MndNum/6)
   # RegData$Aar <- format(RegData$InnDato, '%Y')
   RegData$Aar <- factor(format(RegData$InnDato, '%Y'),
                         levels = min(as.numeric(format(RegData$InnDato, '%Y'))):max(as.numeric(format(RegData$InnDato, '%Y'))))
   # RegData$UkeNr <- format(RegData$InnDato, '%V')
   RegData$UkeNr <- factor(format(RegData$InnDato, '%V.%Y'),
                           levels = min(as.numeric(format(RegData$InnDato, '%V.%Y'))):max(as.numeric(format(RegData$InnDato, '%V.%Y'))))
   #RegData$UkeAar <- format(RegData$InnDato, '%G.%V') #%G -The week-based year, %V - Week of the year as decimal number (01–53) as defined in ISO 8601
   #RegData$UkeAar <- as.factor(RegData$UkeAar)
   # RegData$Dag <- format(RegData$InnDato, '%d.%b')
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
