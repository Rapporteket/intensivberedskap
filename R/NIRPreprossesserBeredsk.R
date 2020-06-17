#' Preprosesser data fra Intensivregisteret
#'
#' Denne funksjonen navner om variabler og beregner evt. nye.
#' Funksjonen lager også et offentlig datasett som kan benyttes til beregning
#' av kvalitetsindikatorer og som kan legges ved pakken
#'
#' @param RegData Beredskapsskjema
#' @param skjema hvilket skjema data som skal preprosesseres tilhører
#' 1: hoved, 2: paaror, 3: influ, 4: beredsk
#'
#' @return Data En liste med det filtrerte datasettet (og sykehusnavnet som tilsvarer reshID, ikke pt)
#'
#' @export
#'
NIRPreprosessBeredsk <- function(RegData=RegData, kobletInt=0)	#, reshID=reshID)
{
   #RegData <- NIRberedskDataSQL()
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
   TilLogiskeVar <- function(Skjema){
      verdiGML <- c('True','False')
      verdiNY <- c(TRUE,FALSE)
      mapping <- data.frame(verdiGML,verdiNY)
      LogVar <- names(Skjema)[which(Skjema[1,] %in% verdiGML)]
      if (length(LogVar)>0) {
         for (k in 1:length(LogVar)) {
            Skjema[,LogVar[k]] <- mapping$verdiNY[match(Skjema[,LogVar[k]], mapping$verdiGML)]
         }}
      return(Skjema)
   }

   RegData <- TilLogiskeVar(RegData)

   #------SLÅ SAMMEN TIL PER PASIENT
   #Respiratortider skal hentes fra intensivskjema

   if (kobletInt==1){
      RegDataRedEkstra <- RegData %>% group_by(PasientID) %>%
         summarise(ExtendedHemodynamicMonitoring = first(ExtendedHemodynamicMonitoring, order_by=FormDate),
                   Bilirubin = first(Bilirubin, order_by=FormDate),
                   BrainDamage = first(BrainDamage, order_by=FormDate),
                   Bukleie = first(Bukleie, order_by=FormDate),
                   ChronicDiseases = first(ChronicDiseases, order_by=FormDate),
                   Diagnosis = first(Diagnosis, order_by=FormDate),
                   EcmoEcla = first(EcmoEcla, order_by=FormDate),
                   Eeg = first(Eeg, order_by=FormDate),
                   FrailtyIndex = first(FrailtyIndex, order_by=FormDate),
                   Glasgow = first(Glasgow, order_by=FormDate),
                   Hco3 = first(Hco3, order_by=FormDate),
                   Iabp = first(Iabp, order_by=FormDate),
                   Icp = first(Icp, order_by=FormDate),
                   Hyperbar = first(Hyperbar, order_by=FormDate),
                   HeartRate = first(HeartRate, order_by=FormDate),
                   Impella = first(Impella, order_by=FormDate),
                   Intermitterende = first(Intermitterende, order_by=FormDate),
                   IntermitterendeDays = first(IntermitterendeDays, order_by=FormDate),
                   InvasivVentilation = sum(InvasivVentilation, na.rm = T),
                   IsEcmoTreatmentAdministered = first(IsEcmoTreatmentAdministered, order_by=FormDate),
                   Isolation = first(Isolation, order_by=FormDate),
                   IsolationDaysTotal = sum(IsolationDaysTotal, na.rm = T),
                   KidneyReplacingTreatment = first(KidneyReplacingTreatment, order_by=FormDate),
                   Kontinuerlig = first(Kontinuerlig, order_by=FormDate),
                   KontinuerligDays = sum(KontinuerligDays, na.rm = T),
                   Leverdialyse = first(Leverdialyse, order_by=FormDate),
                   Leukocytes = first(Leukocytes, order_by=FormDate),
                   MvOrCpap = first(MvOrCpap, order_by=FormDate),
                   NEMS = sum(Nems),
                   No = first(No, order_by=FormDate),
                   NonInvasivVentilation = sum(NonInvasivVentilation, na.rm=T),
                   Oscillator = first(Oscillator, order_by=FormDate),
                   Potassium = first(Potassium, order_by=FormDate),
                   PrimaryReasonAdmitted = first(PrimaryReasonAdmitted, order_by=FormDate),
                   RespiratortidInt = sum(respiratortid, na.rm = T),
                   Saps2Score = first(Saps2Score, order_by=FormDate),
                   Saps2ScoreNumber = first(Saps2ScoreNumber, order_by=FormDate),
                   SerumUreaOrBun = first(SerumUreaOrBun, order_by=FormDate),
                   ShType = first(ShType, order_by=FormDate),
                   Sodium = first(Sodium, order_by=FormDate),
                   SystolicBloodPressure = first(SystolicBloodPressure, order_by=FormDate),
                   Temperature = first(Temperature, order_by=FormDate),
                   TerapetiskHypotermi = first(TerapetiskHypotermi, order_by=FormDate),
                   Trakeostomi = first(Trakeostomi, order_by=FormDate),
                   TypeOfAdmission = first(TypeOfAdmission, order_by=FormDate),
                   UrineOutput = first(UrineOutput, order_by=FormDate),
                   VasoactiveInfusion = first(VasoactiveInfusion  , order_by=FormDate),
         )
   }
   #DateAdmittedIntensive                      DaysAdmittedIntensiv
   #DeadPatientDuring24Hours                   TransferredStatus
   # PatientTransferredFromHospital             PatientTransferredFromHospitalName
   # PatientTransferredToHospital               PatientTransferredToHospitalName
   #MoreThan24Hours
   #NB: Tidspunkt endres til en time før selv om velger tz='UTC' hvis formaterer først
   #  På respirator antar man at hvis de ligger på respirator når de overflyttes
   RegDataRed <- RegData %>% group_by(PasientID) %>%
      summarise(Alder = Alder[1],
                PatientGender = PatientGender[1],
                Morsdato = sort(Morsdato)[1],
                DischargedIntensivStatus = max(DischargedIntensiveStatus, na.rm = T), #0-levende, 1-død
                Overf = max(Overf),
                Graviditet = sum(Graviditet)>0,
                Astma  = sum(Astma)>0,
                Diabetes = sum(Diabetes)>0,
                IsActivSmoker  = sum(IsActiveSmoker)>0, #Opprinnelig navn fra MRS: IsActivSmoker
                IsChronicLungDiseasePatient = sum(IsChronicLungDiseasePatient)>0,
                IsChronicNeurologicNeuromuscularPatient = sum(IsChronicNeurologicNeuromuscularPatient)>0,
                IsHeartDiseaseIncludingHypertensionPatient  = sum(IsHeartDiseaseIncludingHypertensionPatient)>0,
                IsImpairedImmuneSystemIncludingHivPatient = sum(IsImpairedImmuneSystemIncludingHivPatient)>0,
                IsKidneyDiseaseIncludingFailurePatient  = sum(IsKidneyDiseaseIncludingFailurePatient)>0,
                IsLiverDiseaseIncludingFailurePatient = sum(IsLiverDiseaseIncludingFailurePatient)>0,
                IsObesePatient = sum(IsObesePatient)>0,
                IsRiskFactor = sum(IsRiskFactor)>0,
                Kreft = sum(Kreft)>0,
                Bekreftet = max(Bekreftet),
                FormStatus = min(FormStatus), #1-kladd, 2-ferdigstilt
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
                Reinn = ifelse(ReinnTid > 12,  1, 0),
                ReinnNaar = ifelse(Reinn==0, 1, #0-nei, 1-ja
                                   max(which(ReshId[order(FormDate)][2:AntRegPrPas] ==
                                                ReshId[order(FormDate)][1:AntRegPrPas-1]))+1), #Hvilke opphold som er reinnleggelse# ReinnNaar = ifelse(Reinn==0, 0, #0-nei, 1-ja
                #                     max(which(difftime(sort(FormDate)[2:AntRegPrPas],
                #                                        DateDischargedIntensive[order(FormDate)][1:(AntRegPrPas-1)],
                #                                        units = 'hours') > 12))), #Hvilke opphold som er reinnleggelse
                FormDateSiste = nth(FormDate, ReinnNaar, order_by = FormDate),
                #Justering av respiratortid mht. reinnleggelse. NB: Kan være reinnlagt på respirator selv om ikke reinnlagt på intensiv.
                AntRespPas = sum(MechanicalRespirator==1, na.rm=T),
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
                MechanicalRespiratorStart = first(MechanicalRespiratorStart, order_by = MechanicalRespiratorStart),
                MechanicalRespirator = min(MechanicalRespirator), #1-ja, 2-nei
                EcmoStart = sort(EcmoStart)[1],
                DateDischargedIntensive = last(DateDischargedIntensive, order_by = FormDate), #max(DateDischargedIntensive), # sort(DateDischargedIntensive, decreasing = T)[1],
                MechanicalRespiratorEnd = last(MechanicalRespiratorEnd, order_by = FormDate),
                EcmoEnd = sort(EcmoEnd, decreasing = T)[1], #sort(NA) gir tom, men sort(NA)[1] gir NA
                Municipal = first(Municipal, order_by = FormDate),
                ReshId = first(ReshId, order_by = FormDate),
                RHF = first(RHF, order_by = FormDate),
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


   #----------------------------
   RegData <- if (kobletInt==1) {
      data.frame(cbind(RegDataRed, RegDataRedEkstra))
   } else {data.frame(RegDataRed)}
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
   RegData$MechanicalRespiratorStart <- as.POSIXlt(RegData$MechanicalRespiratorStart,
                                                   tz= 'UTC', format='%Y-%m-%d %H:%M:%S')
   RegData$MechanicalRespiratorEnd <- as.POSIXlt(RegData$MechanicalRespiratorEnd,
                                                 tz= 'UTC', format='%Y-%m-%d %H:%M:%S')
   #De som har Morsdato før utskriving fra intensiv:
   ind <- which(as.Date(RegData$Morsdato, format='%Y-%m-%d %H:%M:%S') <= as.Date(RegData$DateDischargedIntensive))
   RegData$DischargedIntensivStatus[ind] <- 1


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
   RegData$UkeNr <- factor(format(RegData$InnDato, '%V'),
                           levels = min(as.numeric(format(RegData$InnDato, '%V'))):max(as.numeric(format(RegData$InnDato, '%V'))))
   #RegData$UkeAar <- format(RegData$InnDato, '%G.%V') #%G -The week-based year, %V - Week of the year as decimal number (01–53) as defined in ISO 8601
   #RegData$UkeAar <- as.factor(RegData$UkeAar)
   # RegData$Dag <- format(RegData$InnDato, '%d.%B')
   RegData$Dag <- factor(format(RegData$InnDato, '%d.%B'),
                         levels = format(seq(min(RegData$InnDato), max(RegData$InnDato), by='day'), '%d.%B'))



   #En overlever: Person som er i live 30 dager etter innleggelse.
   RegData$Dod30 <- 0
   RegData$Dod30[which(difftime(as.Date(RegData$Morsdato, format='%Y-%m-%d %H:%M:%S'),
                                as.Date(RegData$InnDato), units='days')< 30)] <- 1
   RegData$Dod90 <- 0
   RegData$Dod90[which(difftime(as.Date(RegData$Morsdato, format='%Y-%m-%d %H:%M:%S'),
                                as.Date(RegData$InnDato), units='days')< 90)] <- 1




   return(invisible(RegData))
}
