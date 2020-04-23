#' Preprosesser data fra Intensivregisteret
#'
#' Denne funksjonen navner om variabler og beregner evt. nye.
#' Funksjonen lager også et "offentlig" datasett som kan benyttes til beregning
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
NIRPreprosessBeredsk <- function(RegData=RegData)	#, reshID=reshID)
{
   #RegData <- NIRberedskDataSQL()
   # Endre variabelnavn:
   #names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
   RegData$Alder <- lubridate::time_length(difftime(as.Date(RegData$FormDate), as.Date(RegData$Birthdate)), "years")
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

   last(RegData$DateDischargedIntensive, order_by = RegData$FormDate)
   #------SLÅ SAMMEN TIL PER PASIENT
   #NB: Tidspunkt endres til en time før selv om velger tz='UTC' hvis formaterer først
   #  På respirator antar man at hvis de ligger på respirator når de overflyttes
   RegDataRed <- RegData %>% group_by(PasientID) %>%
      summarise(Alder = Alder[1],
                PatientGender = PatientGender[1],
                MechanicalRespiratorStart = sort(MechanicalRespiratorStart)[1],
                EcmoStart = sort(EcmoStart)[1],
                DateDischargedIntensive = last(DateDischargedIntensive, order_by = FormDate), #max(DateDischargedIntensive), # sort(DateDischargedIntensive, decreasing = T)[1],
                MechanicalRespiratorEnd = last(MechanicalRespiratorEnd, order_by = FormDate, default = NA),
                #sort(MechanicalRespiratorEnd, decreasing = T)[1],
                #ifelse(is.na(MechanicalRespiratorStart),NA, )
                # RespTid = as.numeric(difftime(max(MechanicalRespiratorEnd,
                #                                           MechanicalRespiratorStart,
                #                                           units = 'days')),
                EcmoEnd = sort(EcmoEnd, decreasing = T)[1], # max(EcmoEnd),
                Morsdato = sort(Morsdato)[1],
                FormStatus = min(FormStatus), #1-kladd, 2-ferdigstilt
                DischargedIntensivStatus = max(DischargedIntensivStatus, na.rm = T), #0-levende, 1-død
                MechanicalRespirator = min(MechanicalRespirator), #1-ja, 2-nei
                Overf = max(Overf),
                Graviditet = sum(Graviditet)>0,
                Astma  = sum(Astma)>0,
                Diabetes = sum(Diabetes)>0,
                IsActivSmoker  = sum(IsActivSmoker)>0,
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
                ReshId = first(ReshId, order_by = FormDate),
                RHF=RHF[1],
                HF=HF[1],
                ShNavn=ShNavn[1],
                FormDate = sort(FormDate)[1])


   #----------------------------
   RegData <- data.frame(RegDataRed)
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

   #unique(RegData[RegData$RHF=='Privat',c("ShNavn", "UnitId", "RHF")])

   #Riktig format på datovariable:
   #Benytter FormDate i stedet for DateAdmitted. De er like men FormDate er alltid utfylt.
   RegData$InnDato <- as.Date(RegData$FormDate, tz= 'UTC', format="%Y-%m-%d") #DateAdmittedIntensive
   RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$FormDate, tz= 'UTC',
                                               format="%Y-%m-%d %H:%M:%S" ) #DateAdmittedIntensive
   RegData$DateDischargedIntensive <- as.POSIXlt(RegData$DateDischargedIntensive, tz= 'UTC',
                                                 format="%Y-%m-%d %H:%M:%S" )
   RegData$MechanicalRespiratorStart <- as.POSIXlt(RegData$MechanicalRespiratorStart,
                                                   tz= 'UTC', format="%Y-%m-%d %H:%M:%S")
   RegData$MechanicalRespiratorEnd <- as.POSIXlt(RegData$MechanicalRespiratorEnd,
                                                 tz= 'UTC', format="%Y-%m-%d %H:%M:%S")
   #De som har Morsdato før utskriving fra intensiv:
   ind <- which(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S") <= as.Date(RegData$DateDischargedIntensive))
   RegData$DischargedIntensivStatus[ind] <- 1


   #Liggetider
   #names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
   RegData$ECMOTid <- as.numeric(difftime(RegData$EcmoEnd,
                                          RegData$EcmoStart,
                                          units = 'days'))
   RegData$RespTid <- as.numeric(difftime(RegData$MechanicalRespiratorEnd,
                                          RegData$MechanicalRespiratorStart,
                                          units = 'days'))
   RegData$liggetid <- as.numeric(difftime(RegData$DateDischargedIntensive,
                                           RegData$Innleggelsestidspunkt,
                                           units = 'days'))

   # Nye tidsvariable:
   RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$Innleggelsestidspunkt, tz= 'UTC',
                                               format="%Y-%m-%d %H:%M:%S" )
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
                         levels = format(seq(min(RegData$InnDato), max(RegData$InnDato), by="day"), '%d.%B'))



   #En "overlever": Person som er i live 30 dager etter innleggelse.
   # RegData$Dod30 <- 0
   # RegData$Dod30[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S"),
   #                              as.Date(RegData$InnDato), units='days')< 30)] <- 1
   # RegData$Dod90 <- 0
   # RegData$Dod90[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S"),
   #                              as.Date(RegData$InnDato), units='days')< 90)] <- 1




   return(invisible(RegData))
}
