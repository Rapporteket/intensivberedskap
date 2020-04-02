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
   # Endre variabelnavn:
   names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
   names(RegData)[which(names(RegData) == 'PatientAge')] <- 'Alder'
   #	names(RegData)[which(names(RegData) == 'ReAdmitted')] <- 'Reinn'
   names(RegData)[which(names(RegData) == 'Respirator')] <- 'respiratortid'
   names(RegData)[which(names(RegData) == 'TransferredStatus')] <- 'Overf'
   #names(RegData)[which(names(RegData) == 'ReshID')] <- 'ReshId'
   names(RegData)[which(names(RegData) == 'UnitId')] <- 'ReshId'
   #Avvik ml. test og prod-data:
   names(RegData)[
      names(RegData) %in% c('PatientInRegistryGuid', 'PasientGUID')] <- 'PasientID'

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

   #Diagnoser:
   RegData$Bekreftet <- 0
   RegData$Bekreftet[which(RegData$Diagnosis %in% 100:103)] <- 1

#------SLÅ SAMMEN TIL PER PASIENT
#NB: Tidspunkt endres til en time før selv om velger tz='UTC'
   options(warn = -1)
   RegDataRed <- RegData %>% group_by(PasientID) %>%
      summarise(Alder = median(Alder),
                PatientGender = PatientGender[1],
                InnDato = min(InnDato),
                Innleggelsestidspunkt = min(format.Date(Innleggelsestidspunkt, tz='UTC'), na.rm = T), #ymd_hms(Innleggelsestidspunkt, tz='UTC')
                DischargedIntensivStatus = max(DischargedIntensivStatus, na.rm = T), #0-levende, 1-død
                DateDischargedIntensive = max(ymd_hms(DateDischargedIntensive), na.rm = T),
                EcmoEnd = max(ymd_hms(EcmoEnd), na.rm = T),
                EcmoStart = min(ymd_hms(EcmoStart), na.rm = T),
                MechanicalRespiratorEnd = max(ymd_hms(MechanicalRespiratorEnd)),
                MechanicalRespiratorStart = min(ymd_hms(MechanicalRespiratorStart)),
                FormStatus = min(FormStatus), #1-kladd, 2-ferdigstilt
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
                MechanicalRespirator = min(MechanicalRespirator), #1-ja, 2-nei
                Overf = max(Overf),
                Bekreftet = max(Bekreftet),
                ReshId = first(ReshId, order_by = InnDato),
                RHF=RHF[1],
                HF=HF[1],
                ShNavn=ShNavn[1])


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

