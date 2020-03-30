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

      #Kjønn
      RegData$erMann <- NA #1=Mann, 2=Kvinne, 0=Ukjent
      RegData$erMann[RegData$PatientGender == 1] <- 1
      RegData$erMann[RegData$PatientGender == 2] <- 0
      RegData$Kjonn <- factor(RegData$erMann, levels=0:1, labels=c('kvinner','menn'))

      #Diagnoser:
      RegData$Korona <- factor(NA, levels = c('M', 'B'))
      RegData$Korona[which(RegData$Diagnosis %in% c(-1,104:107))] <- 'M'
      RegData$Korona[which(RegData$Diagnosis %in% 100:103)] <- 'B'
      RegData$Bekreftet <- 0
      RegData$Bekreftet[which(RegData$Diagnosis %in% 100:103)] <- 1



      # Endre variabelnavn:
      names(RegData)[which(names(RegData) == 'Saps2Score')] <- 'SMR' #Saps2Score er SAPS estimert mortalitet
      names(RegData)[which(names(RegData) == 'Saps2ScoreNumber')] <- 'SAPSII'
      names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
      names(RegData)[which(names(RegData) == 'Nems')] <- 'NEMS'
      names(RegData)[which(names(RegData) == 'PatientAge')] <- 'Alder'
      #	names(RegData)[which(names(RegData) == 'ReAdmitted')] <- 'Reinn'
      names(RegData)[which(names(RegData) == 'Respirator')] <- 'respiratortid'
      names(RegData)[which(names(RegData) == 'TransferredStatus')] <- 'Overf'
      names(RegData)[which(names(RegData) == 'TypeOfAdmission')] <- 'InnMaate'
      names(RegData)[which(names(RegData) == 'ReshID')] <- 'ReshId'
      #names(RegData)[which(names(RegData) == 'PatientInRegistryGuid')] <- 'PasientID'
      names(RegData)[which(names(RegData) == 'UnitId')] <- 'ReshId'
      #Avvik ml. test og prod-data:
      names(RegData)[
            names(RegData) %in% c('PatientInRegistryGuid', 'PasientGUID')] <- 'PasientID'

      # Enhetsnivånavn
      RegData$ShNavn <- trimws(as.character(RegData$ShNavn)) #Fjerner mellomrom (før) og etter navn
      RegData$RHF <- sub('Helse ', '', RegData$RHF) #factor()
      # Kode om fra Haraldsplass til RHF Vest og Lovisenberg diakonhjemmet til RHF Øst, fra priv
      RegData$RHF[RegData$ReshId == 100180] <- 'Vest' #Haraldsplass
      RegData$RHF[RegData$ReshId == 42088921] <- 'Sør-Øst' #Lovisenberg Diakonale
      RegData$RHF[RegData$ReshId == 108897] <- 'Sør-Øst' #Diakonhjemmet

      #unique(RegData[RegData$RHF=='Privat',c("ShNavn", "UnitId", "RHF")])

      # RegData$HF <- sub(' universitetssykehus', '', RegData$HF)
      # RegData$HF <- sub(' sykehus', '', RegData$HF)
      # RegData$HF <- sub('Sykehuset i', '', RegData$HF)
      # RegData$HF <- sub('Sykehuset ', '', RegData$HF)
      # RegData$HF <- sub(' AS', '', RegData$HF)


      #Riktig format på datovariable:
      RegData$InnDato <- as.Date(RegData$FormDate, tz= 'UTC', format="%Y-%m-%d") #DateAdmittedIntensive
      RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$FormDate, tz= 'UTC',
                                                  format="%Y-%m-%d %H:%M:%S" ) #DateAdmittedIntensive
      RegData$DateDischargedIntensive <- as.POSIXlt(RegData$DateDischargedIntensive, tz= 'UTC',
                                                    format="%Y-%m-%d %H:%M:%S" )
      RegData$MechanicalRespiratorStart <- as.POSIXlt(RegData$MechanicalRespiratorStart,
                                                      tz= 'UTC', format="%Y-%m-%d %H:%M:%S")
      RegData$MechanicalRespiratorEnd <- as.POSIXlt(RegData$MechanicalRespiratorEnd,
                                                      tz= 'UTC', format="%Y-%m-%d %H:%M:%S")

      #Liggetider
      names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
      RegData$ECMOTid <- as.numeric(difftime(RegData$EcmoEnd,
                                             RegData$EcmoStart,
                                             units = 'days'))
      RegData$RespTid <- as.numeric(difftime(RegData$MechanicalRespiratorEnd,
                                  RegData$MechanicalRespiratorStart,
                                  units = 'days'))

      # Nye tidsvariable:
      RegData$MndNum <- RegData$Innleggelsestidspunkt$mon +1
      RegData$MndAar <- format(RegData$Innleggelsestidspunkt, '%b%y')
      RegData$Kvartal <- ceiling(RegData$MndNum/3)
      RegData$Halvaar <- ceiling(RegData$MndNum/6)
      RegData$Aar <- format(RegData$InnDato, '%Y')
      RegData$UkeNr <- format(RegData$InnDato, '%V')
      #RegData$UkeAar <- format(RegData$InnDato, '%G.%V') #%G -The week-based year, %V - Week of the year as decimal number (01–53) as defined in ISO 8601
      #RegData$UkeAar <- as.factor(RegData$UkeAar)
      RegData$Dag <- format(RegData$InnDato, '%d.%B')

      ##Kode om  pasienter som er overført til/fra egen avdeling til "ikke-overført"
      #1= ikke overført, 2= overført
      ind <- union(which(RegData$ReshId == RegData$PatientTransferredFromHospital),
                   which(RegData$ReshId == RegData$PatientTransferredToHospital))
      RegData$Overf[ind] <- 1

      #De som har Morsdato før utskriving fra intensiv:
      ind <- which(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S") <= as.Date(RegData$DateDischargedIntensive))
      RegData$DischargedIntensivStatus[ind] <- 1

      #En "overlever": Person som er i live 30 dager etter innleggelse.
      RegData$Dod30 <- 0
      RegData$Dod30[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S"),
                                   as.Date(RegData$InnDato), units='days')< 30)] <- 1
      RegData$Dod90 <- 0
      RegData$Dod90[which(difftime(as.Date(RegData$Morsdato, format="%Y-%m-%d %H:%M:%S"),
                                   as.Date(RegData$InnDato), units='days')< 90)] <- 1


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


      return(invisible(RegData))
}

