

#' Influensatilfeller per uke, regionvis og hele landet
#'
#' @param RegData Influensadata
#' @param bekr 1:bekreftet 0: mistenkt tilfelle
#' @param ferdigstilt ferdigstilt registrering
#' @param sesong influensasesong
#'
#' @return
#' @export
#'
InfluensaUkeRHF <- function(RegData, bekr=9, skjemastatus=9, dodInt=9, erMann=9, sesong='2021-22', alleUker=1){
  # InfluData$Sesong <- 'diverse'
  # InfluData$Sesong[(InfluData$InnDato >= '2018-10-01') & (InfluData$InnDato <= '2019-05-19')] <- '2018-19'
  # InfluData$Sesong[(InfluData$InnDato >= '2019-09-30') & (InfluData$InnDato <= '2020-05-17')] <- '2019-20'
  # InfluData$Sesong[(InfluData$InnDato >= '2020-09-28') & (InfluData$InnDato <= '2021-05-23')] <- '2020-21'
  # InfluData$Sesong <- factor(InfluData$Sesong,levels = c('2018-19', '2019-20', '2020-21', 'diverse'))
  #,labels = c('2018-19', '2019-20', '2020-21', 'diverse'))

InfluData <- RegData
  InfluData <- InfluData[InfluData$Sesong == sesong,]
  if (bekr != 9) {InfluData <- InfluData[InfluData$Bekr == bekr, ]}
  if (skjemastatus != 9) {InfluData <- InfluData[InfluData$FormStatus == skjemastatus, ]}
  if (dodInt %in% -1:1){InfluData <- subset(InfluData, InfluData$DischargedIntensiveStatus==dodInt)}
  if (erMann %in% 0:1){InfluData <- subset(InfluData, InfluData$erMann==erMann)}


  if (alleUker==1){
    uker <- switch(sesong,
                   '2018-19' = seq(as.Date('2018-10-01'), as.Date('2019-05-19'), "weeks"),
                   '2019-20' = seq(as.Date( '2019-09-30'), as.Date('2020-05-17'), "weeks"),
                   '2020-21' = seq(as.Date( '2020-09-28'), as.Date('2021-05-23'), "weeks"),
                   '2021-22' = seq(as.Date('2021-10-04'), min(Sys.Date(), as.Date('2022-05-22')), "weeks")
                   )
  InfluData$UkeAar <- factor(InfluData$UkeAar, levels=format(uker, '%G.%V'))
  }

  TabUkeRHF <- table(InfluData[ , c('UkeAar', 'RHF')])
  #TabUkeRHF <- ftable(InfluData[ , c('UkeAar', 'RHF', 'Influensa')], row.vars ='UkeAar') #, col.vars = c('RHF', 'Influensa'))
  #TabUkeRHFFerdig <- ftable(InfluData[indFerdig , c('UkeAar', 'RHF', 'Influensa')], row.vars ='UkeAar') #, col.vars = c('RHF', 'Influensa'))
  antUker <- dim(TabUkeRHF)[1]
  indAntRader <- max(1,(antUker-20)):(antUker+1)

  # TabUkeInflu <- table(InfluData[ ,c('UkeAar', 'Influensa')])      #InfluData$UkeNr, function(x) sum((InfluData$ICD10_1==10 | InfluData$ICD10_2==10)))
  #   TabUkeTot <- addmargins(TabUkeInflu) #cbind(TabUkeInflu, 'Tot. ant. skjema' = table(InfluData$UkeAar))
  # row.names(TabUkeTot)[antUker+1] <- 'Sum, hele sesongen'
  # TabUkeTotFerdig <- addmargins(TabUkeInfluFerdig) #cbind(TabUkeInflu, 'Tot. ant. skjema' = table(InfluData$UkeAar))
  # row.names(TabUkeTotFerdig)[antUker+1] <- 'Sum, hele sesongen'

  # navnRHF <- attributes(TabUkeRHF)$col.vars$RHF
  # navnUkenr <- attributes(TabUkeRHF)$row.vars$UkeAar
  # TabUkeRHF <- as.matrix(TabUkeRHF)
  TabUkeRHF <- addmargins(TabUkeRHF)
  return(invisible(TabUkeRHF))
}
