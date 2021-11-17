

#' Influensatilfeller per uke, regionvis og hele landet
#'
#' @param RegData Influensadata
#' @param bekr 1:bekreftet 0: mistenkt tilfelle
#' @param ferdigstilt ferdigstilt registrering
#' @param sesong influensasesong
#' @param alleUker 0: viser uker med hendelser, 1: viser alle uker
#'
#' @return
#' @export
#'
InfluensaUkeRHF <- function(RegData, bekr=9, skjemastatus=9, dodInt=9, erMann=9, sesong='2021-22', alleUker=1){

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

  TabUkeRHF <- addmargins(TabUkeRHF)
  return(invisible(TabUkeRHF))
}
