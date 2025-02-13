

#' Influensatilfeller per uke, regionvis og hele landet
#'
#' @param RegData Influensadata
#' @param bekr 1:bekreftet 0: mistenkt tilfelle
#' @param ferdigstilt ferdigstilt registrering
#' @param sesong influensasesong
#' @param alleUker 0: viser uker med hendelser, 1: viser alle uker
#'
#' @return Tabell med antall influensatilfeller per uke og RHF
#' @export
#'
InfluensaUkeRHF <- function(RegData, bekr=9, skjemastatus=9, dodInt=9, erMann=9, sesong='2023-24', alleUker=1){

InfluData <- RegData
  InfluData <- InfluData[InfluData$Sesong == sesong,]
  if (bekr != 9) {InfluData <- InfluData[InfluData$Bekreftet == bekr, ]}
  if (skjemastatus != 9) {InfluData <- InfluData[InfluData$FormStatus == skjemastatus, ]}
  if (dodInt %in% -1:1){InfluData <- subset(InfluData, InfluData$DischargedIntensiveStatus==dodInt)}
  if (erMann %in% 0:1){InfluData <- subset(InfluData, InfluData$erMann==erMann)}


  if (alleUker==1){
    startU40 <- c('2017-10-02', '2018-10-01', '2019-09-30', '2020-09-28', '2021-10-04', '2022-10-03',
                  '2023-10-02', '2024-09-30', '2025-09-29', '2026-09-28', '2027-10-04', '2028-10-02')
    sluttU20 <- c('2018-05-20', '2019-05-19', '2020-05-17', '2021-05-23', '2022-05-22', '2023-05-21',
                  '2024-05-19', '2025-05-18', '2026-05-17', '2027-05-23', '2028-05-21', '2029-05-20')
    sesonger <- paste0(2017:2028,'-',18:29)
    ind <- which(sesonger == sesong)
    uker <- seq(as.Date(startU40[ind]), as.Date(sluttU20[ind]), "weeks") #sesong,

  InfluData$UkeAar <- factor(InfluData$UkeAar, levels=format(uker, '%G.%V'))
  }

  TabUkeRHF <- table(InfluData[ , c('UkeAar', 'RHF')])

  TabUkeRHF <- addmargins(TabUkeRHF)
  return(invisible(TabUkeRHF))
}
