#' Legger til og formaterer tidsvariable og noen flere
#'
#' @param RegData
#'
#' @return
#' @export
#'
tilretteleggBeredVar <- function(RegData){

RegData$Aar <- format(RegData$InnDato, '%Y')
RegData$UkeNr <- format(RegData$InnDato, '%V')
#RegData$UkeAar <- format(RegData$InnDato, '%G.%V') #%G -The week-based year, %V - Week of the year as decimal number (01–53) as defined in ISO 8601
#RegData$UkeAar <- as.factor(RegData$UkeAar)
RegData$Dag <- format(RegData$InnDato, '%d.%B')

RegData$RHF <- factor(sub('Helse ', '', RegData$RHF))
RegData$erMann <- factor(RegData$erMann, levels=0:1, labels=c('kvinner','menn'))
return(invisible(RegData))
}
