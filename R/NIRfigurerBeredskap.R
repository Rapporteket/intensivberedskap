#Fil med samling av funksjoner som lager figurer for beredskapsskjema

#' Antall tilfeller for valgt tidsenhet og enhetsnivå. Filtreringer kan også gjøres.
#'
#' @param AntTab Dataramme med nødvendige figurparametre
#'
#' @return
#' @export
FigTidEnhet <- function(AntTab, outfile=''){

  # x11()
  NutvTxt <- length(AntTab$utvalTxt)
  FigTypUt <- rapFigurer::figtype(outfile=outfile, fargepalett = 'OffAlleFarger')
  par('fig'=c(0.05, 1, 0, 1-0.02*(NutvTxt-1)))
  farger <- FigTypUt$farger
  x_labs <- AntTab$Tab_tidy$Dato[-dim(AntTab$Tab_tidy)[1]]
  legendTxt <- names(AntTab$Tab_tidy[,-c(1, dim(AntTab$Tab_tidy)[2])])
  N_kat <- dim(AntTab$Tab_tidy)[2] - 2
  ymax <- max(rowSums(AntTab$Tab_tidy[-dim(AntTab$Tab_tidy)[1],-c(1, dim(AntTab$Tab_tidy)[2])], na.rm = T))
  pos <- barplot(t(as.matrix(AntTab$Tab_tidy[-dim(AntTab$Tab_tidy)[1],-c(1, dim(AntTab$Tab_tidy)[2])])),
                 xaxt="n", ylim = c(0,ymax), col = farger[1:N_kat], ylab = 'Antall', main = 'Antall tilfeller')
  text(cex=1, x=pos-.25, y=-0.25, x_labs, xpd=TRUE, srt=45, adj = 1)
  legend('topleft', rev(legendTxt), ncol=1, fill=rev(farger[1:N_kat]), border=rev(farger[1:N_kat]),
         bty='n', cex=1, xpd = T, )

  mtext(AntTab$utvalTxt, side=3, las=1, cex=0.9, adj=0, line=c(3+0.8*((NutvTxt-1):0)))
  par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}
}



# FigAlderKjonn <- function(RegData, valgtRHF='Alle', bekr=9, skjemastatus=9,
#                      dodInt=9,erMann=9){#enhetsNivaa='RHF'
#
#   #if (valgtRHF != 'Alle'){RegData$RHF <- factor(RegData$RHF, levels=unique(c(levels(as.factor(RegData$RHF)), valgtRHF)))}
#   RegData$RHF <- as.factor(RegData$RHF)
#   UtData <- NIRUtvalgBeredsk(RegData=RegData,
#                              #valgtRHF=valgtRHF,
#                              bekr=bekr,
#                              dodInt = dodInt,
#                              erMann = erMann,
#                              skjemastatus=skjemastatus
#   )
#   RegData <- UtData$RegData
#   RegData$EnhetsNivaaVar <- RegData$RHF #RegData[ , enhetsNivaa]
#
#   N <- dim(RegData)[1]
#   gr <- seq(0, 90, ifelse(N<100, 25, 10) )
#   RegData$AldersGr <- cut(RegData$Alder, breaks=c(gr, 110), include.lowest=TRUE, right=FALSE)
#   grtxt <- if(N<100){c('0-24', '25-49', "50-74", "75+")} else {
#     c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')}
#   levels(RegData$AldersGr) <- grtxt #c(levels(RegData$AldersGr)[-length(gr)], paste0(max(gr),'+'))
#   TabAlder <- table(RegData$AldersGr, RegData$EnhetsNivaaVar)
#   TabAlder <- addmargins(TabAlder) #switch(enhetsNivaa, RHF = 'Totalt', HF = paste0(valgtRHF, ', totalt'))
#   TabAlderPst <-prop.table(TabAlder[-nrow(TabAlder),],2)*100
#
#   TabAlderAlle <- cbind(
#     'Antall, tot.' = TabAlder[,'Sum'],
#     'Andel, tot.' = paste0(sprintf('%.0f', c(TabAlderPst[,'Sum'], 100)), ' %')
#   )
#   TabAlderUt <-  if (valgtRHF %in% levels(RegData$RHF)){
#     TabAlderUt <- cbind(
#       'Antall, eget' = TabAlder[ ,valgtRHF],
#       'Andel, eget' = paste0(sprintf('%.0f', c(TabAlderPst[ ,valgtRHF], 100)), ' %'),
#       TabAlderAlle)
#   } else {TabAlderAlle}
#
#   return(invisible(UtData <-
#                      list(Tab=TabAlderUt,
#                           utvalgTxt=c(UtData$utvalgTxt, paste0('Valgt RHF: ', valgtRHF)))))
# }
#
#
# Fiks_duplikater <- function(RegData) {
#
#   lengde_unik <- function(x){length(unique(x))}
#   aux <- RegData %>% group_by(PasientID) %>% summarise_all(lengde_unik)
#
#   names(colSums(aux[, -1])[colSums(aux[, -1]) > dim(aux)[1]])
#
#
# }


