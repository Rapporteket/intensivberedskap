#Fil med samling av funksjoner som lager figurer for beredskapsskjema

#' Antall tilfeller for valgt tidsenhet og enhetsnivå. Filtreringer kan også gjøres.
#'
#' @param AntTab Dataramme med nødvendige figurparametre
#'
#' @return
#' @export
FigTidEnhet <- function(AntTab){

  # x11()
  FigTypUt <- rapFigurer::figtype(outfile='', height=3*800)	#res=96,
  farger <- FigTypUt$farger
  x_labs <- AntTab$Tab_tidy$Dato[-dim(AntTab$Tab_tidy)[1]]
  legendTxt <- names(AntTab$Tab_tidy[,-c(1, dim(AntTab$Tab_tidy)[2])])
  N_kat <- dim(AntTab$Tab_tidy)[2] - 2
  ymax <- max(rowSums(AntTab$Tab_tidy[-dim(AntTab$Tab_tidy)[1],-c(1, dim(AntTab$Tab_tidy)[2])], na.rm = T))
  pos <- barplot(t(as.matrix(AntTab$Tab_tidy[-dim(AntTab$Tab_tidy)[1],-c(1, dim(AntTab$Tab_tidy)[2])])),
                 xaxt="n", ylim = c(0,ymax), col = farger)
  # text(cex=1, x=pos-.25, y=-1.25, x_labs, xpd=TRUE, srt=45)
  text(cex=1, x=pos-.25, y=-0.25, x_labs, xpd=TRUE, srt=45, adj = 1)
  # legend('topleft', legender)
  legend('topleft', legendTxt, ncol=1, fill=farger[1:N_kat], border=farger[1:N_kat],
         bty='n', cex=1, xpd = T)

}


