# Nok en fil med samling av funksjoner som lager tabeller for beredskapsskjema

#' Antall avdøde per tidsenhet (basert på avdøddato) og enhetsnivå. Filtreringer kan også gjøres.
#'
#' @inheritParams TabTidEnhet
#'
#' @return
#' @export
antallTidUtskrevneNIRberedskap <- function(RegData, tidsenhet='dag', erMann=9, resp=9,
                                           datoFra=0, datoTil=Sys.Date(), bekr=9, skjemastatus=9,
                                           dodInt=9, valgtRHF='Alle', velgAvd=0){
  #valgtEnhet representerer eget RHF/HF

  UtData <- NIRUtvalgBeredsk(RegData=RegData, datoFra=datoFra, datoTil=datoTil, erMann=erMann,
                             bekr=bekr, skjemastatus=skjemastatus, resp=resp,
                             dodInt=dodInt) # Kun døde på intensiv telles

  RegDataAlle <- UtData$RegData
  RegDataAlle$UtDato <- as.Date(RegDataAlle$DateDischargedIntensive, tz= 'UTC', format="%Y-%m-%d")

  if (datoFra != 0) {RegDataAlle <-
    RegDataAlle[which(RegDataAlle$UtDato >= datoFra) & which(RegDataAlle$UtDato <= datoTil), ]} # filtrerer på dato

  RegDataAlle$TidsVar <- switch (tidsenhet,
                                 dag = factor(format(RegDataAlle$UtDato, '%d.%m.%y'),
                                              levels = format(rev(seq(Sys.Date(), if (datoFra!=0) datoFra else min(RegDataAlle$UtDato, na.rm = T),
                                                                      by=paste0('-1 day'))), '%d.%m.%y')),
                                 uke = factor(paste0('Uke ', format(RegDataAlle$UtDato, '%V.%y')),
                                              levels = paste0('Uke ', format(rev(seq(Sys.Date(), if (datoFra!=0) datoFra else min(RegDataAlle$UtDato, na.rm = T),
                                                                                     by=paste0('-1 week'))), '%V.%y'))),
                                 maaned = factor(format(RegDataAlle$UtDato, '%b %y'),
                                                 levels = format(rev(seq(Sys.Date(), if (datoFra!=0) datoFra else
                                                   min(RegDataAlle$UtDato, na.rm = T), by=paste0('-1 month'))), '%b %y')))

  RegDataAlle <- RegDataAlle[!is.na(RegDataAlle$TidsVar), ]

  ##############################
  RegData <- if(valgtRHF=='Alle') {RegDataAlle} else {RegDataAlle[RegDataAlle$RHFut == valgtRHF, ]}
  Ntest <- dim(RegData)[1]

  if (valgtRHF == 'Ukjent'){
    TabTidEnh <- as.matrix(c(table(RegDataAlle$TidsVar), dim(RegDataAlle)[1]), ncol=1)
    colnames(TabTidEnh) <- 'Hele landet'
  } else {

    enhetsNivaa <- ifelse(as.character(valgtRHF)=='Alle', 'RHFut', 'HFut')

    RegData$EnhetsNivaaVar <- RegData[ ,enhetsNivaa]
    kolNavnSum <- switch(enhetsNivaa,
                         RHFut = 'Hele landet',
                         HFut = paste0(valgtRHF, ', totalt'))
    if (Ntest==0) {
      TabTidEnh <- matrix(0, ncol=1, nrow=length(levels(RegData$TidsVar)) + 1,
                          dimnames = list(c(levels(RegData$TidsVar), 'Totalt'), valgtRHF)) #table(RegData$TidsVar)
    }else{
      TabTidEnh <- table(RegData[ , c('TidsVar', enhetsNivaa)]) #ftable(RegData[ , c(TidsVar, enhetsNivaa, 'Korona')], row.vars =TidsVar)
      TabTidEnh <- addmargins(TabTidEnh, FUN=list('Totalt'=sum, 'Hele landet' = sum), quiet=TRUE)
      colnames(TabTidEnh)[ncol(TabTidEnh)] <- kolNavnSum
    }
    if (valgtRHF != 'Alle'){
      TabTidEnh <- cbind(TabTidEnh,
                         'Hele landet'= c(table(RegDataAlle$TidsVar), dim(RegDataAlle)[1]))}
  }
  TabTidEnh_tidy <- tidyr::as_tibble(as.data.frame.matrix(TabTidEnh), rownames='Tid')

  return(UtData <- list(utvalTxt=UtData$utvalgTxt, Ntest=dim(RegData)[1],
                        Tab_tidy=TabTidEnh_tidy))
}


#' Funksjon som avgjør om en pasient er inneliggende på aktuell dato
#'
#' Returnerer TRUE for datoer pasienten er inneliggende
#'
#' @param datoer datoer som inneligging skal avgjøres for
#' @param regdata Dataramme som inneholder InnDato og Utdato per pasient
#'
#' @return
#' @export
erInneliggende <- function(datoer, regdata){
  # regnes som inneliggende på aktuell dato hvis den faller mellom inn- og utdato eller
  # er etter inndato og det ikke finnes utddato. Flere betingelser kan legges til.

  auxfunc <- function(x) {
    (x > regdata$InnDato & x <= regdata$UtDato) | (x > regdata$InnDato & is.na(regdata$UtDato))}
  map_df(datoer, auxfunc)
}


#' Transponer output fra tidyr::summarize
#'
#' Denne funksjonen tar som input resultatet av tidyr::summarize og returnerer dens
#' transponerte uten at formatene endres.
#'
#' @param x En dataramme med output fra en gruppert summarise
#' @param grvarnavn Navnet som skal gis til første kolonne i output
#' @return tr_frame Den transponerte av inputen
#'
#' @export
tr_summarize_output <- function(x, grvarnavn=''){

  rekkefolge <- names(x)[-1]
  x <- x %>% mutate_if(is.factor, as.character)
  rekkefolge_col <- x[[1]]
  y <- x %>% gather(names(x)[-1], key=nokkel, value = verdi) %>%
    spread(key=names(x)[1], value = verdi)
  y <- y[match(rekkefolge, y$nokkel), ]
  names(y)[1] <- grvarnavn
  y <- y[,  c(1, match(rekkefolge_col, names(y)))]
  y
}

#' Antall inneliggende per tidsenhet  og enhetsnivå. Filtreringer kan også gjøres.
#' Detaljerinsnivå er styrt av tilgangsnivå
#'
#' @param RegData dataramme med preprossesserte data
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned'
#' ('RHF', 'HF', 'ShNavn') resultatene skal vises for
#' @inheritParams NIRUtvalgBeredsk
#'
#' @return
#' @export
antallTidInneliggendeBeredskap <- function(RegData, tidsenhet='dag', erMann=9, resp=9, datoFra=0,
                                  bekr=9, skjemastatus=9, dodInt=9, valgtRHF='Alle', velgAvd=0){

  UtData <- NIRUtvalgBeredsk(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann,
                             bekr=bekr, skjemastatus=skjemastatus, resp=resp,
                             dodInt=dodInt) # Kun døde på intensiv telles

  RegDataAlle <- UtData$RegData
  RegDataAlle$UtDato <- as.Date(RegDataAlle$DateDischargedIntensive, tz= 'UTC', format="%Y-%m-%d")

  if (datoFra != 0) {RegDataAlle <- RegDataAlle[RegDataAlle$UtDato >= datoFra | is.na(RegDataAlle$UtDato), ]} # filtrerer på dato

  datoer <- seq(if (datoFra!=0) as.Date(datoFra, tz= 'UTC', format="%Y-%m-%d") else min(RegDataAlle$InnDato), today(), by="day")

  if (tidsenhet=='dag') {
    names(datoer) <- format(datoer, '%d.%m.%y')
    aux <- erInneliggende(datoer = datoer, regdata = RegDataAlle)
    RegDataAlle <- bind_cols(RegDataAlle[ , c("PasientID", "HFut", "RHFut")], aux)
  } else {
    names(datoer) <- datoer
    aux <- erInneliggende(datoer = datoer, regdata = RegDataAlle)
    aux <- bind_cols(as_tibble(RegDataAlle)[, "PasientID"], aux)
    aux <- aux %>% gather(names(aux)[-1], key=Tid, value = verdi)
    aux$Tid <- as.Date(aux$Tid)
    aux$Tid <- switch (tidsenhet,
                       'uke' = paste0('Uke ', format(aux$Tid, "%V.%y")),
                       'maaned' = format(aux$Tid, "%b %y")
    )
    aux <- aux %>% group_by(PasientID, Tid) %>%
      summarise(er_inne = max(verdi))
    aux <- aux %>% spread(key=Tid, value = er_inne)
    RegDataAlle <- merge(RegDataAlle[ , c("PasientID", "HFut", "RHFut")], aux, by = 'PasientID')
  }

  switch (tidsenhet,
          uke = datoer <- unique(paste0('Uke ', format(datoer, '%V.%y'))),
          maaned = datoer <- unique(format(datoer, '%b %y')))
  if (tidsenhet %in% c("uke", "maaned")) {
    names(datoer) <- datoer
  }

  RegData <- if(valgtRHF=='Alle') {RegDataAlle} else {RegDataAlle[RegDataAlle$RHFut == valgtRHF, ]}
  Ntest <- dim(RegData)[1]


  enhetsNivaa <- ifelse(as.character(valgtRHF)=='Alle', 'RHFut', 'HFut')

  RegData$EnhNivaaVis <- RegData[ ,enhetsNivaa]
  kolNavnSum <- switch(enhetsNivaa,
                       RHFut = 'Hele landet',
                       HFut = paste0(valgtRHF, ', totalt'))
  if (Ntest==0) {
    TabTidEnh <- matrix(0, ncol=1, nrow=length(datoer) + 1,
                        dimnames = list(c(names(datoer), 'Totalt'), kolNavnSum))
  } else {
    total <- RegData %>%
      group_by(EnhNivaaVis) %>%
      summarise(Totalt = length(unique(PasientID))) %>%
      tr_summarize_output(grvarnavn = 'Tid')
    TabTidEnh <-
      RegData[,c("EnhNivaaVis", names(datoer))] %>%
      group_by(EnhNivaaVis) %>%
      summarise_all(sum) %>%
      tr_summarize_output(grvarnavn = 'Tid') %>%
      bind_rows(total) %>%
      mutate('Hele landet' = select(., names(.)[-1]) %>% rowSums())
    # bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Totalt")))
    colnames(TabTidEnh)[ncol(TabTidEnh)] <- kolNavnSum
  }

  if (enhetsNivaa != 'Alle'){
    aux <- bind_cols(kol1 = 'Hele landet', RegDataAlle[, c(names(datoer))] %>% summarise_all(sum)) %>%
      tr_summarize_output(grvarnavn = 'Tid') %>%
      bind_rows(tibble(Tid = "Totalt", "Hele landet"=as.integer(length(unique(RegDataAlle$PasientID)))))
    TabTidEnh[, "Hele landet"] <- aux[, "Hele landet"]
  }

  # if (enhetsNivaa=='Alle'){valgtEnhet<-NULL}
  return(UtData <- list(utvalgTxt=UtData$utvalgTxt, Ntest=dim(RegData)[1], Tab_tidy=TabTidEnh))
}





