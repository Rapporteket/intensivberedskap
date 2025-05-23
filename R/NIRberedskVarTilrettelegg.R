#' Funksjon for å tilrettelegge variable for beregning.
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk.
#' Videre bruk kan eksempelvis være beregning av AggVerdier eller gjennomsnitt.
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen.
#' Her kan mye hentes til analysebok
#'
#' @inheritParams NIRberedskFigAndeler
#' @param figurtype Hvilken figurtype det skal tilrettelegges variable for:
#'                'andeler', 'andelGrVar', 'andelTid', 'gjsnGrVar', 'gjsnTid'
#'
#' @return Definisjon av valgt variabel, samt flere andre parametre som
#' tittel, xAkseTxt, sortAvtagende (standard: TRUE)
#'       #Kan her definere opp alle aktuelle grupperingsvariable og deres tekst.
#' Variabeltyper: Numeriske, kategoriske, indikator
#' For hver valgtVar: Definer og gjør utvalg for variabelen
#' @export
#'

NIRberedskVarTilrettelegg  <- function(RegData, valgtVar, grVar='ShNavn', figurtype='andeler'){
      #, datoFra='2011-01-01', datoTil='3000-12-31',
      #		minald=0, maxald=110, erMann='',InnMaate='', dodInt='',outfile='',
      #		preprosess=1, hentData=0, reshID, enhetsUtvalg=1)


      "%i%" <- intersect


      #----------- Figurparametre ------------------------------
      cexgr <- 1	#Kan endres for enkeltvariable
      retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
      flerevar <- 0
      grtxt <- ''		#Spesifiseres for hver enkelt variabel
      grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
      grNavn <- ''
      varTxt <- ''
      xAkseTxt <- ''	#Benevning
      yAkseTxt <- ''
      pktTxt <- '' #(evt. søyletekst)
      txtEtiketter  <- ''	#legend
      verdier <- ''	#AggVerdier, gjennomsnitt, ...
      verdiTxt <- '' 	#pstTxt, ...
      strIfig <- ''		#cex
      sortAvtagende <- TRUE  #Sortering av resultater
      KImaal <- NA
      KImaaltxt=''
      varTxt <- 'hendelser'

      minald <- 0
      maxald <- 110
      tittel <- 'Mangler tittel'
      variable <- 'Ingen'
      RegData$Variabel <- 0



      #-------------------------------------


      if (valgtVar=='alder') {	#Fordeling, GjsnGrVar, GjsnTid
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel <- RegData$Alder  	#GjsnTid, GjsnGrVar
            xAkseTxt <- 'alder (år)'
            tittel <- 'Alder ved innleggelse på intensiv'
            if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
                  tittel <- 'alder ved innleggelse'}
            if (figurtype == 'andeler') {	#Fordelingsfigur
                  gr <- c(seq(0, 100, 10),150)
                  RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
                  grtxt <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99','100+')
                  xAkseTxt <- 'Aldersgrupper (år)'}
            sortAvtagende <- FALSE
      }


     if (valgtVar=='dod30d') { #AndelTid,AndelerGrVar
            RegData$Variabel <- RegData$Dod30
            varTxt <- 'pasienter som døde'
            tittel <- 'Opphold der pasienten døde innen 30 dager etter innleggelse'
            sortAvtagende <- FALSE
      }
      if (valgtVar=='dod90d') { #AndelTid,AndelerGrVar
        RegData$Variabel <- RegData$Dod90
        varTxt <- 'pasienter som døde'
        tittel <- 'Opphold der pasienten døde innen 90 dager etter innleggelse'
        sortAvtagende <- FALSE
      }
      if (valgtVar=='dod365d') { #AndelTid,AndelerGrVar
        RegData$Variabel <- RegData$Dod365
        varTxt <- 'pasienter som døde'
        tittel <- 'Opphold der pasienten døde innen ett år etter innleggelse'
        sortAvtagende <- FALSE
      }

      if (valgtVar=='bukleie') { #fordeling, AndelTid,AndelerGrVar
        #Kun pasienter med invasiv støtte'
        #indPotBuk <- which(RegData$Bukleie >-1)
        RegData <- RegData[which(RegData$InvasivVentilation>0), ] #'%i%' indPotBuk
        indBukleie <- which(RegData$Bukleie>0)
        if (figurtype == 'andeler') {	#Fordelingsfigur
          RegData <- RegData[indBukleie, ]
          tittel <- 'Opphold der pasienten har vært i bukleie'
          gr <- c(1:10,100)
          RegData$VariabelGr <- cut(RegData$Bukleie, breaks=gr, include.lowest=TRUE, right=FALSE)
          grtxt <- c(gr[1:9], '10+')
          xAkseTxt <- 'Antall ganger i bukleie'
        }
        if (figurtype %in% c('andelTid', 'andelGrVar')) {
          tittel <- 'Opphold med invasiv resp.støtte der pasienten er snudd til bukleie'
        RegData$Variabel[indBukleie] <- 1
        varTxt <- 'opphold i bukleie'
        }
        sortAvtagende <- FALSE
      }

      if (valgtVar=='dodeIntensiv') { #AndelTid,AndelerGrVar
            #Andel som dør på intensiv
            RegData$Variabel <- RegData$DischargedIntensiveStatus	#0: I live, 1: Død intensiv
            RegData <- RegData[which(RegData$Variabel %in% 0:1), ]
            varTxt <- 'pasienter som døde på intensiv'
            tittel <- 'Opphold der pasienten døde på intensiv'
            sortAvtagende <- FALSE
      }
      if (valgtVar=='ExtendedHemodynamicMonitoring') { #andeler, andelerGrVar
            #-1:Ikke svart, 1:Nei, 2:Picco, 3:PA
            tittel <- 'Utvidet hemodynamisk monitorering'
            if (figurtype=='andeler') {
                  gr <- c(-1,1:3)
                  RegData <- RegData[ which((RegData$ExtendedHemodynamicMonitoring %in% gr)), ]
                  RegData$VariabelGr <- factor(RegData$ExtendedHemodynamicMonitoring, levels=gr)
                  grtxt <- c('Ikke svart', 'Nei','Piccokateter o.l', 'Pulmonaliskateter')
            }
            if (figurtype=='andelGrVar'){
                  tittel <- 'Opphold med registrert utvidet hemodynamisk monitorering'
                  RegData <- RegData[ which(RegData$ExtendedHemodynamicMonitoring %in% 1:3), ]
                  RegData$Variabel[which(RegData$ExtendedHemodynamicMonitoring %in% 2:3)] <- 1
                  }
            #xAkseTxt <- ''
      }
      if (valgtVar=='ExtendedHemodynamicMonitoringPA') { #andelerGrVar
            tittel <- 'Utvidet hemodynamisk monitorering, PA'
            RegData <- RegData[which((RegData$ExtendedHemodynamicMonitoring %in% 2:3)), ]
            RegData$Variabel[which(RegData$ExtendedHemodynamicMonitoring == 3)] <- 1
            grtxt <- c('Ikke svart', 'Nei','Piccokateter o.l', 'Pulmonaliskateter')
            #xAkseTxt <- ''
      }

       if (valgtVar=='frailtyIndex') { #Andeler
        #1:9 Veldig sprek - Terminalt syk
        tittel <- 'Skrøpelighets indeks ("frailty")'
        gr <- 1:9
        RegData <- RegData[which((RegData$FrailtyIndex %in% gr)), ]  #Kun gyldige verdier: 0,6,8
        RegData$VariabelGr <- factor(RegData$FrailtyIndex, levels=gr)
        grtxt <- c('Veldig sprek', 'Sprek', 'Ok', 'Sårbar', 'Lett skrøpelig', 'Moderat skrøpelig',
                   'Alvorlig skøpelig', 'Svært skrøpelig', 'Terminal')
        xAkseTxt <- 'Grad av skrøpelighet'
        retn <- 'H'
      }

      if (valgtVar=='isolering') { #Andeler, andelerGrVar
            #-1 = Velg verdi, 1 = Ingen, 2 = Kontaktsmitte, 3 = Luftsmitte
            tittel <- 'Andel av opphold med registrert isolasjon av pasient'
         if (figurtype=='andeler') {
                  gr <- c(-1,1:5)
                  RegData <- RegData[ which((RegData$Isolation %in% gr)), ]
                  RegData$VariabelGr <- factor(RegData$Isolation, levels=gr)
                  grtxt <- c('Ikke svart', 'Ingen','Kontaktsmitte', 'Luftsmitte',
                             'Dråpesmitte ', 'Beskyttende isolasjon')
                  retn <- 'H'
            }
        if (figurtype=='andelGrVar'){
                  RegData <- RegData[ which(RegData$Isolation %in% 1:5), ]
                  RegData$Variabel[which(RegData$Isolation %in% 2:5)] <- 1
                  }
      }

     if (valgtVar == 'isoleringDogn' ) {   # Andeler,
            RegData <- RegData[which((RegData$Isolation %in% 2:5) & (RegData$IsolationDaysTotal>0)), ]
            RegData$Variabel <- RegData$IsolationDaysTotal
            xAkseTxt <- 'døgn'
            tittel <- 'Antall døgn (heltall) med registrert isolasjon av pasient'
            gr <- c(1, 2, 3, 4, 5, 6, 7, 14, 1000)
            RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c(1:6,'7-13','14+')
            sortAvtagende <- FALSE
      }

      if (valgtVar=='InnMaate') { #Andeler
            #InnMaate - 0-El, 6-Ak.m, 8-Ak.k, standard: alle (alt unntatt 0,6,8)
            tittel <- 'Fordeling av type opphold'
            gr <- c(0,6,8)
            RegData <- RegData[which((RegData$InnMaate %in% gr)), ]  #Kun gyldige verdier: 0,6,8
            RegData$VariabelGr <- factor(RegData$InnMaate, levels=gr)
            grtxt <- c('Planlagt operasjon','Akutt non-operativ', 'Akutt operasjon')
            xAkseTxt <- 'Type opphold'
      }


      if (valgtVar == 'liggetid') { #Andeler #GjsnGrVar
            #Liggetid bare >0
            RegData$Variabel  <- as.numeric(RegData$Liggetid)
            RegData <- RegData[which(RegData$Variabel>0), ]
            tittel <- 'Liggetid'
            if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
                  tittel <- 'liggetid'}
            gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 21, 28, 1000)
            RegData$VariabelGr <- cut(RegData$Liggetid, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','[14-21)','[21-28)','28+')
            xAkseTxt <- 'Liggetid (døgn)'
      }

      if (valgtVar=='erMann') { #AndelTid/GrVar
        RegData <- RegData[which(RegData$erMann %in% 0:1), ]  	#Tar bort ukjente
        RegData$Variabel <- RegData$erMann
        varTxt <- 'menn'
        tittel <- 'Andel av oppholdene hvor pasienten er mann'
      }
      if (valgtVar=='liggetidDod') { #AndelTid
            RegData <- RegData[which(RegData$liggetid>=0), ]    #Tar bort liggetid<0 samt NA
            RegData <- RegData[which(RegData$DischargedIntensiveStatus %in% 0:1), ]  	#Tar bort ukjente
            RegData$Variabel<-RegData$liggetid	#Liggetid for alle (total liggetid)
            RegData$Variabel2<- as.numeric(RegData$DischargedIntensiveStatus)*RegData$liggetid #Liggetid for døde
            varTxt <- 'pasienter som døde'
            tittel <- 'Andel av total liggetid brukt på dem som dør på intensiv'
      }


      if (valgtVar == 'nyreBeh' ) {   # Andeler, andelerGrVar
            tittel <- 'Andel av opphold med registrert nyreerstattende behandling'
            RegData <- RegData[which(RegData$InnDato>=as.Date('2015-01-01', tz='UTC')), ]
            if (figurtype == 'andelGrVar') {
                  RegData <- RegData[RegData$KidneyReplacingTreatment %in% 1:2,]
                  RegData$Variabel[which(RegData$KidneyReplacingTreatment ==1)] <- 1
            }
            if (figurtype == 'andeler') {
                  RegData <- RegData[which(RegData$KidneyReplacingTreatment ==1), ]		#Bare de som har fått behandling
                  RegData$VariabelGr <- 9
                  RegData$VariabelGr[which(RegData$Kontinuerlig == TRUE)] <- 1
                  RegData$VariabelGr[which(RegData$Intermitterende == TRUE)] <- 2
                  RegData$VariabelGr[(RegData$Kontinuerlig & RegData$Intermitterende) == TRUE] <- 3 #Overskriver tidl 1 eller 2
                  RegData$VariabelGr <- factor(RegData$VariabelGr, levels=c(1:3,9))
            }
            grtxt <- c('Kontinuerlig \n(hemo-/dia-filtrasjon)', 'Intermitterende \n(hemodialyse)', 'Begge', 'Ukjent')
            retn <- 'H'
            xAkseTxt <- 'Andel (%)'
      }


      if (valgtVar == 'nyreBehTid' ) {   # Andeler,
            #Noen har KidneyReplacingTreatment=1 uten å ha registrert tid. Velger ut de som har registrert tid.
            #indTid <- union(!is.na(RegData$KontinuerligDays),!is.na(RegData$IntermitterendeDays))
            RegData <- RegData[which(RegData$InnDato>=as.Date('2015-01-01', tz='UTC')) %i%
                                     which(RegData$KidneyReplacingTreatment == 1), ]
            RegData$Variabel <- rowSums(RegData[ ,c('KontinuerligDays','IntermitterendeDays')], na.rm = T)
            RegData <- RegData[which(RegData$Variabel>0), ]
            xAkseTxt <- 'døgn'
            tittel <- 'Antall døgn (heltall) med registrert nyreerstattende behandling'
            gr <- c(1, 2, 3, 4, 5, 6, 7, 14, 1000)
            RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c(1:6,'7-13','14+')
            sortAvtagende <- FALSE
      }

      if (valgtVar == 'overfTil'){ #Overf: 1= ikke overført, 2= overført
        tittel <- 'Pasienter overført TIL valgt(e) enheter'
        ind <- which(RegData$Overf==2 & !is.na(RegData$PatientTransferredToHospitalName))
        RegData <- RegData[ind,]
        RegData$VariabelGr <- as.factor(RegData$PatientTransferredToHospitalName)
        grtxt <- levels(RegData$VariabelGr)
        retn <- 'H'

      }

      if (valgtVar %in% c('regForsinkelseInn', 'regForsinkelseUt')) {  #Andeler, GjsnGrVar
        #Bare ferdigstilte?
        #Endrede skjema:

        RegData$RegForsink <- switch(valgtVar,
                                     regForsinkelseInn = as.numeric(difftime(RegData$CreationDate,
                                                                             RegData$Innleggelsestidspunkt, units = 'days')),
                                     regForsinkelseUt = as.numeric(difftime(RegData$FirstTimeClosed,
                                                                            RegData$DateDischargedIntensive, units = 'days'))
        )
        RegData <- RegData[which(RegData$RegForsink>0), ] #which(!is.na(RegData$RegForsink))
        tittel <- switch(valgtVar,
                         regForsinkelseInn='Tid fra første innleggelse på intensiv til opprettet innleggelsesskjema',
                         regForsinkelseUt = 'Tid fra utskriving intensiv til ferdigstilt utskrivingsskjema')
        subtxt <- 'døgn'
        gr <- c(0,1:7,500) #gr <- c(seq(0, 90, 10), 1000)
        RegData$VariabelGr <- cut(RegData$RegForsink, breaks = gr, include.lowest = TRUE, right = TRUE)
        grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '7+')
        cexgr <- 0.9
        xAkseTxt <- 'dager'
      }

       if (valgtVar=='reinn') { #AndelGrVar, AndelTid

            #Andel reinnlagte kun hvor dette er registrert. #Tidligere: Ja=1, nei=2, ukjent=9
            #Endret til: -1 = Velg verdi, 1 = Ja, 2 = Nei, 3 = Ukjent

            #Det er mange feil i variabelen ReAdmitted. Beregner derfor reinnleggelse basert på
            #Innleggelsestidspunkt , DateDischargedIntensive og en PasientID
            #29.06.18: Filtrer på kun ikke-overflyttede. Overf: (1= ikke overført, 2= overført)
            RegData <- RegData[which(RegData$InnDato >= as.Date('2016-01-01', tz='UTC')) %i%
                                     which(RegData$Overf==1), ]
            RegData <- FinnReinnleggelser(RegData=RegData, PasientID = 'PasientID')
            if (figurtype %in% c('andelGrVar', 'andelTid')) {
                  RegData$Variabel[which(RegData$Reinn==1)] <- 1}
            if (figurtype == 'gjsnGrVar') {RegData$Variabel <- RegData$Reinn}
            tittel <-c('Reinnleggelser på intensivavd. (innen 72t)',
                       'uten overflyttede pasienter')
            sortAvtagende <- FALSE      #Rekkefølge
            KImaal <- 4  #Reinnleggelser <4%
            KImaaltxt <- '<4'
      }

      if (valgtVar == 'RespiratortidInt') { #andeler, gjsnGrVar, GjsnTid
            RegData <- RegData[which(RegData$RespiratortidInt>0), ]
            RegData$Variabel  <- as.numeric(RegData$RespiratortidInt)
            tittel <- 'Respiratortid'
            if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
                  tittel <- 'respiratortid'}
            gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000) #c(0, exp(seq(0,log(30),length.out = 6)), 500),1)
            RegData$VariabelGr <- cut(RegData$RespiratortidInt, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
            xAkseTxt <- 'Respiratortid (døgn)'
            sortAvtagende <- TRUE      #Rekkefølge
      }
      if (valgtVar == 'respiratortidNonInv') { #andeler, gjsnGrVar, GjsnTid
            RegData <- RegData[which(RegData$NonInvasivVentilation>0), ]
            tittel <- 'Non-invasiv ventilasjon/maskeventilasjon'
            RegData$Variabel  <- as.numeric(RegData$NonInvasivVentilation)
            if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
                  tittel <- 'ventilasjonstid, maskeventilasjon'
            }
            gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)#c(0, exp(seq(0,log(30),length.out = 6)), 500),1)
            RegData$VariabelGr <- cut(RegData$NonInvasivVentilation, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
            xAkseTxt <- 'ventilasjonstid (døgn)'
            sortAvtagende <- TRUE      #Rekkefølge
      }
      if (valgtVar == 'respiratortidInv') { #Andeler #GjsnGrVar #AndelGrVar, GjsnTid
            #InvasivVentilation (pusterør/åpnet lufterør)
             ind <- which(RegData$InvasivVentilation>0) %i%  which(RegData$InnDato>=as.Date('2015-01-01', tz='UTC'))

            RegData <- RegData[ind,]
            if (figurtype %in% c('andeler', 'gjsnGrVar', 'gjsnTid')) {
                  RegData$Variabel  <- as.numeric(RegData$InvasivVentilation)
                  tittel <- 'invasiv ventilasjon '      #Andeler, GjsnGrVar
            }
            if (figurtype == 'andeler') {tittel <- 'Invasiv ventilasjon (inkl. overførte pasienter)'}
            if (figurtype %in% c('andelTid', 'andelGrVar')) {
                  RegData$Variabel[which(RegData$InvasivVentilation < 2.5)] <- 1
                   tittel <- 'Invasiv ventilasjon < 2,5 døgn (inkl. overførte pasienter)'}     #AndelGrVar, AndelTid
            gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000) #c(0, exp(seq(0,log(30),length.out = 6)), 500),1)
            RegData$VariabelGr <- cut(RegData$InvasivVentilation, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
            xAkseTxt <- 'ventilasjonstid (døgn)'
            varTxt <- 'med inv.ventilasjon < 2,5 døgn'
            sortAvtagende <- TRUE      #Rekkefølge
      }
      # if (valgtVar == 'respiratortidInvUoverf') { #Andeler #GjsnGrVar #AndelGrVar, GjsnTid
      #       #InvasivVentilation (pusterør/åpnet lufterør), uten overførte pasienter
      #
      #       ind <- which(RegData$InvasivVentilation>0) %i%
      #                              which(RegData$InnDato>=as.Date('2015-01-01', tz='UTC')) %i% which(RegData$Overf ==1)
      #       RegData <- RegData[ind,]
      #       if (figurtype %in% c('andeler', 'gjsnGrVar', 'gjsnTid')) {
      #             RegData$Variabel  <- as.numeric(RegData$InvasivVentilation)
      #             tittel <- 'invasiv ventilasjon (uten overførte pasienter)'
      #             KImaal <- 2.5 #Median respiratortid <2,5døgn
      #             #KImaal <- 50 #Over 50% med respiratortid <2,5døgn
      #             KImaaltxt <- '<2,5'
      #       }      #Andeler, GjsnGrVar
      #       if (figurtype == 'andeler') {tittel <- 'Invasiv ventilasjon (uten overførte pasienter)'}
      #       if (figurtype %in% c('andelTid', 'andelGrVar')) {
      #             RegData$Variabel[which(RegData$InvasivVentilation < 2.5)] <- 1
      #             #KImaal <- 50 #Over 50% med respiratortid <2,5døgn
      #             #KImaaltxt <- '>50'
      #             tittel <- 'Invasiv ventilasjon < 2,5 døgn (uten overførte pasienter)'}     #AndelGrVar, AndelTid
      #       gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000) #c(0, exp(seq(0,log(30),length.out = 6)), 500),1)
      #       RegData$VariabelGr <- cut(RegData$InvasivVentilation, breaks=gr, include.lowest=TRUE, right=FALSE)
      #       grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
      #       xAkseTxt <- 'ventilasjonstid (døgn)'
      #       varTxt <- 'med inv.ventilasjon < 2,5 døgn (uten overførte pasienter)'
      #       sortAvtagende <- TRUE      #Rekkefølge
      # }
      #
      # if (valgtVar=='respiratortidDod') {
      #       RegData <- RegData[which(RegData$respiratortid>=0), ]    #Tar bort respiratortid<0 samt NA
      #       RegData <- RegData[which(RegData$DischargedIntensiveStatus %in% 0:1), ]    #Tar bort ukjente. 0:levende, 1:døde
      #       RegData$Variabel<-RegData$respiratortid
      #       RegData$Variabel2<-as.numeric(RegData$DischargedIntensiveStatus)*RegData$respiratortid
      #       varTxt <- 'pasienter som døde'
      #       tittel <- 'Andel av total respiratortid brukt på dem som dør på intensiv'
      # }

      if (valgtVar=='respStotte') { #AndelGrVar, AndelTid
            #Fått respiratorstøtte. Ja=1, nei=2,
            RegData <- RegData[which(RegData$MechanicalRespirator %in% 1:2), ]
            RegData$Variabel[which(RegData$MechanicalRespirator==1)] <- 1
            varTxt <- 'pasienter med respiratorstøtte'
            tittel <-'Andel med respiratorstøtte'
            sortAvtagende <- TRUE      #Rekkefølge
      }
      if (valgtVar=='Saps2ScoreNumber') { #Andeler #GjsnGrVar
            #Tar ut SAPSII=0 (ikke scorede)
            #og de under 16år (tas ut i NIRutvalg)
            tittel <- 'Fordeling av SAPSII-skår'
            if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
                  tittel <- 'SAPSII' }
            minald <- max(16, minald)     #Bare voksne skal skåres
            RegData <- RegData[which(as.numeric(RegData$Saps2ScoreNumber) > 0), ]
            RegData$Variabel <- RegData$Saps2ScoreNumber
            gr <- c(seq(0, 100,10), 500)
            RegData$VariabelGr <- cut(RegData$Saps2ScoreNumber, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c('(0-10)','[10-20)','[20-30)','[30-40)','[40-50)','[50-60)','[60-70)','[70-80)','[80-90)','[90-100)','100+')
            xAkseTxt <- 'SAPSII-skår'
      }

      # if (valgtVar == 'SMR') { #GjsnGrVar
      #       #Tar ut reinnlagte på intensiv og  de med SAPSII=0 (ikke scorede)
      #       #05.06.2018 overflyttede skal ikke lenger tas bort
      #       #De under 16år tas ut i NIRutvalg
      #       #(TransferredStatus: 1= ikke overført, 2= overført),
      #       #Skal ikke brukes: ReAdmitted: #1:Ja, 2:Nei, 3:Ukjent, -1:Ikke utfylt
      #       #Reinn: #1:Ja, 2:Nei, 3:Ukjent, -1:Ikke utfylt
      #       minald <- max(16, minald)
      #       indMed <- which(as.numeric(RegData$SAPSII)>0) %i% #which(RegData$Overf==1) %i%
      #             which(RegData$InnDato >= as.Date('2016-01-01', tz='UTC'))
      #       RegData <- RegData[indMed,]
      #       RegData <- FinnReinnleggelser(RegData=RegData)
      #       RegData <- RegData[RegData$Reinn==2, ]
      #       RegData$Variabel <- RegData$SMR
      #       xAkseTxt <- 'Observert 30-dagers dødelighet / estimert dødelighet'
      #       sortAvtagende <- FALSE
      # }
      if (valgtVar == 'PIMdod') { #GjsnGrVar
        #Tar ut reinnlagte på intensiv og  de med SAPSII=0 (ikke scorede)
        #De under 16år tas ut i NIRutvalg
        #Reinn: #1:Ja, 2:Nei, 3:Ukjent, -1:Ikke utfylt
        maxald <- min(15, maxald)
        # indMed <- which(as.numeric(RegData$SAPSII)>0) %i%
        #   which(RegData$InnDato >= as.Date('2016-01-01', tz='UTC'))
        # RegData <- RegData[indMed,]
        #RegData <- FinnReinnleggelser(RegData=RegData)
        #RegData <- RegData[RegData$Reinn==2, ]
        RegData$Variabel <- RegData$PIM_Probability*100 #For å få samme format som SMR
        xAkseTxt <- 'Observert 30-dagers dødelighet / PIM-estimert dødelighet'
        sortAvtagende <- FALSE
      }



      #---------------KATEGORISKE

      if (valgtVar == 'PrimaryReasonAdmitted') { #Andeler
            #                       1:Respiratorisk svikt, 2:Sirk./kardiovaskulær svikt, 3:Gastroenterologisk svikt,
            #                       4:Nevrologisk svikt, 5:Sepsis, 6:Skade/traume, 7:Metabolsk/intoksikasjon, 8:Hematologisk svikt,
            #                       9:Nyresvikt, 10:Postoperativt, 11:Annet
            gr <- 1:11
            RegData <- RegData[which(RegData$PrimaryReasonAdmitted %in% gr)
                                     %i% which(RegData$InnDato >= as.Date('2016-01-01', tz='UTC')), ] #Innført ila 2015
            retn <- 'H'
            tittel <- 'Primærårsak til intensivoppholdet'
            RegData$VariabelGr <- factor(RegData$PrimaryReasonAdmitted, levels=gr)
            grtxt <- c('Respiratorisk svikt', 'Sirk./kardiovaskulær svikt', 'Gastroenterologisk svikt',
                       'Nevrologisk svikt', 'Sepsis', 'Skade/traume', 'Metabolsk/intoksikasjon', 'Hematologisk svikt',
                       'Nyresvikt', 'Postoperativt (komplikasjon \ntil anestesi/kirurgi)', 'Annet')
            cexgr <- 0.9
      }
      #-------------- SAMMENSATTE variable
      #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer
      #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
      # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
      # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
      # som 0.
      #Vi sender tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
      #(Alternativt kan vi gjøre beregninga her og sende tilbake teller og nevner for den sammensatte variabelen)

      if (valgtVar == 'inklKrit' ) {   # Andeler
            tittel <- 'Inklusjonskriterier, NIR'
            #RegData <- RegData[which(RegData$InnDato>=as.Date('2016-01-01', tz='UTC')), ]
            sortAvtagende <- T
            retn <- 'H'
            flerevar <- 1
            variable <- c('MoreThan24Hours',  'MechanicalRespirator', 'DeadPatientDuring24Hours',
                          'MovedPatientToAnotherIntensivDuring24Hours', 'VasoactiveInfusion' )
            #retn <- 'H'
            grtxt <- c('Liggetid over 24t', 'Mekanisk \nrespirasjonsstøtte', 'Død innen 24t',  'Overflyttet innen 24t',
                       'Infusjon av medikamenter for å \n endre hemodynamikk/sirkulasjon')
            ind01 <- which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
            ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
            RegData[ ,variable] <- NA
            RegData[ ,variable][ind01] <- 0
            RegData[ ,variable][ind1] <- 1
            xAkseTxt <- 'Andel opphold (%)'
            #Beregne direkte:
            #apply(RegData[,variable], MARGIN=2, FUN=function(x) sum(x %in% 0:1))
      }
      if (valgtVar == 'spesTiltak' ) {   # Andeler
            #SpecialMeasures
            tittel <- 'Spesielle tiltak/intervensjoner'
            sortAvtagende <- T
            retn <- 'H'
            flerevar <- 1
            variable <- c('TerapetiskHypotermi', 'EcmoEcla', 'Iabp', 'Impella', 'Icp', 'Oscillator', 'No',
                          'Leverdialyse', 'Hyperbar', 'Eeg')
            #retn <- 'H'
            grtxt <- c('Terapetisk hypotermi', 'ECMO/ECLA', 'IABP Aortaballongpumpe', 'Impella/VV-assist',
                       'ICP, intrakranielt trykk', 'Oscillator', 'NO-behandling',
                       'Leverdialyse', 'Hyperbar oksygenbeh.', 'Kontinuerlig EEG')
            #ind01 <- which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
            ind1 <- which(RegData[ ,variable] == TRUE, arr.ind=T) #Ja i alle variable
            RegData[ ,variable] <- 0
            RegData[ ,variable][ind1] <- 1
            xAkseTxt <- 'Andel opphold (%)'
      }


      if (valgtVar == 'risikoFakt' ) {   # Andeler

        tittel <- 'Risikofaktorer'
        sortAvtagende <- T
        retn <- 'H'
        flerevar <- 1
        variable <- c('IsCancerPatient', 'IsImpairedImmuneSystemIncludingHivPatient', 'IsDiabeticPatient',
                      'IsHeartDiseaseIncludingHypertensionPatient', 'IsObesePatient', 'IsAsthmaticPatient',
                      'IsChronicLungDiseasePatient', 'IsKidneyDiseaseIncludingFailurePatient',
                      'IsLiverDiseaseIncludingFailurePatient', 'IsChronicNeurologicNeuromuscularPatient',
                      'IsPregnant', 'IsActiveSmoker', 'IsRiskFactor')
        grtxt <- c('Kreft', 'Nedsatt immunforsvar', 'Diabetes',
                   'Hjertesykdom', 'Fedme (KMI>30)', 'Astma',
                   'Kronisk lungesykdom', 'Nyresykdom',
                   'Leversykdom', 'Nevrologisk/nevromusk.',
                   'Graviditet', 'Røyker', 'Minst én risikofaktor')
        # IsAsthmaticPatient
        # IsPregnant
        # IsDiabeticPatient
        # IsCancerPatient
        # for (var in variable) {
        #   print(class(RegData[,var]))}
        ind1 <- which(RegData[ ,variable] == TRUE, arr.ind=T) #Ja i alle variable
        RegData[ ,variable] <- 0
        RegData[ ,variable][ind1] <- 1
        xAkseTxt <- 'Andel opphold (%)'
}



      RegData$Variabel <- as.numeric(RegData$Variabel)

      UtData <- list(RegData=RegData, minald=minald, maxald=maxald,
                     grtxt=grtxt, cexgr=cexgr, varTxt=varTxt, xAkseTxt=xAkseTxt, KImaal=KImaal, KImaaltxt=KImaaltxt,
                     retn=retn,tittel=tittel, flerevar=flerevar, variable=variable, sortAvtagende=sortAvtagende)
      #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
      return(invisible(UtData))

}

