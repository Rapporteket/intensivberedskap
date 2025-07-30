#
# Shiny-app for intensivberedskap
# Inneholder hovedsakelig resultater for covid- og influensapasienter

# Flunkende ny applikasjon for NIR-bereskap

#' Server logic for the rapRegTemplate app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export
ui_NIRbered <- function() { #input, output, session

  library(intensivberedskap)
  addResourcePath('rap', system.file('www', package='rapbase'))
  context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt

  idag <- Sys.Date()
  datoTil <- as.POSIXlt(idag)
  startDato <- '2020-03-01'

  regTitle <- paste0('Norsk Intensiv- og kriseregister, Covid- og Influensaregistrerigner ',ifelse(context %in% c('QA', 'QAX'), 'QA',''))


  navbarPage(
    id='hovedark',
    title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",

    #------------Oversiktsside-----------------------------
    tabPanel("Oversikt",
             shinyjs::useShinyjs(),
             sidebarPanel(id = 'brukervalgStartside',
                          width = 3,
                          uiOutput('CoroRappTxt'),
                          downloadButton(outputId = 'CoroRapp.pdf', label='Last ned covid-19rapport', class = "butt"),
                          tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                          br()
                          # selectInput(inputId = "valgtNivaa", label="Velg enhetsnivå for rapporten",
                          #             choices = enhetsNivaa
                          # ),
                          # br(),
                          # h4('Gjør filtreringer/utvalg i tabeller og figurer:'),
                          #
                          # selectInput(inputId = "valgtRHF", label="Velg RHF",
                          #             choices = rhfNavn
                          # ),
                          # selectInput(inputId = "bekr", label="Bekreftet/Mistenkt",
                          #             choices = c("Alle"=9, "Bekreftet"=1, "Mistenkt"=0)
                          # ),
                          # selectInput(inputId = "skjemastatus", label="Skjemastatus",
                          #             choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
                          # ),
                          # selectInput(inputId = "resp", label="Respiratorbehandlet (invasiv+non-inv.)",
                          #             choices = c("Alle"=9, "Ja"=1, "Nei"=2)
                          # ),
                          # selectInput(inputId = "dodInt", label="Tilstand ut fra intensiv",
                          #             choices = c("Alle"=9, "Død"=1, "Levende"=0)
                          # ),
                          # selectInput(inputId = "erMann", label="Kjønn",
                          #             choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
                          # ),
                          # dateRangeInput(inputId = 'datovalgStart', start = startDato, end = idag, #'2020-05-10',
                          #                label = "Tidsperiode", separator="t.o.m.", language="nb"
                          # ),
                          # h4('Kun for risikofaktorer:'),
                          # sliderInput(inputId="alder", label = "Alder",
                          #             min = 0, max = 110,
                          #             value = c(0, 110),
                          #             step = 10
                          # ),
                          # br(),
                          # actionButton("tilbakestillValg", label="Tilbakestill valg"),
                          # br(),
                          # selectInput(inputId = "bildeformatAldKj",
                          #             label = "Velg format for nedlasting av figur",
                          #             choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))

             ),
             mainPanel(width = 9,
                       tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
                       rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE),
                       h1('Rapporteket-NIR-beredskap er under utvikling!!! ', style = "color:red"),
                       h3('Resultater skal endres til at de er basert på opphold i stedet for personer/forløp. I den forbindelse ønsker registeret:'),
                       h4('1: Gjere om tekst slik at det er tydeleg overalt (automatiserte rapportar, rapporteket i øvrig)'),
                       h4('Legge til rette slik at tal pasientar går frem i ein samletabell på alle nivå.'),
                       h4('Legge til rette slik at tal pasientforløp går frem i ein samletabell på alle nivå. (Overflyttingar)'),
                       h4('Kunne gjere greie for tid mellom innleggingar (median og gjennomsnitt med spredningsmål, evt også fordelingsfigur)'),

                       h3('Resultater fra intensivregisterets beredskapsskjema for mistenkt/bekreftet
                       Coronasmitte.'),
                       h4('Merk at resultatene er basert på til dels ikke-fullstendige registreringer'),
                       br(),
                       fluidRow(
                         column(width = 4,
                                h4('Forløp uten registrert ut-tid fra intensiv'), #, align='center'),
                                uiOutput('utvalgNaa'),
                                tableOutput('tabECMOrespirator'),
                                br(),
                                h4('Forløp registrert som utskrevet, uten ferdigstilt skjema:'),
                                uiOutput('RegIlimbo')
                         ),
                         column(width=5, offset=1,
                                uiOutput('tittelFerdigeReg'),
                                uiOutput('utvalgFerdigeReg'),
                                tableOutput('tabFerdigeReg')
                         )),

                       h3('Antall inneliggende i hvert HF'),
                       h5('Mistenkte og bekreftede'),
                       # tableOutput('tabInneliggHF'),

                       h3('Antall ny-innlagte pasienter, siste 10 dager'),
                       h4('NB: Inkluderer ikke overføringer mellom intensivenheter'),
                       uiOutput('utvalgHoved'),
                       tableOutput('tabTidEnhet'),
                       br(),
                       fluidRow(
                         column(width=3,
                                h3('Risikofaktorer'),
                                uiOutput('utvalgRisiko'),
                                tableOutput('tabRisikofaktorer'))
                         # column(width=5, offset=1,
                         #        h3('Aldersfordeling'),
                         #        plotOutput("FigurAldersfordeling", height="auto"),
                         #        downloadButton("LastNedFigAldKj", "Last ned figur"),
                         #        downloadButton("lastNedAldKj", "Last ned tabell")
                         #
                         # )
                         )
             ) #main
    ), #tab Oversikt

    #------------Figurer-----------------------------------
    # tabPanel("Antall intensivpasienter",
    #          covidfigurer_UI(id = "covidfigurer_id", rhfNavn=egetRHF)
    # ),
    # tabPanel(p("Fordelingsfigurer",
    #            title='Fordelingsfigurer for variabler registrert under intensivoppholdet'),
    #          value = 'Fordelingsfigurer',
    #          sidebarPanel(
    #            h4('Her kan man velge hvilken variabel man ønsker å se på og gjøre ulike filtreringer.'),
    #            selectInput(
    #              inputId = "valgtVar", label="Velg variabel",
    #              selected = 'regForsinkelseInn',
    #              choices = c('Alder' = 'alder',
    #                          'Bukleie' = 'bukleie',
    #                          'Hemodynamisk overvåkn.' = 'ExtendedHemodynamicMonitoring',
    #                          'Frailty index' = 'frailtyIndex',
    #                          'Liggetid' = 'liggetid',
    #                          'Primærårsak' = 'PrimaryReasonAdmitted',
    #                          'Registreringsforsinkelse, innleggelse' = 'regForsinkelseInn',
    #                          'Registreringsforsinkelse, utskriving' = 'regForsinkelseUt',
    #                          'Respiratortid, totalt' = 'RespiratortidInt',
    #                          'Respiratortid, ikke-invasiv' = 'respiratortidNonInv',
    #                          'Respiratortid, invasiv' = 'respiratortidInv',
    #                          'SAPSII-skår (alvorlighet av sykd.)' = 'Saps2ScoreNumber'
    #                          #,'Spesielle tiltak' = 'spesTiltak'
    #              )
    #            ),
    #            selectInput(inputId = "bekrFord", label="Bekreftet/Mistenkt",
    #                        choices = c("Bekreftet"=1, "Alle"=9, "Mistenkt"=0)
    #            ),
    #            dateRangeInput(inputId = 'datovalg', start = startDato, end = Sys.Date(),
    #                           label = "Tidsperiode", separator="t.o.m.", language="nb"
    #            ),
    #            selectInput(inputId = "erMannFord", label="Kjønn",
    #                        choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
    #            ),
    #            selectInput(inputId = "bildeformatFord",
    #                        label = "Velg format for nedlasting av figur",
    #                        choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')
    #            )
    #          ),
    #          mainPanel(
    #            tabsetPanel(
    #              tabPanel(
    #                'Figur',
    #                h4('Data er aggregerte til pasientnivå og inneholder kun registreringer
    #                            hvor pasienten har både beredskapsskjema og ferdigstilte intensivskjema.'),
    #                plotOutput('fordelinger', height="auto"),
    #                downloadButton(outputId = "LastNedFigFord", label = "Last ned figur")),
    #              tabPanel(
    #                'Tabell',
    #                uiOutput("tittelFord"),
    #                tableOutput('fordelingTab'),
    #                downloadButton(outputId = 'lastNed_tabFord', label='Last ned tabell') #, class = "butt")
    #              )
    #            )
    #          )
    # ), #Tab, fordelinger

    #---------Datakvalitet-----------------
    tabPanel(#p('Tilhørende intensivskjema som mangler ferdigstillelse'),
      title = 'Datakvalitet',
      value = 'Datakvalitet',
      sidebarLayout(
        sidebarPanel(width = 2,
                     dateInput(inputId = 'datoFraMI', label = "Fra dato: ",
                               value = as.character(Sys.Date()-365)) #, min = startDato, max = Sys.Date())
        ),
        mainPanel(
          h4('Ferdistilte beredskapsskjema som mangler ferdigstillelse av tilhørende intensivskjema'),
          br(),
          h5(tags$b('SkjemaGUID'),' er beredskapsskjemaets skjemaID'),
          h5(tags$b('HovedskjemaGUID'),' er intensivskjemaes skjemaID'),
          br(),
          downloadButton(outputId = 'lastNed_ManglerIntSkjema', label='Last ned tabell'),
          br(),
          uiOutput("tabManglerIntSkjema")
        )
      )
    ), #tab Datakvalitet

    #------------ Influensa -----------------------
    #Resultater fra influensaskjema


    tabPanel(title = 'Influensa',
             value = 'Influensa',
             sidebarPanel(id = 'brukervalgInfluensa',
                          width = 3,
                          h4(paste0('Influensarapport med samling av resultater for sesongen ', uiOutput('sesongSiste'))),
                          h5('(Influensasesong oppdateres automatisk så snart det registreres data for en ny sesong)'),
                          br(),
                          downloadButton(outputId = 'InfluRapp.pdf', label='Last ned Influensarapport', class = "butt"),
                          tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                          h5('Influensarapporten kan man få regelmessig tilsendt på e-post.
                           Gå til fanen "Abonnement" for å bestille dette.'),
                          br(),
                          br(),
                          h3('Gjør filtreringer/utvalg i tabellene til høyre:'),
                          uiOutput('velgSesong'),
                          # selectInput(inputId = "sesongInflu", label="Velg influensasesong",
                          #             choices = sesongValg, selected = sesongSiste),
                          h5('(Valgene omfatter alle sesonger hvor det er gjort registreringer.)'),

                          conditionalPanel("input.influensa == 'Oppsummeringer'",
                                           selectInput(inputId = "bekrInflu", label="Bekreftet/Mistenkt",
                                                       choices = c("Alle"=9, "Bekreftet"=1, "Mistenkt"=0)),
                                           selectInput(inputId = "skjemastatusInflu", label="Skjemastatus",
                                                       choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
                                           )),
                          selectInput(inputId = "erMannInflu", label="Kjønn",
                                      choices = c("Begge"=9, "Kvinne"=0, "Mann"=1)
                          ),
                          selectInput(inputId = "dodIntInflu", label="Tilstand ut fra intensiv",
                                      choices = c("Alle"=9, "Død"=1, "Levende"=0, "Ukjent"=-1)
                          ),
                          conditionalPanel("input.influensa == 'Fordelinger'",
                                           selectInput(
                                             inputId = "valgtVarInflu", label="Velg variabel",
                                             selected = 'regForsinkelseInn',
                                             choices = c('Alder' = 'alder',
                                                         'Hemodynamisk overvåkn.' = 'ExtendedHemodynamicMonitoring', #- int.variabel
                                                         'Frailty index' = 'frailtyIndex', #- int.variabel
                                                         'Liggetid' = 'liggetid',
                                                         'Registreringsforsinkelse, innleggelse' = 'regForsinkelseInn',
                                                         'Respiratortid, totalt' = 'RespiratortidInt',
                                                         'Respiratortid, ikke-invasiv' = 'respiratortidNonInv',
                                                         'Respiratortid, invasiv' = 'respiratortidInv',
                                                         'Risikofaktorer' = 'risikoFakt',
                                                         'SAPSII-skår (alvorlighet av sykd.)' = 'Saps2ScoreNumber', #- int.
                                                         'Spesielle tiltak' = 'spesTiltak'
                                             )
                                           )),

                          actionButton("tilbakestillValgInflu", label="Tilbakestill valg"),
             ),

             mainPanel(
               width = 9,
               uiOutput('TittelInflu'),
               h4('Alle resultater er basert på opphold. (Det er ikke gjort noen aggregering på person.)'),
               br(),
               tabsetPanel(
                 id = 'influensa',
                 tabPanel(
                   'Oppsummeringer',
                   h3('Utvalg som er gjort:'),
                   uiOutput('UtvalgInflu'),
                   br(),

                   fluidRow(
                     column(width = 4,
                            h3('Inneliggende, dvs. opphold uten registrert ut-tid fra intensiv'),
                            #uiOutput('utvalgNaaInflu'),
                            tableOutput('tabECMOrespiratorInflu')
                     ),
                     column(width=5, offset=1,
                            uiOutput('tittelFerdigeRegInflu'),
                            #uiOutput('utvalgFerdigeRegInflu'),
                            tableOutput('tabFerdigeRegInflu')
                     )),

                   br(),
                   conditionalPanel("input.sesongInflu != 'Alle'",
                                    h3('Antall registrerte opphold per uke'),
                                    tableOutput('tabInfluUkeRHF')
                   )

                 ),
                 tabPanel('Fordelinger',
                          tabsetPanel(
                            tabPanel('Figur',
                                     plotOutput('fordInflu', height="auto"),
                                     downloadButton(outputId = "LastNedFigFordInflu", label = "Last ned figur")
                            ),
                            tabPanel('Tabell',
                                     uiOutput("tittelFordInflu"),
                                     tableOutput('fordelingTabInflu'),
                                     downloadButton(outputId = 'lastNed_tabFordInflu', label='Last ned tabell') #, class = "butt")
                            ))) #Fordelinger
               ))#main
    ), #Influensaresultater

    #-----------Abonnement--------------------------------
    # tabPanel(p("Abonnement",
    #            title='Bestill automatisk utsending av rapporter på e-post'),
    #          value = 'Abonnement',
    #
    #          sidebarLayout(
    #            sidebarPanel(
    #              autoReportInput("beredAbb")
    #            ),
    #            shiny::mainPanel(
    #              autoReportUI("beredAbb")
    #            )
    #          )
    # ), #tab abonnement

    #-----------Registeradmin.------------
    tabPanel(p("Registeradmin."),
             value = 'Registeradmin.',

             tabsetPanel(
               tabPanel(p('Oversiktsdata',
                          title='Data til artikkel'),
                        value = 'Oversiktsdata',

                        sidebarLayout(
                          sidebarPanel(width = 4,
                                       h3('Data fra beredskapsskjema og tilhørende intensivskjema'),
                                       h4('Koblede RÅdata, opphold'),
                                       h5('Rådata inneholder alle beredskapsskjema og alle variabler fra tilhørende
                                     intensivskjema hvis dette finnes. Der variabelen finnes i begge, hentes den stort
                                     sett fra intensivskjema. Merk at ikke alle beredskapsskjema er ferdigstilte.'),
                                       downloadButton(outputId = 'lastNed_dataBeredNIRraa', label='Last ned rådata'),
                                       br(),
                                       br(),
                                       h4('Koblet aggregert datatsett: Covid-pasienter'),
                                       h5('Inneholder alle registrerte pasienter som har ferdigstilt intensivskjema tilknyttet
                                        alle sine beredskapsskjema. Datasettet inneholder bare de variabler
                                        hvor vi har definert aggregeringsregler.'),
                                       downloadButton(outputId = 'lastNed_dataBeredNIR', label='Last ned data'),
                                       br(),
                                       br(),
                                       h4('Gjør utvalg av data'),
                                       selectInput(inputId = "erMannArt", label="Kjønn",
                                                   choices = c("Begge"=9, "Kvinne"=0, "Mann"=1)),
                                       br(),
                                       br(),
                                       h3('Valg som gjelder registeringsforsinkelse'),
                                       selectInput(inputId = "innUtForsink", label="Velg innleggelse/utsriving",
                                                   choices = c("Innleggelse"=1, "Utskriving"=2)),
                                       selectInput(inputId = "pstForsink", label="Vis antall eller andel",
                                                   choices = c("Andeler"=1, "Antall"=0)),
                                       dateRangeInput(inputId = 'datovalgForsink', start = startDato, end = idag, #'2020-05-10',
                                                      label = "Tidsperiode", separator="t.o.m.", language="nb"
                                       ),
                                       br(),
                                       h4('Hvis man av en eller annen grunn ønsker å oppdatere staging-data utenom de faste oppdateringstidene, trykk på knappen:'),
                                       actionButton("oppdatStaging", "Oppdater stagingdata")
                          ),
                          mainPanel(
                            br(),
                            h4('Div andeler...'),
                            # tableOutput('tabAndeler'),
                            # br(),
                            # h4('Div. sentralmål...'),
                            # tableOutput('tabSentralmaal'),
                            # br(),
                            # br(),
                            # h3('Registeringsforsinkelse, antall dager'),
                            # h4('Tabellen viser fordeling av registreringsforsinkelse per enhet.
                            #  Tallene i tabellen er fordeling over gitte antall dager, angitt i prosent'),
                            # uiOutput("tabRegForsinkEnhet"),
                            # downloadButton(outputId = 'lastNed_tabForsink', label='Last ned tabell') #, class = "butt")
                          )
                        )
               ), #tab artikkelarb

               # tabPanel(p('Utsendinger',
               #            title = 'Administrer utsending av rapporter'),
               #          value = 'Utsendinger',
               #
               #          shiny::sidebarLayout(
               #            shiny::sidebarPanel(
               #              autoReportOrgInput("beredUts"), #Definert slik at RHFnavn/HFnavn benyttes
               #              autoReportInput("beredUts")
               #            ),
               #            shiny::mainPanel(
               #              autoReportUI("beredUts")
               #            )
               #          )
               # ) #Tab utsending
             ) #tabset
    ) #hovedark Registeradm
  ) # navbarPage

}  # UI-del

  #' Server logic for the rapRegTemplate app
  #'
  #' @param input shiny input object
  #' @param output shiny output object
  #' @param session shiny session object
  #'
  #' @return A shiny app server object
  #' @export
  server_NIRbered <- function(input, output, session) {


  #-----------Div serveroppstart------------------
   rapbase::appLogger(session = session, msg = "Starter Corona-app")

  # widget
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', user$role(),
                                           ', bruker: ', user$name(),
                                           '<br> ReshID: ', user$org()) )

  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })




  #---------Hente data------------

  # Ønsker å fase ut bruk av aggregerte data

  CoroDataRaa <- NIRberedskDataSQL(kobleInt = 0)
  CoroDataRaa$HovedskjemaGUID <- toupper(CoroDataRaa$HovedskjemaGUID)
  CoroData <- NIRPreprosessBeredsk(RegData = CoroDataRaa, aggPers = 0) #NB: CoroData er nå opphold og IKKE personer!
  #CoroData <- NIRPreprosessBeredsk(RegData = CoroDataRaa, aggPers = 1, tellFlereForlop = 1)
  #BeredDataOpph <- NIRPreprosessBeredsk(RegData = CoroDataRaa, aggPers = 0)
  BeredIntRaa <- NIRberedskDataSQL(kobleInt = 1) #Bruke bare denne?
  #BeredIntPas <- NIRPreprosessBeredsk(RegData = BeredIntRaa, kobleInt = 1, aggPers = 1, tellFlereForlop = 1)
  InfluData <- NIRsqlPreInfluensa()
  InfluIntData <- NIRsqlPreInfluensa(kobleInt = 1)


  #-----Definere utvalgsinnhold og evt. parametre som er statiske i appen----------

  #Definere utvalgsinnhold
  rhfNavn <- c('Alle', as.character(sort(unique(CoroData$RHF))))
  hfNavn <- sort(unique(CoroData$HF)) #, index.return=T)
  navnUtsendingVerdi <- c(rhfNavn, hfNavn)
  navnUtsending <- c('Hele landet', paste0('RHF: ', rhfNavn[-1]), paste0('HF: ', hfNavn))

  enhetsNivaa <- c('Alle', 'RHF', 'HF')
  names(enhetsNivaa) <- c('Hele landet', 'eget RHF', 'egetHF')

  sesongSiste <- max(sort(unique(InfluData$Sesong)))
  sesongValg <- c('Alle', sort(unique(InfluData$Sesong)))
  output$velgSesong <- renderUI({
    selectInput(inputId = 'sesongInflu', label="Velg influensasesong",
                             choices = sesongValg, selected = sesongSiste)
  })


  RHFvalgInflu <- c('Alle', unique(as.character(InfluData$RHF)))
  names(RHFvalgInflu) <- RHFvalgInflu


  map_avdeling <- data.frame(
    UnitId = unique(CoroData$ReshId),
    orgname = CoroData$ShNavn[match(unique(CoroData$ReshId),
                                   CoroData$ReshId)])

  #user inneholder både reshID: user$org() og  rolle: user$role()
  # "name", "fullName", "phone", "email", "group", "unit", "org", "role", "orgName"
  user <- rapbase::navbarWidgetServer2(
    id = "navbar-widget",
    orgName = "intensivberedskap",
    map_orgname = shiny::req(map_avdeling),
    caller = "intensivberedskap"
  )



  # finnesEgenResh <- FALSE #renderText(user$org() %in% unique(CoroData$ReshId))
  # if (finnesEgenResh) {
  #   indReshEgen <- match(user$org(), CoroData$ReshId)
  #   egetShNavn <- as.character(CoroData$ShNavn[indReshEgen])
  #   egetRHF <- as.character(CoroData$RHF[indReshEgen])
  #   egetHF <- as.character(CoroData$HF[indReshEgen])
  # } else {
  #   egetRHF <- 'Ukjent'
  # }

  observeEvent(user$role(), {
    if ((user$role() != 'SC') ) { # & !(finnesEgenResh)
      shinyjs::hide(id = 'CoroRapp.pdf')
      shinyjs::hide(id = 'CoroRappTxt')
      hideTab(inputId = "hovedark", target = "Abonnement")
    } else {
      shinyjs::show(id = 'CoroRapp.pdf')
      shinyjs::show(id = 'CoroRappTxt')
    }


    # if (!(brukernavn %in% c('lenaro', 'kevin.thon',
    #                         'Reidar', 'eabu', 'eivh', 'mariawa-he', 'helkri'))) {
    if (user$role() == 'CC')  { #Velger ny rolle i stedet for spesifikke navn
      shinyjs::show(id = 'lastNed_dataBeredNIRraa')
      shinyjs::show(id = 'lastNed_dataBeredNIR')
      showTab(inputId = "hovedark", target = "Registeradmin.")
    } else {
      shinyjs::hide(id = 'lastNed_dataBeredNIRraa')
      shinyjs::hide(id = 'lastNed_dataBeredNIR')
      hideTab(inputId = "hovedark", target = "Registeradmin.")
    }
   })


  # if (user$role() != 'SC') {
  #   updateSelectInput(session, "valgtRHF",
  #                     choices = unique(c('Alle', ifelse(egetRHF=='Ukjent', 'Alle',
  #                                                       egetRHF))))
  #   }




  #-------- Laste ned Samlerapport------------
  observe({

    output$CoroRapp.pdf <- downloadHandler(
      filename = function(){
        paste0('CoronaRapport', Sys.time(), '.pdf')},
      content = function(file){
        henteSamlerapporterBered(file, rnwFil="BeredskapCorona.Rnw",
                                 enhetsNivaa = as.character(input$valgtNivaa),
                                 #valgtRHF = valgtRHF,
                                 reshID = user$org())
      }
    )
  })
  output$InfluRapp.pdf <- downloadHandler(
    filename = function(){
      paste0('InfluensaRapport', Sys.time(), '.pdf')},
    content = function(file){
      henteSamlerapporterBered(file, rnwFil="NIRinfluensa.Rnw")
    }
  )


  output$CoroRappTxt <- renderUI(tagList(
    h3(HTML('Coronarapport med samling av resultater')),
    h5(HTML('Coronarapporten kan man få regelmessig tilsendt på e-post.
                    Gå til fanen "Abonnement" for å bestille dette')))
  )


  #----------Tabeller----------------------------

  observeEvent(input$tilbakestillValg, shinyjs::reset("brukervalgStartside"))

  observe({
  #
  #   valgtRHF <- ifelse(user$role() == 'SC', as.character(input$valgtRHF), egetRHF)
  #
  #   AntTab <- TabTidEnhet(RegData=CoroData, tidsenhet='dag',
  #                         valgtRHF= valgtRHF,
  #                         skjemastatus=as.numeric(input$skjemastatus),
  #                         resp=as.numeric(input$resp),
  #                         bekr=as.numeric(input$bekr),
  #                         dodInt=as.numeric(input$dodInt),
  #                         erMann=as.numeric(input$erMann)
  #   )
  #
  #   UtData <- NIRUtvalgBeredsk(RegData=CoroData,
  #                              valgtRHF= ifelse(valgtRHF=='Ukjent','Alle',valgtRHF),
  #                              skjemastatus=as.numeric(input$skjemastatus),
  #                              resp=as.numeric(input$resp),
  #                              bekr=as.numeric(input$bekr),
  #                              dodInt=as.numeric(input$dodInt),
  #                              erMann=as.numeric(input$erMann)
  #   )
  #
  #   utvalg <- if (length(UtData$utvalgTxt)>0) {
  #     UtData$utvalgTxt
  #   } else {'Alle registrerte '}
  #   txt <- if(dim(UtData$RegData)[1]>2) {
  #     paste0('For innlagte f.o.m. 10.mars 2020, er gjennomsnittsalderen <b>', round(mean(UtData$RegData$Alder, na.rm = T)), '</b> år og ',
  #            round(100*mean(UtData$RegData$erMann, na.rm = T)), '% er menn. Antall døde: ',
  #            sum(UtData$RegData$DischargedIntensiveStatus==1))
  #   } else {''}
  #
  #   output$utvalgHoved <- renderUI({
  #     UtTekst <- tagList(
  #       h5(HTML(paste0(utvalg, '<br />'))),
  #       h4(HTML(paste0(txt, '<br />')))
  #
  #     )})
  #
  #   visNdager <- nrow(AntTab$Tab)
  #   output$tabTidEnhet <- renderTable({AntTab$Tab[(visNdager-10):visNdager,]}, rownames = T, digits=0, spacing="xs"
  #   )
  #
  #
  #
  #   #Tab status nå
    # statusNaaTab <- statusECMOrespTab(RegData=CoroData, valgtRHF=input$valgtRHF,
    #                                   erMann=as.numeric(input$erMann),
    #                                   bekr=as.numeric(input$bekr))
    # output$tabECMOrespirator <- renderTable({statusNaaTab$Tab}, rownames = T, digits=0, spacing="xs")
    #  output$utvalgNaa <- renderUI({h5(HTML(paste0(statusNaaTab$utvalgTxt, '<br />'))) })
  #
  #
  #
  #   #Tab risiko
  #   RisikoTab <- RisikofaktorerTab(RegData=CoroData, #tidsenhet='Totalt',
  #                                  valgtRHF= input$valgtRHF,
  #                                  skjemastatus=as.numeric(input$skjemastatus),
  #                                  bekr=as.numeric(input$bekr),
  #                                  resp=as.numeric(input$resp),
  #                                  dodInt=as.numeric(input$dodInt),
  #                                  datoFra = input$datovalgStart[1],
  #                                  datoTil = input$datovalgStart[2],
  #                                  erMann=as.numeric(input$erMann),
  #                                  minald=as.numeric(input$alder[1]),
  #                                  maxald=as.numeric(input$alder[2]))
  #
  #
  #   output$tabRisikofaktorer <- if (RisikoTab$Ntest>2){
  #     renderTable(RisikoTab$Tab, rownames = T, digits=0, spacing="xs") } else {
  #       renderText('Få registreringer (N<3)')}
  #   output$utvalgRisiko <- renderUI({h5(HTML(paste0(RisikoTab$utvalgTxt, '<br />'))) #tagList()
  #   })
  #
  #
   }) #observe

  #------------------ Abonnement ----------------------------------------------

  # alleResh <- unique(CoroDataRaa$UnitId)
  # Navn <- CoroDataRaa$HealthUnitShortName[match(alleResh, CoroDataRaa$UnitId)]
  # names(alleResh) <- Navn
  # orgsAbb <- as.list(alleResh)

  ## parametre til utsending
  orgs <- navnUtsendingVerdi #rhfNavn. sykehusValg har enhetsnavn med verdi resh
  names(orgs) <- navnUtsending
  orgs <- as.list(orgs)
  org <- rapbase::autoReportOrgServer("beredUts", orgs)

  paramNamesAbb <- shiny::reactive(c("reshID"))
  paramValuesAbb <- shiny::reactive(user$org())

  ## Abonnementsvalg
  reports <- list(
    # CovidRappAlle = list(
    #   synopsis = "Intensivpasienter med covid-19: Hele landet",
    #   fun = "abonnementBeredsk",
    #   paramNames = c('rnwFil', 'reshID'),
    #   paramValues = c('Alle_BeredskapCorona.Rnw', 'user$org()')
    # ),
    # CovidRappRHF = list(
    #   synopsis = "Intensivpasienter med covid-19: RHF",
    #   fun = "abonnementBeredsk",
    #   paramNames = c('rnwFil', 'reshID'),
    #   paramValues = c('RHF_BeredskapCorona.Rnw', 'user$org()')
    # ),
    # CovidRappHF = list(
    #   synopsis = "Intensivpasienter med covid-19: HF",
    #   fun = "abonnementBeredsk",
    #   paramNames = c('rnwFil', 'reshID'),
    #   paramValues = c('HF_BeredskapCorona.Rnw', 'user$org()')
    # ),
    InfluensaRapp = list(
      synopsis = "Influensarapport",
      fun = "abonnementBeredsk",
      paramNames = c('rnwFil'),
      paramValues = c('NIRinfluensa.Rnw')
    )
  )

  rapbase::autoReportServer(
    id = "beredAbb",
    registryName = "intensivberedskap",
    type = "subscription",
    paramNames = c('rnwFil'), # paramNamesAbb,
    paramValues = c('NIRinfluensa.Rnw'), #paramValuesAbb,
    reports = reports,
    orgs = orgs,
    user = user
  )


 #------------Utsending-----------------


  ## Rapportvalg for utsending
  reports <- list(
    # CovidRapp = list(
    #   synopsis = "Intensivpasienter med covid-19",
    #   fun = "abonnementBeredsk", #Lag egen funksjon for utsending
    #   paramNames = c('rnwFil', 'nivaaNavn'), #"valgtRHF"),
    #   paramValues = c('BeredskapCorona.Rnw', 'Alle' ) #'Alle')
    # ),
    InfluensaRapp = list(
      synopsis = "Rapporteket-NIR-beredskap:Influensarapport",
      fun = "abonnementBeredsk",
      paramNames = c('rnwFil'),
      paramValues = c('NIRinfluensa.Rnw')
    )
  )

#Foreløpig tilrettelegges bare for influsensa
  # set reactive parameters overriding those in the reports list
  paramNames <- shiny::reactive(c('rnwFil')) #"nivaaNavn"
  paramValues <- shiny::reactive(c('NIRinfluensa.Rnw')) #org$value()

  rapbase::autoReportServer(
    id = "beredUts",
    registryName = "intensivberedskap",
    type = "dispatchment",
    org = org$value,
    paramNames = paramNames, # paramNames,
    paramValues = paramValues, #paramValues
    reports = reports,
    orgs = orgs,
    eligible = (user$role() == "SC"),
    user = user  )



  #---------------Influensa-------------------------
  observeEvent(input$tilbakestillValgInflu, shinyjs::reset("brukervalgInfluensa"))

     output$TittelInflu <- renderUI(h2(paste0('Resultater fra influensaregistrering. Sesong: ',
                                           input$sesongInflu)))
     observe({
      valgtSes <- ifelse(is.null(input$sesongInflu), 'Alle', input$sesongInflu)
      indSes <- if (valgtSes == 'Alle') {1:dim(InfluData)[1]} else {which(InfluData$Sesong == valgtSes)}

    UtDataInflu <- InfluData[indSes,]
    UtDataInflu <- NIRUtvalgBeredsk(RegData=UtDataInflu,
                                    #valgtRHF= as.character(input$valgtRHFinflu),
                                    skjemastatus=as.numeric(input$skjemastatusInflu),
                                    bekr=as.numeric(input$bekrInflu),
                                    dodInt=as.numeric(input$dodIntInflu),
                                    erMann=as.numeric(input$erMannInflu)
    )

    utvalg <- if (length(UtDataInflu$utvalgTxt)>0) {
      UtDataInflu$utvalgTxt
    } else {'Alle registrerte '}

     output$UtvalgInflu <- renderUI({h4(HTML(paste0(UtDataInflu$utvalgTxt, '<br />'))) })


 #Tab status nå
     statusNaaTabInflu <- statusECMOrespTab(RegData=UtDataInflu$RegData, influ=1)

    output$tabECMOrespiratorInflu <- renderTable({statusNaaTabInflu$Tab}, rownames = T, digits=0, spacing="xs")
    output$utvalgNaaInflu <- renderUI({h5(HTML(paste0(statusNaaTabInflu$utvalgTxt, '<br />'))) })
  #
  #
  #   #Tab ferdigstilte
  #   TabFerdigInflu <- oppsumFerdigeRegTab(RegData=UtDataInflu$RegData)
  #
  #   output$tabFerdigeRegInflu <- if (TabFerdigInflu$Ntest > 2){
  #     renderTable({TabFerdigInflu$Tab}, rownames = T, digits=0, spacing="xs")} else {
  #       renderText('Få registreringer (N<3)')}
  #
  #   output$utvalgFerdigeRegInflu <- renderUI({h5(HTML(paste0(TabFerdigInflu$utvalgTxt, '<br />'))) })
  #   output$tittelFerdigeRegInflu <- renderUI(
  #     h3(paste0('Ferdigstilte forløp (', TabFerdigInflu$Ntest, ' forløp)')))
  #
  #
  # output$tabInfluUkeRHF <- renderTable({
  #   TabUkeRHFinflu <- InfluensaUkeRHF(
  #     RegData=UtDataInflu$RegData, alleUker=0,
  #     sesong=ifelse(is.null(input$sesongInflu), sesongSiste, input$sesongInflu))
  #   xtable::xtable(TabUkeRHFinflu)}, rownames = T, digits=0, spacing="xs"
  # )
  #
  #
 }) #observe

  # observe({
  #   #InfluIntData <- InfluIntData[which(InfluIntData$Sesong == input$sesongInflu),]
  #   ind <- if (input$sesongInflu == 'Alle') {1:dim(InfluData)[1]} else {
  #     which(InfluData$Sesong == input$sesongInflu)}
  #   InfluIntData <- InfluIntData[ind, ]
  #
  #
  # output$fordInflu <- renderPlot({
  #   NIRberedskFigAndeler(RegData=InfluIntData, preprosess = 0,
  #                        valgtVar=input$valgtVarInflu,
  #                        dodInt=as.numeric(input$dodIntInflu),
  #                        erMann=as.numeric(input$erMannInflu), session = session
  #   )
  # }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
  # )
  #
  #
  # output$LastNedFigFordInflu <- downloadHandler(
  #   filename = function(){
  #     paste0('FordelingsFigur_', valgtVar=input$valgtVarInflu, '.', input$bildeformatFord) #'_', Sys.time(),
  #   },
  #
  #   content = function(file){
  #     NIRberedskFigAndeler(RegData=InfluIntData, preprosess = 0,
  #                          valgtVar=input$valgtVarInflu,
  #                          dodInt=as.numeric(input$dodIntInflu),
  #                          erMann=as.numeric(input$erMannInflu), session = session,
  #                          outfile = file)
  #   }
  # )
  #
  # UtDataFordInflu <- NIRberedskFigAndeler(RegData=InfluIntData, preprosess = 0,
  #                                      valgtVar=input$valgtVarInflu,
  #                                      #bekr=as.numeric(input$bekrInflu),
  #                                      dodInt=as.numeric(input$dodIntInflu),
  #                                      erMann=as.numeric(input$erMannInflu),
  #                                      session = session
  #   )
  #   tab <- lagTabavFigFord(UtDataFraFig = UtDataFordInflu)
  #
  #   output$tittelFordInflu <- renderUI({
  #     tagList(
  #       h3(HTML(paste(UtDataFordInflu$tittel, sep='<br />'))),
  #       h5(HTML(paste0(UtDataFordInflu$utvalgTxt, '<br />')))
  #     )}) #, align='center'
  #   output$fordelingTabInflu <- function() {
  #     antKol <- ncol(tab)
  #     kableExtra::kable(tab, format = 'html'
  #                       , full_width=F
  #                       , digits = c(0,0,1,0,0,1)[1:antKol]
  #     ) %>%
  #       #add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
  #       kableExtra::column_spec(column = 1, width_min = '7em') %>%
  #       kableExtra::column_spec(column = 2:(ncol(tab)+1), width = '7em') %>%
  #       kableExtra::row_spec(0, bold = T)
  #   }
  #
  #   output$lastNed_tabFordInflu <- downloadHandler(
  #     filename = function(){
  #       paste0(input$valgtVarInflu, '_fordeling.csv')
  #     },
  #     content = function(file, filename){
  #       write.csv2(tab, file, row.names = T, na = '')
  #     })
  # }) #observe


  #-----------Registeradmin.------------
  # BeredIntPasBekr <- BeredIntPas[which(BeredIntPas$Bekreftet==1), ]# 2020-05-11),
  #
  # output$lastNed_dataBeredNIRraa <- downloadHandler(
  #   filename = function(){
  #     paste0('DataCovidIntensivRaa.', Sys.Date(), '.csv')
  #   },
  #   content = function(file, filename){
  #     write.csv2(BeredIntRaa, file, row.names = F, na = '')
  #   })
  #
  # output$lastNed_dataBeredNIR <- downloadHandler(
  #   filename = function(){
  #     paste0('BeredIntPas', Sys.Date(), '.csv')
  #   },
  #   content = function(file, filename){
  #     write.csv2(BeredIntPas, file, row.names = F, na = '')
  #   })
  # #})

}
