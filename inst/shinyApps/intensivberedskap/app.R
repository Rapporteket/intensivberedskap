#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(intensivberedskap)

addResourcePath('rap', system.file('www', package='rapbase'))
context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- context %in% c("DEV", "TEST", "QA", "PRODUCTION")

idag <- Sys.Date()
datoTil <- as.POSIXlt(idag)
startDato <- '2020-03-01'

regTitle <- ifelse(paaServer,
                   paste0('Norsk Intensiv- og pandemiregister, Beredskapsregistrering ',ifelse(context=='QA', 'QA','')),
                   'Norsk Intensivregister med FIKTIVE data')

#---------Hente data------------

CoroDataRaa <- NIRberedskDataSQL(kobleInt = 0)
CoroDataRaa$HovedskjemaGUID <- toupper(CoroDataRaa$HovedskjemaGUID)

#Bruk resh før preprosesserer
CoroData <- NIRPreprosessBeredsk(RegData = CoroDataRaa, aggPers = 1, tellFlereForlop = 1)
BeredDataOpph <- NIRPreprosessBeredsk(RegData = CoroDataRaa, aggPers = 0)

BeredIntRaa <- NIRberedskDataSQL(kobleInt = 1)

if (dim(BeredIntRaa)[1]>0) {
  BeredIntPas <- NIRPreprosessBeredsk(RegData = BeredIntRaa, kobleInt = 1, aggPers = 1, tellFlereForlop = 1)
}

#Influensadata# MÅ GJØRES I EI preprosess-fil eller i spørringa som henter data!!
queryInflu <- paste0('SELECT * FROM InfluensaFormDataContract')
InfluData <- NIRsqlPreInfluensa() #InfluDataRaa #intensiv::NIRPreprosess(RegData = InfluDataRaa, skjema = 3)
InfluIntData <- NIRsqlPreInfluensa(kobleInt = 1) #InfluDataRaa #intensiv::NIRPreprosess(RegData = InfluDataRaa, skjema = 3)


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

RHFvalgInflu <- c('Alle', unique(as.character(InfluData$RHF)))
names(RHFvalgInflu) <- RHFvalgInflu


source(system.file("shinyApps/intensivberedskap/R/koronafigurer_modul.R", package = "intensivberedskap"), encoding = 'UTF-8')

ui <- tagList(
  navbarPage(id='hovedark',
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
                                   br(),
                                   selectInput(inputId = "valgtNivaa", label="Velg enhetsnivå for rapporten",
                                               choices = enhetsNivaa
                                   ),
                                   br(),
                                   h4('Gjør filtreringer/utvalg i tabeller og figurer:'),

                                   selectInput(inputId = "valgtRHF", label="Velg RHF",
                                               choices = rhfNavn
                                   ),
                                   selectInput(inputId = "bekr", label="Bekreftet/Mistenkt",
                                               choices = c("Alle"=9, "Bekreftet"=1, "Mistenkt"=0)
                                   ),
                                   selectInput(inputId = "skjemastatus", label="Skjemastatus",
                                               choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
                                   ),
                                   selectInput(inputId = "resp", label="Respiratorbehandlet (invasiv+non-inv.)",
                                               choices = c("Alle"=9, "Ja"=1, "Nei"=2)
                                   ),
                                   selectInput(inputId = "dodInt", label="Tilstand ut fra intensiv",
                                               choices = c("Alle"=9, "Død"=1, "Levende"=0)
                                   ),
                                   selectInput(inputId = "erMann", label="Kjønn",
                                               choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
                                   ),
                                   dateRangeInput(inputId = 'datovalgStart', start = startDato, end = idag, #'2020-05-10',
                                                  label = "Tidsperiode", separator="t.o.m.", language="nb"
                                   ),
                                   h4('Kun for risikofaktorer:'),
                                   sliderInput(inputId="alder", label = "Alder",
                                               min = 0, max = 110,
                                               value = c(0, 110),
                                               step = 10
                                   ),
                                   br(),
                                   actionButton("tilbakestillValg", label="Tilbakestill valg"),
                                   br(),
                                   selectInput(inputId = "bildeformatAldKj",
                                               label = "Velg format for nedlasting av figur",
                                               choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))

                                   # dateRangeInput(inputId = 'datovalg', start = startDato, end = idag,
                                   #                label = "Tidsperiode", separator="t.o.m.", language="nb" #)
                                   # ),
                      ),
                      mainPanel(width = 9,
                                appNavbarUserWidget(user = uiOutput("appUserName"),
                                                    organization = uiOutput("appOrgName"),
                                                    addUserInfo = TRUE),
                                tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),

                                h3('Resultater fra intensivregisterets beredskapsskjema for mistenkt/bekreftet
                       Coronasmitte.'),
                                h4('Merk at resultatene er basert på til dels ikke-fullstendige registreringer'),
                                #h5('Siden er under utvikling... ', style = "color:red"),
                                br(),
                                fluidRow(
                                  column(width = 4,
                                         h4('Forløp uten registrert ut-tid fra intensiv'), #, align='center'),
                                         uiOutput('liggetidNaa'),
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
                                tableOutput('tabInneliggHF'),

                                h3('Antall ny-innlagte pasienter, siste 10 dager'),
                                h4('NB: Inkluderer ikke overføringer mellom intensivenheter'),
                                uiOutput('utvalgHoved'),
                                tableOutput('tabTidEnhet'),
                                br(),
                                fluidRow(
                                  column(width=3,
                                         h3('Risikofaktorer'),
                                         uiOutput('utvalgRisiko'),
                                         tableOutput('tabRisikofaktorer')),
                                  column(width=5, offset=1,
                                         h3('Aldersfordeling'),
                                         plotOutput("FigurAldersfordeling", height="auto"),
                                         downloadButton("LastNedFigAldKj", "Last ned figur"),
                                         downloadButton("lastNedAldKj", "Last ned tabell")

                                  ))
                      ) #main
             ), #tab Oversikt

             #------------Figurer-----------------------------------
             tabPanel("Antall intensivpasienter",
                      koronafigurer_UI(id = "koronafigurer_id", rhfNavn=rhfNavn)
             ),
             tabPanel(p("Fordelingsfigurer",
                        title='Fordelingsfigurer for variabler registrert under intensivoppholdet'),
                      value = 'Fordelingsfigurer',
                      sidebarPanel(
                        h4('Her kan man velge hvilken variabel man ønsker å se på og gjøre ulike filtreringer.'),
                        selectInput(
                          inputId = "valgtVar", label="Velg variabel",
                          selected = 'regForsinkelseInn',
                          choices = c('Alder' = 'alder',
                                      'Bukleie' = 'bukleie',
                                      'Hemodynamisk overvåkn.' = 'ExtendedHemodynamicMonitoring',
                                      'Frailty index' = 'frailtyIndex',
                                      'Liggetid' = 'liggetid',
                                      'Primærårsak' = 'PrimaryReasonAdmitted',
                                      'Registreringsforsinkelse, innleggelse' = 'regForsinkelseInn',
                                      'Registreringsforsinkelse, utskriving' = 'regForsinkelseUt',
                                      'Respiratortid, totalt' = 'RespiratortidInt',
                                      'Respiratortid, ikke-invasiv' = 'respiratortidNonInv',
                                      'Respiratortid, invasiv' = 'respiratortidInv',
                                      'SAPSII-skår (alvorlighet av sykd.)' = 'Saps2ScoreNumber'
                                      #,'Spesielle tiltak' = 'spesTiltak'
                          )
                        ),
                        selectInput(inputId = "bekrFord", label="Bekreftet/Mistenkt",
                                    choices = c("Bekreftet"=1, "Alle"=9, "Mistenkt"=0)
                        ),
                        dateRangeInput(inputId = 'datovalg', start = startDato, end = Sys.Date(),
                                       label = "Tidsperiode", separator="t.o.m.", language="nb"
                        ),
                        selectInput(inputId = "erMannFord", label="Kjønn",
                                    choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
                        ),
                        selectInput(inputId = "bildeformatFord",
                                    label = "Velg format for nedlasting av figur",
                                    choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')
                        )
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel(
                            'Figur',
                            h4('Data er aggregerte til pasientnivå og inneholder kun registreringer
                               hvor pasienten har både beredskapsskjema og ferdigstilte intensivskjema.'),
                            plotOutput('fordelinger', height="auto"),
                            downloadButton(outputId = "LastNedFigFord", label = "Last ned figur")),
                          tabPanel(
                            'Tabell',
                            uiOutput("tittelFord"),
                            tableOutput('fordelingTab'),
                            downloadButton(outputId = 'lastNed_tabFord', label='Last ned tabell') #, class = "butt")
                          )
                        )
                      )
             ), #Tab, fordelinger

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
                                   h4(paste0('Influensarapport med samling av resultater for sesongen ', sesongSiste)),
                                   h5('(Influensasesong oppdateres automatisk så snart det registreres data for en ny sesong)'),
                                   br(),
                                   downloadButton(outputId = 'InfluRapp.pdf', label='Last ned Influensarapport', class = "butt"),
                                   tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                                   h5('Influensarapporten kan man få regelmessig tilsendt på e-post.
                           Gå til fanen "Abonnement" for å bestille dette.'),
                                   br(),
                                   br(),
                                   h3('Gjør filtreringer/utvalg i tabellene til høyre:'),

                                   # selectInput(inputId = "valgtRHFinflu", label="Velg RHF",
                                   #             choices = RHFvalgInflu
                                   #             #,selected = 'Alle'
                                   # ),

                                   selectInput(inputId = "sesongInflu", label="Velg influensasesong",
                                               choices = sesongValg, selected = sesongSiste),
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
                        #h3('Siden er under utvikling... ', style = "color:red"),
                        uiOutput('TittelInflu'),
                        h4('Alle resultater er basert på opphold. (Det er ikke gjort noen aggregering på person.)'),
                        tabsetPanel(
                          id = 'influensa',
                          tabPanel(
                            'Oppsummeringer',
                            h3('Utvalg som er gjort:'),
                            uiOutput('UtvalgInflu'),
                            #h4('Merk at resultatene er basert på til dels ikke-fullstendige registreringer'),
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
             tabPanel(p("Abonnement",
                        title='Bestill automatisk utsending av rapporter på e-post'),
                      value = 'Abonnement',

                      sidebarLayout(
                        sidebarPanel(
                          #autoReportOrgInput("beredAbb"), #ReshId
                          autoReportInput("beredAbb")
                        ),
                        shiny::mainPanel(
                          autoReportUI("beredAbb")
                        )
                      )
             ), #tab abonnement

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
                                   ),
                                   mainPanel(
                                     h3('Bekreftede Covidpasienter, t.o.m. dagens dato'),
                                     br(),
                                     br(),
                                     h4('Div andeler...'),
                                     tableOutput('tabAndeler'),
                                     br(),
                                     h4('Div. sentralmål...'),
                                     tableOutput('tabSentralmaal'),
                                     br(),
                                     br(),
                                     h3('Registeringsforsinkelse, antall dager'),
                                     h4('Tabellen viser fordeling av registreringsforsinkelse per enhet.
                             Tallene i tabellen er fordeling over gitte antall dager, angitt i prosent'),
                                     uiOutput("tabRegForsinkEnhet"),
                                     downloadButton(outputId = 'lastNed_tabForsink', label='Last ned tabell') #, class = "butt")
                                   )
                                 )
                        ), #tab artikkelarb

                        tabPanel(p('Utsendinger',
                                   title = 'Administrer utsending av rapporter'),
                                 value = 'Utsendinger',

                                 shiny::sidebarLayout(
                                   shiny::sidebarPanel(
                                     autoReportOrgInput("beredUts"), #Definert slik at RHFnavn/HFnavn benyttes
                                     autoReportInput("beredUts")
                                   ),
                                   shiny::mainPanel(
                                     autoReportUI("beredUts")
                                   )
                                 )
                        ) #Tab utsending
                      ) #tabset
             ) #hovedark Registeradm
  ) # navbarPage
) # tagList



#----------Slutt ui-del--------------


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  #-----------Div serveroppstart------------------
  if (context %in% c('QA', 'PRODUCTION')){
    rapbase::appLogger(session = session, msg = "Starter Corona-app")}

  reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 42088921) # 42088921

  rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')
  brukernavn <- ifelse(paaServer, rapbase::getUserName(shinySession=session), 'brukernavn')

  finnesEgenResh <- reshID %in% unique(CoroData$ReshId)
  if (finnesEgenResh) {
    indReshEgen <- match(reshID, CoroData$ReshId)
    egetShNavn <- as.character(CoroData$ShNavn[indReshEgen])
    egetRHF <- as.character(CoroData$RHF[indReshEgen])
    egetHF <- as.character(CoroData$HF[indReshEgen])
  } else {
    egetRHF <- 'Ukjent'
  }
  #18.jan. 2022 egetRHF <- ifelse(rolle=='SC', 'Alle', egetRHF)

  observe({
    if ((rolle != 'SC') & !(finnesEgenResh)) { #
      shinyjs::hide(id = 'CoroRapp.pdf')
      shinyjs::hide(id = 'CoroRappTxt')
      hideTab(inputId = "hovedark", target = "Abonnement")
    }
    if (!(brukernavn %in% c('lenaro', 'kevin.thon',
                            'Reidar', 'eabu', 'eivh', 'mariawa-he', 'helkri'))) {
      shinyjs::hide(id = 'lastNed_dataBeredNIRraa')
      shinyjs::hide(id = 'lastNed_dataBeredNIR')
    }
    if (!(brukernavn %in% c('lenaro', 'Reidar', 'eabu', 'eivh', 'jlaake', 'mariawa-he', 'helkri'))) { #jlaake-ikke datafiler
      hideTab(inputId = "hovedark", target = "Registeradmin.")
    }
  })
  if (rolle != 'SC') {
    updateSelectInput(session, "valgtRHF",
                      choices = unique(c('Alle', ifelse(egetRHF=='Ukjent', 'Alle',
                                                        egetRHF))))
  }


  # widget
  if (paaServer) {
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', rolle,
                                           ', bruker: ', brukernavn,
                                           '<br> ReshID: ', reshID) )}

  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })



  #-------- Laste ned Samlerapport------------
  observe({
    #valgtRHF <- ifelse(rolle == 'LU', egetRHF, as.character(input$valgtRHF))
    #valgtNivaa <- ifelse(rolle == 'LU', 'HF', as.character(input$valgtNivaa))

    output$CoroRapp.pdf <- downloadHandler(
      filename = function(){
        paste0('CoronaRapport', Sys.time(), '.pdf')},
      content = function(file){
        henteSamlerapporterBered(file, rnwFil="BeredskapCorona.Rnw",
                                 enhetsNivaa = as.character(input$valgtNivaa),
                                 #valgtRHF = valgtRHF,
                                 reshID = reshID)
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

    valgtRHF <- ifelse(rolle == 'SC', as.character(input$valgtRHF), egetRHF)

    AntTab <- TabTidEnhet(RegData=CoroData, tidsenhet='dag',
                          valgtRHF= valgtRHF,
                          skjemastatus=as.numeric(input$skjemastatus),
                          resp=as.numeric(input$resp),
                          bekr=as.numeric(input$bekr),
                          dodInt=as.numeric(input$dodInt),
                          erMann=as.numeric(input$erMann)
    )

    UtData <- NIRUtvalgBeredsk(RegData=CoroData,
                               valgtRHF= ifelse(valgtRHF=='Ukjent','Alle',valgtRHF),
                               skjemastatus=as.numeric(input$skjemastatus),
                               resp=as.numeric(input$resp),
                               bekr=as.numeric(input$bekr),
                               dodInt=as.numeric(input$dodInt),
                               erMann=as.numeric(input$erMann)
    )

    utvalg <- if (length(UtData$utvalgTxt)>0) {
      UtData$utvalgTxt
    } else {'Alle registrerte '}
    txt <- if(dim(UtData$RegData)[1]>2) {
      paste0('For innlagte f.o.m. 10.mars 2020, er gjennomsnittsalderen <b>', round(mean(UtData$RegData$Alder, na.rm = T)), '</b> år og ',
             round(100*mean(UtData$RegData$erMann, na.rm = T)), '% er menn. Antall døde: ',
             sum(UtData$RegData$DischargedIntensiveStatus==1))
    } else {''}

    output$utvalgHoved <- renderUI({
      UtTekst <- tagList(
        h5(HTML(paste0(utvalg, '<br />'))),
        h4(HTML(paste0(txt, '<br />')))

      )})

    visNdager <- nrow(AntTab$Tab)
    output$tabTidEnhet <- renderTable({AntTab$Tab[(visNdager-10):visNdager,]}, rownames = T, digits=0, spacing="xs"
    )



    #Tab status nå
    statusNaaTab <- statusECMOrespTab(RegData=CoroData, valgtRHF=input$valgtRHF,
                                      erMann=as.numeric(input$erMann),
                                      bekr=as.numeric(input$bekr))
    output$tabECMOrespirator <- renderTable({statusNaaTab$Tab}, rownames = T, digits=0, spacing="xs")
    output$utvalgNaa <- renderUI({h5(HTML(paste0(statusNaaTab$utvalgTxt, '<br />'))) })


    # Inneliggende per HF
    output$tabInneliggHF <- renderTable({
      if (rolle == 'LU') {CoroData <- CoroData[which(CoroData$RHF == egetRHF), ]}
      inneligg <- is.na(CoroData$DateDischargedIntensive)
      RegHF <- CoroData[inneligg,] %>% dplyr::group_by(RHFut, HFut) %>% dplyr::summarise(Antall = n(), .groups = 'keep')
      colnames(RegHF) <- c('RHF', 'HF', 'Antall')
      RegHF
    }, rownames = F, digits = 0)



    #Tab ferdigstilte
    TabFerdig <- oppsumFerdigeRegTab(RegData=CoroData,
                                     valgtRHF=input$valgtRHF,
                                     datoFra = input$datovalgStart[1],
                                     datoTil = input$datovalgStart[2],
                                     bekr = as.numeric(input$bekr),
                                     resp=as.numeric(input$resp),
                                     dodInt=as.numeric(input$dodInt),
                                     erMann=as.numeric(input$erMann))

    output$tabFerdigeReg <- if (TabFerdig$Ntest > 2){
      renderTable({TabFerdig$Tab}, rownames = T, digits=0, spacing="xs")} else {
        renderText('Få registreringer (N<3)')}

    output$utvalgFerdigeReg <- renderUI({h5(HTML(paste0(TabFerdig$utvalgTxt, '<br />'))) })
    output$tittelFerdigeReg <- renderUI(
      h4(paste0('Ferdigstilte forløp (', TabFerdig$Ntest, ' forløp)')))

    #Registreringer i limbo:
    #Må ha egen funksjon for å få dette på sykehusnivå
    output$RegIlimbo <- renderUI({
      # AntIlibo <- AntTab$Ntest - (TabFerdig$Ntest + sum(is.na(CoroData$DateDischargedIntensive))) #RHF/alle
      finnBurdeFerdig <- function(RegData) {sum((!(is.na(RegData$DateDischargedIntensive)) & (RegData$FormStatus!=2)))}
      valgtRHF <- input$valgtRHF
      tittel <- 'Forløp registrert som utskrevet, uten ferdigstilt skjema: '

      AntBurdeFerdig <-
        c( #tittel,
          if (rolle=='LU' & finnesEgenResh) {
            paste0(finnBurdeFerdig(CoroData[(which(CoroData$ReshId==reshID)), ]),' skjema for ', egetShNavn)},
          if (valgtRHF=='Alle') {
            paste0(finnBurdeFerdig(CoroData), ' skjema for hele landet')
          } else {
            paste0(finnBurdeFerdig(CoroData[CoroData$RHF==valgtRHF, ]), ' skjema for ', valgtRHF)}
        )
      h5(HTML(paste0('&nbsp;&nbsp;&nbsp;', AntBurdeFerdig, '<br />')))
    })


    #Tab risiko
    RisikoTab <- RisikofaktorerTab(RegData=CoroData, #tidsenhet='Totalt',
                                   valgtRHF= input$valgtRHF,
                                   skjemastatus=as.numeric(input$skjemastatus),
                                   bekr=as.numeric(input$bekr),
                                   resp=as.numeric(input$resp),
                                   dodInt=as.numeric(input$dodInt),
                                   datoFra = input$datovalgStart[1],
                                   datoTil = input$datovalgStart[2],
                                   erMann=as.numeric(input$erMann),
                                   minald=as.numeric(input$alder[1]),
                                   maxald=as.numeric(input$alder[2]))


    output$tabRisikofaktorer <- if (RisikoTab$Ntest>2){
      renderTable(RisikoTab$Tab, rownames = T, digits=0, spacing="xs") } else {
        renderText('Få registreringer (N<3)')}
    output$utvalgRisiko <- renderUI({h5(HTML(paste0(RisikoTab$utvalgTxt, '<br />'))) #tagList()
    })

    TabAlder <- TabAlderGml(RegData=CoroData,
                            valgtRHF= input$valgtRHF,
                            dodInt=as.numeric(input$dodInt),
                            resp=as.numeric(input$resp),
                            erMann=as.numeric(input$erMann),
                            skjemastatus=as.numeric(input$skjemastatus)
    )
    output$utvalgAlder <- renderUI({h5(HTML(paste0(TabAlder$utvalgTxt, '<br />'))) })


  })

  #------------------Datakvalitet-----------------------
  #Ferdigstilte beredskapsskjema uten ferdigstilt intensivskjema:
  observe({

    ManglerIntSkjemaTab <- ManglerIntSkjema(reshID = ifelse(rolle == 'SC', 0, reshID)
                                            ,datoFra = input$datoFraMI)

    ManglerIntSkjemaTab$FormDate <- as.character(ManglerIntSkjemaTab$FormDate)

    output$tabManglerIntSkjema <- if (dim(ManglerIntSkjemaTab)[1]>0){

      renderTable(ManglerIntSkjemaTab, rownames = F, digits=0, spacing="xs") } else {
        renderText('Alle intensivskjema ferdigstilt')}

    output$lastNed_ManglerIntSkjema <- downloadHandler(
      filename = function(){
        paste0('ManglerIntSkjema.csv')
      },
      content = function(file, filename){
        write.csv2(ManglerIntSkjemaTab, file, row.names = F, na = '')
      })
  })

  #------------------ Abonnement ----------------------------------------------
  #--------Start modul, abonnement
  alleResh <- unique(CoroDataRaa$UnitId)
  Navn <- CoroDataRaa$HelseenhetKortnavn[match(alleResh, CoroDataRaa$UnitId)]
  names(alleResh) <- Navn
  orgsAbb <- as.list(alleResh)

  ## make a list for report metadata
  reports <- list(
    CovidRappAlle = list(
      synopsis = "Intensivpasienter med covid-19: Hele landet",
      fun = "abonnementBeredsk", #Lag egen funksjon for utsending
      paramNames = c('rnwFil', 'reshID'), #"valgtRHF"),
      paramValues = c('Alle_BeredskapCorona.Rnw', reshID) #'Alle')
    ),
    CovidRappRHF = list(
      synopsis = "Intensivpasienter med covid-19: RHF",
      fun = "abonnementBeredsk", #Lag egen funksjon for utsending
      paramNames = c('rnwFil', 'reshID'), #"valgtRHF"),
      paramValues = c('RHF_BeredskapCorona.Rnw', reshID) #'Alle')
    ),
    CovidRappHF = list(
      synopsis = "Intensivpasienter med covid-19: HF",
      fun = "abonnementBeredsk", #Lag egen funksjon for utsending
      paramNames = c('rnwFil', 'reshID'), #"valgtRHF"),
      paramValues = c('HF_BeredskapCorona.Rnw', reshID) #'Alle')
    ),
    InfluensaRapp = list(
      synopsis = "Influensarapport",
      fun = "abonnementBeredsk",
      paramNames = c('rnwFil'),
      paramValues = c('NIRinfluensa.Rnw')
    )
  )

  autoReportServer(
    id = "beredAbb", registryName = "intensivberedskap", type = "subscription",
    org = orgAbb$value, paramNames = paramNames, paramValues = paramValues, #Ta bort org = orgAbb$value?
    reports = reports, orgs = orgsAbb, eligible = TRUE
  )

  #------------Utsending-----------------

  ## parametre til utsending
  orgs <- navnUtsendingVerdi #rhfNavn. sykehusValg har enhetsnavn med verdi resh
  names(orgs) <- navnUtsending
  orgs <- as.list(orgs)

  ## make a list for report metadata
  reports <- list(
    CovidRapp = list(
      synopsis = "Intensivpasienter med covid-19",
      fun = "abonnementBeredsk", #Lag egen funksjon for utsending
      paramNames = c('rnwFil', 'nivaaNavn'), #"valgtRHF"),
      paramValues = c('BeredskapCorona.Rnw', 'Alle' ) #'Alle')
    ),
    InfluensaRapp = list(
      synopsis = "Rapporteket-NIR-beredskap:Influensarapport",
      fun = "abonnementBeredsk",
      paramNames = c('rnwFil'),
      paramValues = c('NIRinfluensa.Rnw')
    )
  )

  org <- autoReportOrgServer("beredUts", orgs)

  # set reactive parameters overriding those in the reports list
  paramNames <- shiny::reactive("nivaaNavn")
  paramValues <- shiny::reactive(org$value())


  autoReportServer(
    id = "beredUts", registryName = "intensivberedskap", type = "dispatchment",
    org = org$value, paramNames = paramNames, paramValues = paramValues,
    reports = reports, orgs = orgs, eligible = TRUE
  )

  #---------------- Alders- og kjønnsfordeling #####################
  output$FigurAldersfordeling <- renderPlot({
    valgtRHF <- ifelse(rolle == 'SC', as.character(input$valgtRHF), egetRHF)
    intensivberedskap::FigFordelingKjonnsdelt(RegData = CoroData, valgtVar = 'Alder', resp=as.numeric(input$resp),
                                              valgtRHF= valgtRHF, dodInt=as.numeric(input$dodInt),
                                              skjemastatus=as.numeric(input$skjemastatus),
                                              bekr=as.numeric(input$bekr))
  }, width = 500, height = 500)

  output$LastNedFigAldKj <- downloadHandler(
    filename = function(){
      paste0('AldKjFig', Sys.time(), '.', input$bildeformatAldKj)
    },

    content = function(file){
      intensivberedskap::FigFordelingKjonnsdelt(RegData = CoroData, valgtVar = 'Alder', dodInt=as.numeric(input$dodInt),
                                                valgtRHF= ifelse(rolle == 'SC', as.character(input$valgtRHF), egetRHF),
                                                skjemastatus=as.numeric(input$skjemastatus), resp=as.numeric(input$resp),
                                                bekr=as.numeric(input$bekr), outfile = file)
    }
  )


  output$tabAlder <- function() {
    valgtRHF <- ifelse(rolle == 'SC', as.character(input$valgtRHF), egetRHF)
    Tabell <- intensivberedskap::FigFordelingKjonnsdelt(RegData = CoroData, valgtVar = 'Alder', resp=as.numeric(input$resp),
                                                        valgtRHF= valgtRHF, dodInt=as.numeric(input$dodInt),
                                                        skjemastatus=as.numeric(input$skjemastatus),
                                                        bekr=as.numeric(input$bekr))
    Tabell %>% knitr::kable("html", digits = 0) %>%
      kable_styling("hover", full_width = F) %>%
      add_header_above(c("Kategori", "Antall" = (dim(Tabell)[2]-3), "Totalt" = 2))
  }


  output$lastNedAldKj <- downloadHandler(
    filename = function(){
      paste0('AldKjTabell', Sys.time(), '.csv')
    },

    content = function(file){
      Tabell <- intensivberedskap::FigFordelingKjonnsdelt(RegData = CoroData, valgtVar = 'Alder', resp=as.numeric(input$resp),
                                                          valgtRHF= valgtRHF <- ifelse(rolle == 'SC', as.character(input$valgtRHF), egetRHF),
                                                          skjemastatus=as.numeric(input$skjemastatus), dodInt=as.numeric(input$dodInt),
                                                          bekr=as.numeric(input$bekr))
      write.csv2(Tabell, file, row.names = F, fileEncoding = 'latin1')
    }
  )


  #----------Figurer, modul og fordelinger #################################

  callModule(koronafigurer, "koronafigurer_id", rolle = rolle, CoroData = CoroData, egetRHF = egetRHF, reshID=reshID)


  output$fordelinger <- renderPlot({
    NIRberedskFigAndeler(RegData=BeredIntPas, preprosess = 0,
                         valgtVar=input$valgtVar,
                         # reshID=reshID,
                         # enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                         bekr=as.numeric(input$bekrFord),
                         datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                         # minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                         erMann=as.numeric(input$erMannFord), session = session
    )
  }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
  )


  output$LastNedFigFord <- downloadHandler(
    filename = function(){
      paste0('FordelingsFigur_', valgtVar=input$valgtVar, '.', input$bildeformatFord) #'_', Sys.time(),
    },

    content = function(file){
      NIRberedskFigAndeler(RegData=BeredIntPas, preprosess = 0,
                           valgtVar=input$valgtVar,
                           bekr=as.numeric(input$bekrFord),
                           datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                           erMann=as.numeric(input$erMannFord), session = session,
                           outfile = file)
    }
  )

  observe({
    UtDataFord <- NIRberedskFigAndeler(RegData=BeredIntPas, preprosess = 0,
                                       valgtVar=input$valgtVar,
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                       erMann=as.numeric(input$erMannFord),
                                       session = session
    )
    tab <- lagTabavFigFord(UtDataFraFig = UtDataFord)

    output$tittelFord <- renderUI({
      tagList(
        h3(HTML(paste(UtDataFord$tittel, sep='<br />'))),
        h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
      )}) #, align='center'
    output$fordelingTab <- function() {
      antKol <- ncol(tab)
      kableExtra::kable(tab, format = 'html'
                        , full_width=F
                        , digits = c(0,0,1,0,0,1)[1:antKol]
      ) %>%
        #add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
        kableExtra::column_spec(column = 1, width_min = '7em') %>%
        kableExtra::column_spec(column = 2:(ncol(tab)+1), width = '7em') %>%
        kableExtra::row_spec(0, bold = T)
    }

    output$lastNed_tabFord <- downloadHandler(
      filename = function(){
        paste0(input$valgtVar, '_fordeling.csv')
      },
      content = function(file, filename){
        write.csv2(tab, file, row.names = T, na = '')
      })
  }) #observe

  #---------------Influensa-------------------------
  observeEvent(input$tilbakestillValgInflu, shinyjs::reset("brukervalgInfluensa"))

  observe({
    output$TittelInflu <- renderUI(h2(paste0('Resultater fra influensaregistrering. Sesong: ',
                                          input$sesongInflu)))
    ind <- if (input$sesongInflu == 'Alle') {1:dim(InfluData)[1]} else {
      which(InfluData$Sesong == input$sesongInflu)}

    UtDataInflu <- InfluData[ind,]
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
    #output$utvalgNaaInflu <- renderUI({h5(HTML(paste0(statusNaaTabInflu$utvalgTxt, '<br />'))) })


    #Tab ferdigstilte
    TabFerdigInflu <- oppsumFerdigeRegTab(RegData=UtDataInflu$RegData)

    output$tabFerdigeRegInflu <- if (TabFerdigInflu$Ntest > 2){
      renderTable({TabFerdigInflu$Tab}, rownames = T, digits=0, spacing="xs")} else {
        renderText('Få registreringer (N<3)')}

    output$utvalgFerdigeRegInflu <- renderUI({h5(HTML(paste0(TabFerdigInflu$utvalgTxt, '<br />'))) })
    output$tittelFerdigeRegInflu <- renderUI(
      h3(paste0('Ferdigstilte forløp (', TabFerdigInflu$Ntest, ' forløp)')))


  output$tabInfluUkeRHF <- renderTable({
    TabUkeRHFinflu <- InfluensaUkeRHF(RegData=UtDataInflu$RegData, alleUker=0,
                                      # InfluData,
                                      # bekr=as.numeric(input$bekrInflu),
                                      # skjemastatus=as.numeric(input$skjemastatusInflu),
                                      # dodInt = as.numeric(input$dodIntInflu),
                                      # erMann = as.numeric(input$erMannInflu),
                                       sesong=input$sesongInflu)
    xtable::xtable(TabUkeRHFinflu)}, rownames = T, digits=0, spacing="xs"
  )


  })

  observe({
    #InfluIntData <- InfluIntData[which(InfluIntData$Sesong == input$sesongInflu),]
    ind <- if (input$sesongInflu == 'Alle') {1:dim(InfluData)[1]} else {
      which(InfluData$Sesong == input$sesongInflu)}
    InfluIntData <- InfluIntData[ind, ]


  output$fordInflu <- renderPlot({
    NIRberedskFigAndeler(RegData=InfluIntData, preprosess = 0,
                         valgtVar=input$valgtVarInflu,
                         dodInt=as.numeric(input$dodIntInflu),
                         erMann=as.numeric(input$erMannInflu), session = session
    )
  }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
  )


  output$LastNedFigFordInflu <- downloadHandler(
    filename = function(){
      paste0('FordelingsFigur_', valgtVar=input$valgtVarInflu, '.', input$bildeformatFord) #'_', Sys.time(),
    },

    content = function(file){
      NIRberedskFigAndeler(RegData=InfluIntData, preprosess = 0,
                           valgtVar=input$valgtVarInflu,
                           dodInt=as.numeric(input$dodIntInflu),
                           erMann=as.numeric(input$erMannInflu), session = session,
                           outfile = file)
    }
  )

  # output$LastNedFigFord <- downloadHandler(
  #   filename = function(){
  #     paste0('FordelingsFigur_', valgtVar=input$valgtVar, '.', input$bildeformatFord) #'_', Sys.time(),
  #   },
  #
  #   content = function(file){
  #     NIRberedskFigAndeler(RegData=BeredIntPas, preprosess = 0,
  #                          valgtVar=input$valgtVar,
  #                          bekr=as.numeric(input$bekrFord),
  #                          datoFra=input$datovalg[1], datoTil=input$datovalg[2],
  #                          erMann=as.numeric(input$erMannFord), session = session,
  #                          outfile = file)
  #   }
  # )
  #
  UtDataFordInflu <- NIRberedskFigAndeler(RegData=InfluIntData, preprosess = 0,
                                       valgtVar=input$valgtVarInflu,
                                       #bekr=as.numeric(input$bekrInflu),
                                       dodInt=as.numeric(input$dodIntInflu),
                                       erMann=as.numeric(input$erMannInflu),
                                       session = session
    )
    tab <- lagTabavFigFord(UtDataFraFig = UtDataFordInflu)

    output$tittelFordInflu <- renderUI({
      tagList(
        h3(HTML(paste(UtDataFordInflu$tittel, sep='<br />'))),
        h5(HTML(paste0(UtDataFordInflu$utvalgTxt, '<br />')))
      )}) #, align='center'
    output$fordelingTabInflu <- function() {
      antKol <- ncol(tab)
      kableExtra::kable(tab, format = 'html'
                        , full_width=F
                        , digits = c(0,0,1,0,0,1)[1:antKol]
      ) %>%
        #add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
        kableExtra::column_spec(column = 1, width_min = '7em') %>%
        kableExtra::column_spec(column = 2:(ncol(tab)+1), width = '7em') %>%
        kableExtra::row_spec(0, bold = T)
    }

    output$lastNed_tabFordInflu <- downloadHandler(
      filename = function(){
        paste0(input$valgtVarInflu, '_fordeling.csv')
      },
      content = function(file, filename){
        write.csv2(tab, file, row.names = T, na = '')
      })
  }) #observe




  #Abonnement, filer til FHI kopi fra korona
  observeEvent(input$bestillDataTilFHI, { #MÅ HA
    owner <- rapbase::getUserName(session)
    organization <- rapbase::getUserReshId(session)
    email <- rapbase::getUserEmail(session)
    interval <- "DSTday"
    intervalName <- "Daglig"
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(interval = interval)
    paramNames = c('zipFilNavn', 'brukernavn')
    paramValues = c(input$hvilkeFilerTilFHI, brukernavn)
    rapbase::createAutoReport(synopsis = paste0('Sendt til FHI: ',input$hvilkeFilerTilFHI),
                              package = 'intensivberedskap',
                              fun = "InfluensadataTilFHI",
                              paramNames = paramNames,
                              paramValues = paramValues,
                              owner = owner,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear,
                              interval = interval,
                              intervalName = intervalName)

    subscription$tab <-
      rapbase::makeAutoReportTab(session, type = "subscription")

  })



  #-----------Registeradmin.------------
  BeredIntPasBekr <- BeredIntPas[which(BeredIntPas$Bekreftet==1), ]# 2020-05-11),

  #Samme pasienter i råfil:
  # BeredIntRaaArt <- BeredIntRaa[
  #   which(sort(BeredIntRaa$PatientInRegistryGuid) %in% sort(BeredIntPasBekr$PasientID)), ]

  output$lastNed_dataBeredNIRraa <- downloadHandler(
    filename = function(){
      paste0('DataCovidIntensivRaa.', Sys.Date(), '.csv')
    },
    content = function(file, filename){
      write.csv2(BeredIntRaa, file, row.names = F, na = '')
    })

  output$lastNed_dataBeredNIR <- downloadHandler(
    filename = function(){
      paste0('BeredIntPas', Sys.Date(), '.csv')
    },
    content = function(file, filename){
      write.csv2(BeredIntPas, file, row.names = F, na = '')
    })
  #})

  observe({

    AndelerTab <- AndelerTab(RegData=BeredIntPasBekr,
                             #datoFra = input$datoValgArt[1], datoTil = input$datoValgArt[2],
                             erMann=as.numeric(input$erMannArt), valgtRHF='Alle') #,bekr=9, dodInt=9, resp=9, minald=0, maxald=110)

    output$tabAndeler <- renderTable(AndelerTab$Tab, rownames = T, digits=0, spacing="xs")

    SentralmaalTab <- SentralmaalTab(RegData=BeredIntPasBekr,
                                     #datoFra = input$datoValgArt[1], datoTil = input$datoValgArt[2],
                                     erMann=as.numeric(input$erMannArt), valgtRHF='Alle')
    output$tabSentralmaal <- renderTable(SentralmaalTab$Tab, rownames = T, digits=1, spacing="xs") #

    TabRegForsinkelse <- tabRegForsinkelse(RegData=BeredDataOpph,
                                           datoFra = input$datovalgForsink[1],
                                           datoTil = input$datovalgForsink[2],
                                           pst = input$pstForsink,
                                           innUt = input$innUtForsink)
    output$tabRegForsinkEnhet <- renderTable(TabRegForsinkelse, rownames = T, digits=0, spacing="xs")

    output$lastNed_tabForsink <- downloadHandler(
      filename = function(){
        paste0('RegForsinkelse.csv')
      },
      content = function(file, filename){
        write.csv2(TabRegForsinkelse, file, row.names = T, na = '')
      })

  })


}
# Run the application
shinyApp(ui = ui, server = server)
