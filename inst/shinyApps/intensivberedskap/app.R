#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
library(rapbase)
library(intensiv)
library(intensivberedskap)

addResourcePath('rap', system.file('www', package='rapbase'))
context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- context %in% c("DEV", "TEST", "QA", "PRODUCTION")

#options(knitr.table.format = "html")
idag <- Sys.Date() #as.Date('2018-11-30') #
datoTil <- as.POSIXlt(idag)
startDato <- '2020-03-01'  #paste0(as.numeric(format(idag-90, "%Y")), '-01-01')
#AarNaa <- as.numeric(format(idag, "%Y"))

regTitle <- ifelse(paaServer,
                   'Norsk Intensivregister, Beredskapsregistrering',
                   'Norsk Intensivregister med FIKTIVE data')

#---------Hente data------------

if (paaServer) {
  #CoroData <- NIRRegDataSQL(datoFra='2011-01-01', skjema=4) #, session = session) #datoFra = datoFra, datoTil = datoTil)
  qCoro <- 'SELECT *  from ReadinessFormDataContract'
  CoroData <- rapbase::LoadRegData(registryName= "nir", query=qCoro, dbType="mysql")
  #repLogger(session = session, 'Hentet alle data fra intensivregisteret')
# } else {
#   CoroData <- read.table('C:/ResultattjenesteGIT/XX.csv', sep=';',
#                          stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
} #hente data


CoroData <- NIRPreprosessBeredsk(RegData = CoroData)
#CoroData <- preprosessBeredVar(RegData = CoroData)


#-----Definere utvalgsinnhold og evt. parametre som er statiske i appen----------


#Definere utvalgsinnhold
#sykehusNavn <- sort(c('',unique(CoroData$ShNavn)), index.return=T)
#sykehusValg <- c(0,unique(CoroData$ReshId))[sykehusNavn$ix]
rhfNavn <- c('Alle', as.character(sort(unique(CoroData$RHF)))) #, index.return=T)
hfNavn <- sort(unique(CoroData$HF)) #, index.return=T)
sykehusNavn <- sort(unique(CoroData$ShNavn), index.return=T)
sykehusValg <- unique(CoroData$ReshId)[sykehusNavn$ix]
sykehusValg <- c(0,sykehusValg)
names(sykehusValg) <- c('Ikke valgt',sykehusNavn$x)

enhetsNivaa <- c('RHF', 'HF', 'ShNavn')
names(enhetsNivaa) <- c('RHF', 'HF', 'Sykehus')

ui <- tagList(
  navbarPage(
    title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",

    tabPanel("Oversikt",
             sidebarPanel(
                 width = 3,
                 h3('Coronarapport med samling av resultater'),
                 h5('Coronarapporten kan man få regelmessig tilsendt på e-post.
                    Gå til fanen "Abonnement" for å bestille dette.'),
                 downloadButton(outputId = 'CoroRapp.pdf', label='Last ned Coronarapport', class = "butt"),
                 tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                 br(),
              br(),
              h3('Gjør filtreringer/utvalg:'),
              #br(),
              h4('Alle tabeller OG samledokument:'),
              selectInput(inputId = "valgtRHF", label="Velg RHF",
                          choices = rhfNavn
              ),
              # h4('Utvalget gjelder tabellen med antall tifeller. Det anbefales å først filtrere på RHF'),
              # selectInput(inputId = "velgEnhetsnivaa", label="Velg enhetsnivaa",
              #             choices = enhetsNivaa
              # ),
              br(),
              h4('Tabellene for intensivopphold, aldersfordeling og risikofaktorer:'),
              selectInput(inputId = "skjemastatus", label="Skjemastatus",
                          choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
              ),
              selectInput(inputId = "bekrMist", label="Bekreftet/Mistenkt",
                          choices = c("Alle"=9, "Bekreftet"=1, "Mistenkt"=0)
              ),
              selectInput(inputId = "dodInt", label="Tilstand ut fra intensiv",
                          choices = c("Alle"=9, "Død"=1, "Levende"=0)
              ),
              selectInput(inputId = "erMann", label="Kjønn",
                          choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
              )

              # selectInput(inputId = "velgRHF", label="RHF",
              #             choices = c("Alle"=9, ...)
              # ),
              # selectInput(inputId = 'enhetsGruppe', label='Enhetgruppe',
              #             choices = c("RHF"=1, "HF"=2, "Sykehus"=3)
              # ),
              # dateRangeInput(inputId = 'datovalg', start = startDato, end = idag,
              #                label = "Tidsperiode", separator="t.o.m.", language="nb" #)
              # ),
              # sliderInput(inputId="alder", label = "Alder", min = 0,
              #             max = 110, value = c(0, 110)
              # ),
              # selectInput(inputId = 'velgResh', label='Velg eget Sykehus',
              #             #selected = 0,
              #             choices = sykehusValg),
              # selectInput(inputId = 'enhetsNivaa', label='Enhetsnivå',
              #             choices = c("Egen enhet"=2, "Hele landet"=0,
              #                         "Egen sykehustype"=4, "Egen region"=7)
              # ),
              #actionButton("reset_fordValg", label="Tilbakestill valg")
              ),
             mainPanel(width = 9,
                       shinyalert::useShinyalert(),
                       appNavbarUserWidget(user = uiOutput("appUserName"),
                                           organization = uiOutput("appOrgName"),
                                           addUserInfo = TRUE),
                       tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),

                       h3('Resultater fra intensivregisterets beredskapsskjema for mistenkt/bekreftet
                       Coronasmitte.'),
                       h4('Merk at resultatene er basert på til dels ikke-fullstendige registreringer'),
                      h3('Siden er under utvikling... ', style = "color:red"),
                      br(),

                      fluidRow(

                        h3('Kort oversikt'),
                        column(width=3,
                               h5('Pågående aktivitet', align='center'),
                               tableOutput('tabECMOrespirator')),
                        column(width=5, offset=1,
                               h5('Ferdigstilte registreringer', align='center'),
                               tableOutput('tabOppsumLiggetider')
                        )
                      ),
                      br(),

                      h3('Antall intensivopphold'),
                      h5('Her bør det vel komme en tekst om hvilke utvalg som er gjort'),
                       tableOutput('tabTidEnhet'),
                        br(),
                      fluidRow(
                        column(width=3,
                               h3('Risikofaktorer'),
                               h5('Tabellen oppsummerer alle opphold.'),
                               tableOutput('tabRisikofaktorer')),
                        column(width=5, offset=1,
                               h3('Aldersfordeling'),
                               h5('Her bør det vel komme en tekst om hvilke utvalg som er gjort'),
                               tableOutput('tabAlder'))
                      ),
             ) #main
    ), #tab Tabeller

#-----------Abonnement--------------------------------
    tabPanel(p("Abonnement",
             title='Bestill automatisk utsending av rapporter på e-post'),
             sidebarLayout(
               sidebarPanel(width = 3,
                 selectInput("subscriptionRep", "Dokument:", c("Koronarapport")),
                 selectInput("subscriptionFreq", "Frekvens:",
                             list(Månedlig="Månedlig-month",
                                   Ukentlig="Ukentlig-week",
                                   Daglig="Daglig-DSTday"),
                             selected = "Ukentlig-week"),
                 actionButton("subscribe", "Bestill!")
               ),
                mainPanel(
                  #h3('Mulighet for å abonnere på rapport kommer...', style = "color:red"),
               h4('NB: Abonnementet løper til det sies opp. '),
               uiOutput("subscriptionContent")
                )
             )
    ) #tab abonnement


  ) # navbarPage
) # tagList
#----------Slutt ui-del--------------


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Last inn data
  # regData <- getFakeRegData()

  #-----------Div serveroppstart------------------
 # raplog::appLogger(session = session, msg = "Starter Corona-app")

  reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 0)
  rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')
  brukernavn <- ifelse(paaServer, rapbase::getUserName(shinySession=session), 'brukernavn')

  if (reshID %in% unique(CoroData$ReshId)) {
    indReshEgen <- match(reshID, CoroData$ReshId)
    egetShNavn <- as.character(CoroData$ShNavn[indReshEgen])
    egetRHF <- as.character(CoroData$RHF[indReshEgen])
    egetHF <- as.character(CoroData$HF[indReshEgen])
    egenLokalitet <- c(0, 2, 4, 7)
    names(egenLokalitet) <- c('hele landet', egetShNavn, egetRHF)
  }



  # widget
  if (paaServer) {
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', rolle,
                                           '<br> ReshID: ', reshID) )}
                                           #,'<br> Org: ', egenOrg) )}

  # # User info in widget
  # userInfo <- rapbase::howWeDealWithPersonalData(session)
  # observeEvent(input$userInfo, {
  #   shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
  #                          type = "", imageUrl = "rap/logo.svg",
  #                          closeOnEsc = TRUE, closeOnClickOutside = TRUE,
  #                          html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  # })
  #

  # Figur og tabell
  ## Figur
  #output$distPlot <- renderPlot({
  #  makeHist(df = regData, var = input$var, bins = input$bins)
  #})

  ## Tabell
  #output$distTable <- renderTable({
  #  makeHist(df = regData, var = input$var, bins = input$bins, makeTable = TRUE)
  #})


  #-------- Laste ned Samlerapport------------

  output$CoroRapp.pdf <- downloadHandler(
    filename = function(){
      paste0('CoronaRapport', Sys.time(), '.pdf')},
    content = function(file){
      henteSamlerapporterBered(file, rnwFil="BeredskapCorona.Rnw",
                               valgtRHF = as.character(input$valgtRHF),
                          reshID = reshID) #Vurder å ta med tidsinndeling eller startdato
    }
  )
  # output$samleDok.pdf <- downloadHandler(
  #   filename = function(){ downloadFilename('Samledokument')},
  #   content = function(file){
  #     henteSamlerapporter(file, rnwFil="NGERSamleRapp.Rnw",
  #                         reshID = reshID,
  #                         datoFra = input$datovalgSamleDok[1],
  #                         datoTil = input$datovalgSamleDok[2])
  #   }
  # )

  #----------Tabeller----------------------------

#skjemastatus, bekrMist, dodInt

    output$tabTidEnhet <- renderTable({
    TabTidEnhet(RegData=CoroData, tidsenhet='dag', #enhetsNivaa='RHF',
                valgtRHF= input$valgtRHF,
                skjemastatus=as.numeric(input$skjemastatus),
                bekr=as.numeric(input$bekrMist),
                dodInt=as.numeric(input$dodInt),
                erMann=as.numeric(input$erMann)
    )}, rownames = T, digits=0, spacing="xs"
  )

  output$tabECMOrespirator <- renderTable({
    statusECMOrespTab(RegData=CoroData, valgtRHF=input$valgtRHF) #, datoTil=Sys.Date(), reshID=0)
  }, rownames = T, digits=0, spacing="xs"
  )

  output$tabOppsumLiggetider<- renderTable({
    oppsumLiggetiderTab(RegData=CoroData, valgtRHF=input$valgtRHF) #, datoTil=Sys.Date(), reshID=0)
  }, rownames = T, digits=0, spacing="xs"
  )


    output$tabRisikofaktorer <- renderTable({
      RisikofaktorerTab(RegData=CoroData, tidsenhet='Totalt',
                        valgtRHF= input$valgtRHF,
                        skjemastatus=as.numeric(input$skjemastatus),
                        bekr=as.numeric(input$bekrMist),
                        dodInt=as.numeric(input$dodInt),
                        erMann=as.numeric(input$erMann)
      ) #, datoTil=Sys.Date(), reshID=0)
    }, rownames = T, digits=0, spacing="xs"
    )

    output$tabAlder<- renderTable({
      xtable::xtable(TabAlder(RegData=CoroData, valgtRHF=input$valgtRHF,
                              bekr=as.numeric(input$bekrMist))) #, datoTil=Sys.Date(), reshID=0)
    }, rownames = T, digits=0, spacing="xs"
    )




  #------------- Abonnement----------------
    #------------------ Abonnement ----------------------------------------------
    ## reaktive verdier for å holde rede på endringer som skjer mens
    ## applikasjonen kjører
    rv <- reactiveValues(
      subscriptionTab = rapbase::makeUserSubscriptionTab(session))
    ## lag tabell over gjeldende status for abonnement
    output$activeSubscriptions <- DT::renderDataTable(
      rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
      rownames = FALSE, options = list(dom = 't')
    )

    ## lag side som viser status for abonnement, også når det ikke finnes noen
    output$subscriptionContent <- renderUI({
      fullName <- rapbase::getUserFullName(session)
      if (length(rv$subscriptionTab) == 0) {
        p(paste("Ingen aktive abonnement for", fullName))
      } else {
        tagList(
          p(paste("Aktive abonnement for", fullName, "som sendes per epost til ",
                  rapbase::getUserEmail(session), ":")),
          DT::dataTableOutput("activeSubscriptions")
        )
      }
    })


    ## nye abonnement
    observeEvent (input$subscribe, { #MÅ HA
      #package <- "intensiv"
      owner <- rapbase::getUserName(session)
      interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
      intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
      organization <- rapbase::getUserReshId(session)
      runDayOfYear <- rapbase::makeRunDayOfYearSequence(
        interval = interval
      )
      email <- rapbase::getUserEmail(session)
      print(input$subscriptionRep)
      if (input$subscriptionRep == "Koronarapport") {
        synopsis <- "NIR-Beredskap/Rapporteket: Coronarapport"
        rnwFil <- "BeredskapCorona.Rnw" #Navn på fila
      }
print(rnwFil)
      fun <- "abonnementBeredsk"
      paramNames <- c('rnwFil', 'brukernavn', "reshID", "valgtRHF")
      paramValues <- c(rnwFil, brukernavn, reshID, input$valgtRHF) #input$subscriptionFileFormat)

      #test <- abonnementBeredsk(rnwFil="BeredskapCorona.Rnw", brukernavn='tullebukk',
      #                       reshID=105460)

      rapbase::createAutoReport(synopsis = synopsis, package = 'intensivberedskap',
                                fun = fun, paramNames = paramNames,
                                paramValues = paramValues, owner = owner,
                                email = email, organization = organization,
                                runDayOfYear = runDayOfYear, interval = interval,
                                intervalName = intervalName)
      rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
    })

    ## slett eksisterende abonnement
    observeEvent(input$del_button, {
      selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
      rapbase::deleteAutoReport(selectedRepId)
      rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
    })
}
# Run the application
shinyApp(ui = ui, server = server)

