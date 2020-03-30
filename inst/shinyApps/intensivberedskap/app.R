#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(magrittr)
library(rapbase)
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
                   paste0('Norsk Intensivregister, Beredskapsregistrering ',ifelse(context=='QA', 'QA','')),
                   'Norsk Intensivregister med FIKTIVE data')

#---------Hente data------------

if (paaServer) {
  #CoroData <- NIRRegDataSQL(datoFra='2011-01-01', skjema=4) #, session = session) #datoFra = datoFra, datoTil = datoTil)
  qCoro <- 'SELECT *  from ReadinessFormDataContract'
  CoroData <- rapbase::LoadRegData(registryName= "nir", query=qCoro, dbType="mysql")
  #repLogger(session = session, 'Hentet alle data fra intensivregisteret')
} else {
  CoroData <- read.table('I:/nir/ReadinessFormDataContract2020-03-30 08-15-02.csv', sep=';',
                         stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
} #hente data


CoroData <- NIRPreprosessBeredsk(RegData = CoroData)
#CoroData <- preprosessBeredVar(RegData = CoroData)


#-----Definere utvalgsinnhold og evt. parametre som er statiske i appen----------


#Definere utvalgsinnhold
#sykehusNavn <- sort(c('',unique(CoroData$ShNavn)), index.return=T)
#sykehusValg <- c(0,unique(CoroData$ReshId))[sykehusNavn$ix]
rhfNavn <- c('Alle', as.character(sort(unique(CoroData$RHF))))
hfNavn <- sort(unique(CoroData$HF)) #, index.return=T)
sykehusNavn <- sort(unique(CoroData$ShNavn), index.return=T)
sykehusValg <- unique(CoroData$ReshId)[sykehusNavn$ix]
sykehusValg <- c(0,sykehusValg)
names(sykehusValg) <- c('Ikke valgt',sykehusNavn$x)
#updateTextInput(session, inputId, label = NULL, value = NULL). Hvis input skal endres som flge av et annet input.
enhetsNivaa <- c('RHF', 'HF', 'ShNavn')
names(enhetsNivaa) <- c('RHF', 'HF', 'Sykehus')

source(system.file("shinyApps/intensivberedskap/R/koronafigurer_modul.R", package = "intensivberedskap"), encoding = 'UTF-8')

ui <- tagList(
  navbarPage(id='hovedark',
             title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"),
                         regTitle),
             windowTitle = regTitle,
             theme = "rap/bootstrap.css",


             tabPanel("Oversikt",
                      useShinyjs(),
                      sidebarPanel(id = 'brukervalgStartside',
                                   width = 3,
                                   uiOutput('CoroRappTxt'),
                                   # h3('Coronarapport med samling av resultater'),
                                   # h5('Coronarapporten kan man få regelmessig tilsendt på e-post.
                                   #    Gå til fanen "Abonnement" for å bestille dette.'),
                                   downloadButton(outputId = 'CoroRapp.pdf', label='Last ned Coronarapport', class = "butt"),
                                   tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                                   br(),
                                   br(),
                                   h3('Gjør filtreringer/utvalg:'),
                                   #br(),

                                   # conditionalPanel(condition = "input.hovedark == 'Nøkkeltall' || input.ark == 'Ant. opphold'",
                                   #                  dateInput(inputId = 'sluttDatoReg', label = 'Velg sluttdato', language="nb",
                                   #                            value = Sys.Date(), max = Sys.Date())
                                   # ),
                                   selectInput(inputId = "valgtRHF", label="Velg RHF",
                                               choices = rhfNavn
                                   ),

                                   #br(),
                                   selectInput(inputId = "bekr", label="Bekreftet/Mistenkt",
                                               choices = c("Alle"=9, "Bekreftet"=1, "Mistenkt"=0)
                                   ),
                                   selectInput(inputId = "skjemastatus", label="Skjemastatus",
                                               choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
                                   ),
                                   # selectInput(inputId = "dodInt", label="Tilstand ut fra intensiv",
                                   #             choices = c("Alle"=9, "Død"=1, "Levende"=0)
                                   # ),
                                   selectInput(inputId = "erMann", label="Kjønn",
                                               choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
                                   ),
                                   h4('Kun for risikofaktorer:'),
                                   sliderInput(inputId="alder", label = "Alder",
                                               min = 0, max = 110,
                                               value = c(0, 110),
                                               step = 10
                                   ),
                                   br(),
                                   actionButton("tilbakestillValg", label="Tilbakestill valg")

                                   # selectInput(inputId = 'enhetsGruppe', label='Enhetgruppe',
                                   #             choices = c("RHF"=1, "HF"=2, "Sykehus"=3)
                                   # ),
                                   # dateRangeInput(inputId = 'datovalg', start = startDato, end = idag,
                                   #                label = "Tidsperiode", separator="t.o.m.", language="nb" #)
                                   # ),
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
                                #h5('Siden er under utvikling... ', style = "color:red"),
                                br(),
                                fluidRow(
                                  column(width = 4,
                                         h4('Opphold uten registrert ut-tid fra intensiv'), #, align='center'),
                                         uiOutput('liggetidNaa'),
                                         uiOutput('utvalgNaa'),
                                         tableOutput('tabECMOrespirator'),
                                         br(),
                                         h4('Opphold registrert som utskrevet, uten ferdigstilt skjema:'),
                                         #h4('Antall opphold registrert som utskrevet, med ikke ferdigstilt skjema'),
                                         uiOutput('RegIlimbo')
                                  ),
                                  column(width=5, offset=1,
                                         uiOutput('tittelFerdigeReg'),
                                         uiOutput('utvalgFerdigeReg'),
                                         tableOutput('tabFerdigeReg')
                                  )),

                                h3('Antall intensivopphold'),
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
                                         uiOutput('utvalgAlder'),
                                         tableOutput('tabAlder')
                                  ))
                      ) #main
             ), #tab Tabeller

             #-----------Abonnement--------------------------------
             tabPanel(p("Abonnement",
                        title='Bestill automatisk utsending av rapporter på e-post'),
                      value = 'Abonnement',
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput("subscriptionRep", "Dokument:", c("Koronarapport")),
                                     selectInput("subscriptionFreq", "Frekvens:",
                                                 list(Månedlig="Månedlig-month",
                                                      Ukentlig="Ukentlig-week",
                                                      Daglig="Daglig-DSTday"),
                                                 selected = "Ukentlig-week"),
                                     selectInput(inputId = "valgtRHFabb", label="Velg RHF",
                                                 choices = rhfNavn
                                     ),
                                     actionButton("subscribe", "Bestill!")
                        ),
                        mainPanel(
                          h4('NB: Abonnementet løper til det sies opp. '),
                          uiOutput("subscriptionContent")
                        )
                      )
             ), #tab abonnement
             #------------Figurer-----------------------------------
             tabPanel("Figurer",
                      koronafigurer_UI(id = "koronafigurer_id", rhfNavn=rhfNavn)
             )


  ) # navbarPage
) # tagList
#----------Slutt ui-del--------------


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Last inn data
  # regData <- getFakeRegData()

  #-----------Div serveroppstart------------------
  if (context %in% c('QA', 'PRODUCTION')){
    raplog::appLogger(session = session, msg = "Starter Corona-app")}

  reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 0) # 42088921

  rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')
  brukernavn <- ifelse(paaServer, rapbase::getUserName(shinySession=session), 'brukernavn')

  finnesEgenResh <- reshID %in% unique(CoroData$ReshId)
  if (finnesEgenResh) {
    indReshEgen <- match(reshID, CoroData$ReshId)
    egetShNavn <- as.character(CoroData$ShNavn[indReshEgen])
    egetRHF <- as.character(CoroData$RHF[indReshEgen])
    egetHF <- as.character(CoroData$HF[indReshEgen])
    egenLokalitet <- c(0, 2, 4, 7)
    names(egenLokalitet) <- c('hele landet', egetShNavn, egetRHF)
  } else {
    egetRHF <- 'Ukjent'
  }
  egetRHF <- ifelse(rolle=='SC', 'Alle', egetRHF)

  observe({if ((rolle != 'SC') & !(finnesEgenResh)) { #
    shinyjs::hide(id = 'CoroRapp.pdf')
    shinyjs::hide(id = 'CoroRappTxt')
    hideTab(inputId = "hovedark", target = "Abonnement")
  }
  })
  if (rolle != 'SC') {
    updateSelectInput(session, "valgtRHF",
                      choices = unique(c('Alle', ifelse(egetRHF=='Ukjent', 'Alle',
                                                        egetRHF))))
    #CoroData$RHF[match(reshID, CoroData$ReshId)]))
    updateSelectInput(session, "valgtRHFabb",
                      choices = egetRHF) #unique(c('Alle', egetRHF)))
    #CoroData$RHF[match(reshID, CoroData$ReshId)]))
  }



  # widget
  if (paaServer) {
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', rolle,
                                           '<br> ReshID: ', reshID) )}
  #,'<br> Org: ', egenOrg) )}

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
    valgtRHF <- ifelse(rolle == 'LU', egetRHF, as.character(input$valgtRHF))
    output$CoroRapp.pdf <- downloadHandler(
      filename = function(){
        paste0('CoronaRapport', Sys.time(), '.pdf')},
      content = function(file){
        henteSamlerapporterBered(file, rnwFil="BeredskapCorona.Rnw",
                                 #rolle = rolle,
                                 valgtRHF = valgtRHF, #as.character(input$valgtRHF),
                                 reshID = reshID) #Vurder å ta med tidsinndeling eller startdato
      }
    )
  })

  output$CoroRappTxt <- renderUI(tagList(
    h3(HTML('Coronarapport med samling av resultater')),
    h5(HTML('Coronarapporten kan man få regelmessig tilsendt på e-post.
                    Gå til fanen "Abonnement" for å bestille dette')))
  )

  #----------Resultater som tekst--------------

  output$liggetidNaa <- renderUI({
  })

  #----------Tabeller----------------------------

  observeEvent(input$tilbakestillValg, shinyjs::reset("brukervalgStartside"))

  observe({

    valgtRHF <- ifelse(rolle == 'SC', as.character(input$valgtRHF), egetRHF)
    print(dim(CoroData)[1])
    print(valgtRHF)

    AntTab <- TabTidEnhet(RegData=CoroData, tidsenhet='dag',
                          valgtRHF= valgtRHF,
                          skjemastatus=as.numeric(input$skjemastatus),
                          bekr=as.numeric(input$bekr),
                          #dodInt=as.numeric(input$dodInt),
                          erMann=as.numeric(input$erMann)
    )

    UtData <- NIRUtvalgBeredsk(RegData=CoroData,
                               valgtRHF= ifelse(valgtRHF=='Ukjent','Alle',valgtRHF),
                               skjemastatus=as.numeric(input$skjemastatus),
                               bekr=as.numeric(input$bekr),
                               #dodInt=as.numeric(input$dodInt),
                               erMann=as.numeric(input$erMann)
    )

    utvalg <- if (length(UtData$utvalgTxt)>0) {
      UtData$utvalgTxt
    } else {'Alle registrerte '}
    txt <- if(dim(UtData$RegData)[1]>2) {
      paste0('Gjennomsnittsalderen er <b>', round(mean(UtData$RegData$Alder, na.rm = T)), '</b> år og ',
             round(100*mean(UtData$RegData$erMann, na.rm = T)), '% er menn. Antall døde: ',
             sum(UtData$RegData$DischargedIntensivStatus==1))
    } else {''}

    output$utvalgHoved <- renderUI({
      UtTekst <- tagList(
        h5(HTML(paste0(utvalg, '<br />'))),
        h4(HTML(paste0(txt, '<br />')))

      )})

    output$tabTidEnhet <- renderTable({AntTab$Tab}, rownames = T, digits=0, spacing="xs"
    )

    #Tab status nå
    statusNaaTab <- statusECMOrespTab(RegData=CoroData, valgtRHF=input$valgtRHF,
                                      erMann=as.numeric(input$erMann),
                                      bekr=as.numeric(input$bekr))
    output$tabECMOrespirator <- renderTable({statusNaaTab$Tab}, rownames = T, digits=0, spacing="xs")
    output$utvalgNaa <- renderUI({h5(HTML(paste0(statusNaaTab$utvalgTxt, '<br />'))) })

    #Tab ferdigstilte
    TabFerdig <- oppsumFerdigeRegTab(RegData=CoroData,
                                     valgtRHF=input$valgtRHF,
                                     bekr = as.numeric(input$bekr),
                                     erMann=as.numeric(input$erMann))

    output$tabFerdigeReg <- if (TabFerdig$Ntest>2){
      renderTable({TabFerdig$Tab}, rownames = T, digits=0, spacing="xs")} else {
        renderText('Få registreringer (N<3)')}

    output$utvalgFerdigeReg <- renderUI({h5(HTML(paste0(TabFerdig$utvalgTxt, '<br />'))) })
    output$tittelFerdigeReg <- renderUI(
      h4(paste0('Fullførte registreringer (', TabFerdig$Ntest, ' skjema)')))

    #Registreringer i limbo:
    #Må ha egen funksjon for å få dette på sykehusnivå
    output$RegIlimbo <- renderUI({
      # AntIlibo <- AntTab$Ntest - (TabFerdig$Ntest + sum(is.na(CoroData$DateDischargedIntensive))) #RHF/alle
      finnBurdeFerdig <- function(RegData) {sum((!(is.na(RegData$DateDischargedIntensive)) & (RegData$FormStatus!=2)))}
      #RegData <- CoroData
      valgtRHF <- input$valgtRHF
      tittel <- 'Opphold registrert som utskrevet, uten ferdigstilt skjema: '
      #'Antall opphold registrert som utskrevet, med ikke ferdigstilt skjema',

      AntBurdeFerdig <-
        c( #tittel,
          if (rolle=='LU' & finnesEgenResh) {
            paste0(finnBurdeFerdig(CoroData[(which(CoroData$ReshId==reshID)), ]),' skjema for ', egetShNavn)},
          #paste0(egetShNavn, ': ', finnBurdeFerdig(CoroData[(which(CoroData$ReshId==reshID)), ]), ' skjema ')},
          if (valgtRHF=='Alle') {
            paste0(finnBurdeFerdig(CoroData), ' skjema for hele landet')
            #paste0('Hele landet: ', finnBurdeFerdig(CoroData), ' skjema')
          } else {
            paste0(finnBurdeFerdig(CoroData[CoroData$RHF==valgtRHF, ]), ' skjema for ', valgtRHF)}
          #paste0(valgtRHF, ': ', finnBurdeFerdig(CoroData[CoroData$RHF==valgtRHF, ]))},
        )
      h5(HTML(paste0('&nbsp;&nbsp;&nbsp;', AntBurdeFerdig, '<br />')))
    })


    #Tab risiko
    RisikoTab <- RisikofaktorerTab(RegData=CoroData, tidsenhet='Totalt',
                                   valgtRHF= input$valgtRHF,
                                   skjemastatus=as.numeric(input$skjemastatus),
                                   bekr=as.numeric(input$bekr),
                                   #dodInt=as.numeric(input$dodInt),
                                   erMann=as.numeric(input$erMann),
                                   minald=as.numeric(input$alder[1]),
                                   maxald=as.numeric(input$alder[2]))


    output$tabRisikofaktorer <- if (RisikoTab$Ntest>2){
      renderTable(RisikoTab$Tab, rownames = T, digits=0, spacing="xs") } else {
        renderText('Få registreringer (N<3)')}
    output$utvalgRisiko <- renderUI({h5(HTML(paste0(RisikoTab$utvalgTxt, '<br />'))) #tagList()
    })

    TabAlder <- TabAlder(RegData=CoroData,
                         valgtRHF= egetRHF, #input$valgtRHF,
                         #dodInt=as.numeric(input$dodInt),
                         erMann=as.numeric(input$erMann),
                         bekr=as.numeric(input$bekr),
                         skjemastatus=as.numeric(input$skjemastatus)
    )
    output$tabAlder<- renderTable({xtable::xtable(TabAlder$Tab)}, rownames = T, digits=0, spacing="xs")
    output$utvalgAlder <- renderUI({h5(HTML(paste0(TabAlder$utvalgTxt, '<br />'))) })


  })

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
    if (input$subscriptionRep == "Koronarapport") {
      synopsis <- "NIR-Beredskap/Rapporteket: Coronarapport"
      rnwFil <- "BeredskapCorona.Rnw" #Navn på fila
    }
    fun <- "abonnementBeredsk"
    paramNames <- c('rnwFil', 'brukernavn', "reshID", "valgtRHF")

    paramValues <- c(rnwFil, brukernavn, reshID, as.character(input$valgtRHFabb)) #valgtRHF) #

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

  ################# Modul for figurer #################################

  callModule(koronafigurer, "koronafigurer_id", rolle = rolle, CoroData = CoroData, egetRHF = egetRHF)

}
# Run the application
shinyApp(ui = ui, server = server)

