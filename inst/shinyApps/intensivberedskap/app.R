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
library(tidyverse)
library(lubridate)
library(rapbase)
library(intensiv)
library(intensivberedskap)
library(kableExtra)

addResourcePath('rap', system.file('www', package='rapbase'))
context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- context %in% c("DEV", "TEST", "QA", "PRODUCTION")

#options(knitr.table.format = "html")
idag <- Sys.Date()
datoTil <- as.POSIXlt(idag)
startDato <- '2020-03-01'  #paste0(as.numeric(format(idag-90, "%Y")), '-01-01')
#AarNaa <- as.numeric(format(idag, "%Y"))

regTitle <- ifelse(paaServer,
                   paste0('Norsk Intensiv- og pandemiregister, Beredskapsregistrering ',ifelse(context=='QA', 'QA','')),
                   'Norsk Intensivregister med FIKTIVE data')

#---------Hente data------------

if (paaServer) {
  CoroDataRaa <- NIRberedskDataSQL(kobleInt = 0)
  # qCoro <- 'SELECT *  from ReadinessFormDataContract'
  # CoroDataRaa <- rapbase::LoadRegData(registryName= "nir", query=qCoro, dbType="mysql")
  CoroDataRaa$HovedskjemaGUID <- toupper(CoroDataRaa$HovedskjemaGUID)
  #repLogger(session = session, 'Hentet alle data fra intensivregisteret')
} else {
  # CoroDataRaa <<- read.table('I:/nir/ReadinessFormDataContract2020-06-11 09-31-13.txt', sep=';',
  #                           stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
  # CoroDataRaa$EcmoEnd[CoroDataRaa$EcmoEnd == ""] <- NA
  # CoroDataRaa$EcmoStart[CoroDataRaa$EcmoStart == ""] <- NA
  # CoroDataRaa$MechanicalRespiratorStart[CoroDataRaa$MechanicalRespiratorStart == ""] <- NA
  # CoroDataRaa$MechanicalRespiratorEnd[CoroDataRaa$MechanicalRespiratorEnd == ""] <- NA
  # CoroDataRaa$DateDischargedIntensive[CoroDataRaa$DateDischargedIntensive==""] <- NA
  # NIRraa <- read.table('I:/nir/MainFormDataContract2020-06-12 12-36-21.txt', sep=';',
  #                      stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
} #hente data

#Bruk resh før preprosesserer
CoroData <- NIRPreprosessBeredsk(RegData = CoroDataRaa)
BeredDataOpph <- NIRPreprosessBeredsk(RegData = CoroDataRaa, aggPers = 0)

BeredIntRaa <- NIRberedskDataSQL(kobleInt = 1) #1159 pasienter, 121 manger int: 1047

if (dim(BeredIntRaa)[1]>0) {
  BeredIntPas <- NIRPreprosessBeredsk(RegData = BeredIntRaa, kobleInt = 1)
}

#Influensadata# MÅ GJØRES I EI preprosess-fil eller i spørringa som henter data!!
queryInflu <- paste0('SELECT * FROM InfluensaFormDataContract')
#InfluDataRaa <-  rapbase::loadRegData(registryName = "nir", query = queryInflu, dbType = "mysql")
#InfluDataRaa <- InfluDataRaa[which(as.Date(InfluDataRaa$FormDate) < '2020-08-01'), ]
InfluData <- NIRsqlPreInfluensa() #InfluDataRaa #intensiv::NIRPreprosess(RegData = InfluDataRaa, skjema = 3)


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

sesongNaa <- max(sort(unique(InfluData$Sesong))) #InfluData$Sesong[match(InfluData$InnDato, max(InfluData$InnDato))[1]],
sesongValg <- sort(unique(InfluData$Sesong)) #sesongValg <- rev(c('2018-19', '2019-20', '2020-21', '2021-22', '20')),

## parametre til utsending
orgs <- rhfNavn
names(orgs) <- orgs
orgs <- as.list(orgs)

## make a list for report metadata
reports <- list(
  CovidRapp = list(
    synopsis = "Resultater, Covid-19",
    fun = "abonnementBeredsk", #DENNE MÅ SKRIVES SOM FØR
    paramNames = c('rnwFil', "valgtRHF"),
    paramValues = c('Koronarapport', 'Alle')
  ),
  InfluensaRapp = list(
    synopsis = "Influensarapport",
    fun = "abonnementBeredsk",
    paramNames = c('rnwFil', "valgtRHF"),
    paramValues = c('Influensarapport', 'Alle')
  )
)


source(system.file("shinyApps/intensivberedskap/R/koronafigurer_modul.R", package = "intensivberedskap"), encoding = 'UTF-8')

ui <- tagList(
  navbarPage(id='hovedark',
             title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"),
                         regTitle),
             windowTitle = regTitle,
             theme = "rap/bootstrap.css",

#------------Oversiktsside-----------------------------
             tabPanel("Oversikt",
                      useShinyjs(),
                      sidebarPanel(id = 'brukervalgStartside',
                                   width = 3,
                                   uiOutput('CoroRappTxt'),
                                   # h3('Coronarapport med samling av resultater'),
                                   # h5('Coronarapporten kan man få regelmessig tilsendt på e-post.
                                   #    Gå til fanen "Abonnement" for å bestille dette.'),
                                   downloadButton(outputId = 'CoroRapp.pdf', label='Last ned covid-19rapport', class = "butt"),
                                   tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                                   br(),
                                   br(),
                                   h3('Gjør filtreringer/utvalg:'),
                                   #br(),

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
                                         # uiOutput('utvalgAlder'),
                                         # tableOutput("tabAlder"),
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
                                      # 'Inklusjonskriterier' = 'inklKrit',
                                      # 'Isolasjon, type' = 'isolering',
                                      # 'Isolasjon, varighet' = 'isoleringDogn',
                                       'Liggetid' = 'liggetid',
                                       'NEMS-skår per døgn' = 'NEMS24',
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
                   # dateRangeInput(inputId = 'datovalgMI', start = startDato, end = Sys.Date(),
                   #                label = "Tidsperiode", separator="t.o.m.", language="nb")
                ),
                 mainPanel(
                   h4('Ferdistilte beredskapsskjema som mangler ferdigstillelse av tilhørende intensivskjema'),
                   br(),
                   h5(tags$b('SkjemaGUID'),' er beredskapsskjemaets skjemaID'),
                   h5(tags$b('HovedskjemaGUID'),' er intensivskjemaes skjemaID'),
                   br(),
                   downloadButton(outputId = 'lastNed_ManglerIntSkjema', label='Last ned tabell'),
                   #h5('Datoformatet er lesbart i den nedlastede tabellen'),
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
                       #uiOutput('CoroRappTxt'),
                       h3(paste0('Influensarapport med samling av resultater for sesongen ', sesongNaa)),
                       h5('(Influensasesong oppdateres automatisk når det registreres data for en ny sesong)'),
                        h5('Influensarapporten kan man få regelmessig tilsendt på e-post.
                           Gå til fanen "Abonnement" for å bestille dette.'),
                       br(),
                       downloadButton(outputId = 'InfluRapp.pdf', label='Last ned Influensarapport', class = "butt"),
                       tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                       br(),
                       br(),
                       br(),
                       h3('Gjør filtreringer/utvalg i tabellen til høyre:'),

                       selectInput(inputId = "sesongInf", label="Velg influensasesong",
                                   choices = sesongValg
                                   , selected = sesongNaa
                       ),selectInput(inputId = "bekrInf", label="Bekreftet/Mistenkt",
                                   choices = c("Alle"=9, "Bekreftet"=1, "Mistenkt"=0)
                       ),
                       selectInput(inputId = "skjemastatusInf", label="Skjemastatus",
                                   choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
                       ),
                       selectInput(inputId = "dodIntInf", label="Tilstand ut fra intensiv",
                                   choices = c("Alle"=9, "Død"=1, "Levende"=0, "Ukjent"=-1)
                       ),
                       # selectInput(inputId = "erMannInf", label="Kjønn",
                       #             choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
                       # ),
                       # sliderInput(inputId="alder", label = "Alder",
                       #             min = 0, max = 110,
                       #             value = c(0, 110),
                       #             step = 10
                       # ),
                       # br(),
                       actionButton("tilbakestillValg", label="Tilbakestill valg"),

                       # selectInput(inputId = 'enhetsGruppe', label='Enhetgruppe',
                       #             choices = c("RHF"=1, "HF"=2, "Sykehus"=3)
                       # ),
                       # dateRangeInput(inputId = 'datovalg', start = startDato, end = idag,
                       #                label = "Tidsperiode", separator="t.o.m.", language="nb" #)
                       # ),
                       br(),
                       br(),
                       br(),

                       # h3('Data til FHI'),
                       # h4('Inntil automatsik dataoverføring til FHI er på plass, kan tilsvarende data lastes ned'),
                       # downloadButton("lastNed_DataFHI", "Last ned Influensadata, FHI"),
                       # br(),
                       # br(),
                       # br(),

                       # selectInput("hvilkeFilerTilFHI", "Data:", c("Influensadata" = "InfluDataFHI",
                       #                                             "Testfil" = "Testfil")),
                       # actionButton("bestillDataTilFHI", "Bestill data til FHI"),
                       # br(),
                       # downloadButton(outputId = 'lastNed_filstiDataNHN',
                       #                label='Send filer til NHN og last ned filsti', class = "butt"),
                       br(),
                       br(),


          ),
          mainPanel(width = 9,
                    h3('Resultater fra intensivregisterets influensaregistrering'),
                    h4('Merk at resultatene er basert på til dels ikke-fullstendige registreringer'),
                    #h5('Siden er under utvikling... ', style = "color:red"),
                    br(),

                    #h3('Antall ny-innlagte pasienter, siste 10 dager'),
                    #h4('NB: Inkluderer ikke overføringer mellom intensivenheter'),
                    #uiOutput('utvalgHoved'),
                    tableOutput('tabInfluUkeRHF'),
                    br(),
                    fluidRow()
          ) #main
), #Influensaresultater
#-----------Abonnement--------------------------------
             tabPanel(p("Abonnement",
                        title='Bestill automatisk utsending av rapporter på e-post'),
                      value = 'Abonnement',
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput("subscriptionRep", "Dokument:", c("Koronarapport", "Influensarapport")),
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
                          #med innleggelsesdato t.o.m. 10.mai 2020, div resultater'),
                          br(),
                          #h4('Last ned oppsummeringsdata. Ikke så pen tabell...'),
                          #downloadButton(outputId = 'lastNed_BeredIntOppsumTab', label = 'Last ned oppsummeringstall'),
                          #br(),
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
                     autoReportOrgInput("beredUts"), #Definert slik at RHF benyttes
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
    raplog::appLogger(session = session, msg = "Starter Corona-app")}

  reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 42088921) # 42088921

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

  observe({
    if ((rolle != 'SC') & !(finnesEgenResh)) { #
      shinyjs::hide(id = 'CoroRapp.pdf')
      shinyjs::hide(id = 'CoroRappTxt')
      hideTab(inputId = "hovedark", target = "Abonnement")
    }
    if (!(brukernavn %in% c('lenaro', 'Reidar', 'eabu', 'eivh', 'mariawa-he'))) {  #(brukernavn == 'jlaake') {
      shinyjs::hide(id = 'lastNed_dataBeredNIRraa')
      shinyjs::hide(id = 'lastNed_dataBeredNIR')
    }
    if (!(brukernavn %in% c('lenaro', 'Reidar', 'eabu', 'eivh', 'jlaake', 'mariawa-he'))) { #jlaake-ikke datafiler
      hideTab(inputId = "hovedark", target = "Registeradmin.")
      #hideTab(inputId = "hovedark", target = "Fordelingsfigurer")
    }
     #print(brukernavn)
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
                                           ', bruker: ', brukernavn,
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
    output$InfluRapp.pdf <- downloadHandler(
      filename = function(){
        paste0('InfluensaRapport', Sys.time(), '.pdf')},
      content = function(file){
        henteSamlerapporterBered(file, rnwFil="NIRinfluensa.Rnw") #Vurder å ta med tidsinndeling eller startdato
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
      paste0('For innlagte f.o.m. 10.mars, er gjennomsnittsalderen <b>', round(mean(UtData$RegData$Alder, na.rm = T)), '</b> år og ',
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
    #print(input$datovalgStart[1])
    #print(input$datovalgArt[1])
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

    TabAlder <- TabAlder(RegData=CoroData,
                         valgtRHF= input$valgtRHF, #egetRHF, #
                         dodInt=as.numeric(input$dodInt),
                         resp=as.numeric(input$resp),
                         erMann=as.numeric(input$erMann),
                         bekr=as.numeric(input$bekr),
                         skjemastatus=as.numeric(input$skjemastatus)
    )
    # output$tabAlder<- renderTable({xtable::xtable(TabAlder$Tab)}, rownames = T, digits=0, spacing="xs")
    output$utvalgAlder <- renderUI({h5(HTML(paste0(TabAlder$utvalgTxt, '<br />'))) })


  })

  #------------------Datakvalitet-----------------------
  #Ferdigstilte beredskapsskjema uten ferdigstilt intensivskjema:
  #reshSe <- ifelse(rolle == 'SC', 0, reshID)
  observe({
    #print(input$datoValgMI[1])
    #print(input$datoValgMI[2])
    #print(input$datoFraMI)

  ManglerIntSkjemaTab <- ManglerIntSkjema(reshID = ifelse(rolle == 'SC', 0, reshID)
                                          ,datoFra = input$datoFraMI)
                                          #,datoFra = input$datoValgMI[1], datoTil = input$datoValgMI[2])
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
  ## reaktive verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  # rv <- reactiveValues(
  #   subscriptionTab = rapbase::makeUserSubscriptionTab(session))

  subscription <- reactiveValues(
    tab = rapbase::makeAutoReportTab(session, type = "subscription"))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    subscription$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    rownames = FALSE, options = list(dom = 't')
  )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    fullName <- rapbase::getUserFullName(session)
    if (length(subscription$subscriptionTab) == 0) {
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
    owner <- rapbase::getUserName(session)
    interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
    intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
    organization <- rapbase::getUserReshId(session)
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = interval
    )
    email <- rapbase::getUserEmail(session)

    if (input$subscriptionRep == "Influensarapport") {
      synopsis <- "NIR-Beredskap/Rapporteket: Influensarapport"
      rnwFil <- "NIRinfluensa.Rnw" #Navn på fila
    }
    # fun <- "abonnementKorona"
    # paramNames <- c('rnwFil', 'brukernavn', "reshID") #, "valgtEnhet")
    # paramValues <- c(rnwFil, brukernavn, reshID) #, as.character(input$valgtEnhetabb))


    if (input$subscriptionRep == "Koronarapport") {
      synopsis <- "NIR-Beredskap/Rapporteket: Coronarapport"
      rnwFil <- "BeredskapCorona.Rnw" #Navn på fila
    }
    fun <- "abonnementBeredsk"
    paramNames <- c('rnwFil', 'brukernavn', "reshID", "valgtRHF")
    paramValues <- c(rnwFil, brukernavn, reshID, as.character(input$valgtRHFabb)) #valgtRHF) #

    # test <- abonnementBeredsk(rnwFil="BeredskapCorona.Rnw", brukernavn='tullebukk',
    #                       reshID=105460, valgtRHF = as.character(input$valgtRHFabb))

    rapbase::createAutoReport(synopsis = synopsis, package = 'intensivberedskap',
                              fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear, interval = interval,
                              intervalName = intervalName)
    subscription$subscriptionTab <- rapbase::makeAutoReportTab(session, type = "subscription") #makeUserSubscriptionTab(session)
  })

  ## slett eksisterende abonnement
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    subscription$subscriptionTab <- rapbase::makeAutoReportTab(session, type = "subscription") #makeUserSubscriptionTab(session)
  })

  #------------Utsending-----------------

  org <- autoReportOrgServer("beredUts", orgs)


  # set reactive parameters overriding those in the reports list
  paramNames <- shiny::reactive("valgtRHF")
  paramValues <- shiny::reactive(org$value())


  autoReportServer(
    id = "beredUts", registryName = "rapbase", type = "dispatchment",
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


  # output$tabAlder<- renderTable({xtable::xtable()}, rownames = F, digits=0, spacing="xs")

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

  #git

  #----------Figurer, modul og fordelinger #################################

  callModule(koronafigurer, "koronafigurer_id", rolle = rolle, CoroData = CoroData, egetRHF = egetRHF, reshID=reshID)


  output$fordelinger <- renderPlot({
    #print(paste0('FigFord_', input$valgtVar, '.', input$bildeformatFord))
    NIRberedskFigAndeler(RegData=BeredIntPas, preprosess = 0, valgtVar=input$valgtVar,
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
      NIRberedskFigAndeler(RegData=BeredIntPas, preprosess = 0, valgtVar=input$valgtVar,
                           # reshID=reshID,
                           # enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                           bekr=as.numeric(input$bekrFord),
                           datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                           # minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                           erMann=as.numeric(input$erMannFord), session = session,
                       outfile = file)
    }
  )

    observe({
    UtDataFord <- NIRberedskFigAndeler(RegData=BeredIntPas, preprosess = 0,
                                       valgtVar=input$valgtVar,
                                       # reshID=reshID,
                                       # enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                       bekr=as.numeric(input$bekrFord),
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                       # minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
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
        column_spec(column = 1, width_min = '7em') %>%
        column_spec(column = 2:(ncol(tab)+1), width = '7em') %>%
        row_spec(0, bold = T)
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
  observeEvent(input$tilbakestillValg, shinyjs::reset("brukervalgInfluensa"))

  output$tabInfluUkeRHF <- renderTable({
    TabUkeRHFinflu <- InfluensaUkeRHF(RegData=InfluData, bekr=as.numeric(input$bekrInf),
                                      skjemastatus=as.numeric(input$skjemastatusInf),
                                      dodInt = as.numeric(input$dodIntInf),
                                      #erMann = as.numeric(input$erMannInf),
                                      sesong=input$sesongInf)
    xtable::xtable(TabUkeRHFinflu)}, rownames = T, digits=0, spacing="xs"
  )


  # #Send filer til FHI: kopi fra korona
  # output$lastNed_filstiDataNHN <- downloadHandler(
  #   filename = function(){
  #     paste0('Filsti', Sys.time(), '.csv')},
  #   content = function(file, filename){
  #     Filsti <- sendInfluDataFHI(zipFilNavn=input$hvilkeFilerTilFHI) #brukernavn = brukernavn)
  #     write.csv2(x=Filsti, file, row.names = F, na = '') #x - r-objektet
  #   })
  #
  # #Manuell nedlasting av FHI-data (influensa)
  # InfluensadataFHI <- lagInfluDataFHI()
  #
  # output$lastNed_DataFHI <- downloadHandler(
  #   filename = function(){
  #     paste0('DataInfluensaFHI.', Sys.Date(), '.csv')
  #   },
  #   content = function(file, filename){
  #     write.csv2(InfluensadataFHI, file, row.names = F, na = '')
  #   })


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

    #subscription$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
    subscription$tab <-
      rapbase::makeAutoReportTab(session, type = "subscription")

  })



  #-----------Registeradmin.------------
  #CoroDataRaa <- NIRberedskDataSQL()

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
