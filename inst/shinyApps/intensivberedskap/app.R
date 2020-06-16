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
  CoroDataRaa <- rapbase::LoadRegData(registryName= "nir", query=qCoro, dbType="mysql")
  CoroDataRaa$HovedskjemaGUID <- toupper(CoroDataRaa$HovedskjemaGUID)
  #repLogger(session = session, 'Hentet alle data fra intensivregisteret')
} else {
  CoroDataRaa <<- read.table('I:/nir/ReadinessFormDataContract2020-06-11 09-31-13.txt', sep=';',
                            stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
  CoroDataRaa$EcmoEnd[CoroDataRaa$EcmoEnd == ""] <- NA
  CoroDataRaa$EcmoStart[CoroDataRaa$EcmoStart == ""] <- NA
  CoroDataRaa$MechanicalRespiratorStart[CoroDataRaa$MechanicalRespiratorStart == ""] <- NA
  CoroDataRaa$MechanicalRespiratorEnd[CoroDataRaa$MechanicalRespiratorEnd == ""] <- NA
  CoroDataRaa$DateDischargedIntensive[CoroDataRaa$DateDischargedIntensive==""] <- NA
  NIRraa <- read.table('I:/nir/MainFormDataContract2020-06-12 12-36-21.txt', sep=';',
                       stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
} #hente data

#Bruk resh før preprosesserer
CoroData <- NIRPreprosessBeredsk(RegData = CoroDataRaa)
#CoroData <- preprosessBeredVar(RegData = CoroData)

#Koble på intensivdata. Først til bruk i artikkel
forsteReg <- min(as.Date(CoroDataRaa$FormDate))
if (paaServer) {
  IntDataRaa <- intensiv::NIRRegDataSQL(datoFra = forsteReg)
} else {
  IntDataRaa <- NIRraa[as.Date(NIRraa$DateAdmittedIntensive) >= forsteReg, ]
}

#Felles variabler som skal hentes fra intensiv (= fjernes fra beredskap)
varFellesInt <- c('DateAdmittedIntensive', 'DateDischargedIntensive',	'DaysAdmittedIntensiv',
                  'DeadPatientDuring24Hours',	'MechanicalRespirator',	'RHF', 'TransferredStatus',
                  'VasoactiveInfusion',	'MoreThan24Hours',	'Morsdato',
                  'MovedPatientToAnotherIntensivDuring24Hours',	'PatientAge',	'PatientGender',
                  #'PatientInRegistryGuid', 'FormStatus', 'ShNavn',
                  'UnitId')

BeredRaa <- CoroDataRaa[ ,-which(names(CoroDataRaa) %in% varFellesInt)]
#names(IntDataRaa) #Enders når vi har bestemt hvilke variabler vi skal ha med
#varIKKEmed <- CerebralCirculationAbolished	CerebralCirculationAbolishedReasonForNo	CurrentMunicipalNumber	DistrictCode	Eeg	FormStatus	FormTypeId	HF	HFInt	Hyperbar	Iabp	Icp	Isolation	LastUpdate	Leverdialyse	MajorVersion	MinorVersion	MorsdatoOppdatert	Municipal	MunicipalNumber	Nas	No	OrganDonationCompletedReasonForNoStatus	OrganDonationCompletedStatus	Oscillator	PIM_Probability	PIM_Score	PostalCode	RHF	Sykehus	TerapetiskHypotermi	UnitIdInt
BeredIntRaa1 <- merge(BeredRaa, IntDataRaa, suffixes = c('','Int'),
                      by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = F, all.y=F)
#intvar <- names(BeredIntRaa)[grep('Int', names(BeredIntRaa))]
varMed <- c('Age', 'AgeAdmitted', 'Astma', 'Bilirubin', 'Birthdate', 'BrainDamage',
            'Bukleie', 'ChronicDiseases', 'Diabetes', 'Diagnosis', 'DischargedIntensivStatus',
            'EcmoEcla', 'EcmoEnd', 'EcmoStart', 'ExtendedHemodynamicMonitoring', 'FrailtyIndex',
            'Glasgow', 'Graviditet', 'Hco3', 'HeartRate',
            'HovedskjemaGUID', 'Impella', 'Intermitterende', 'IntermitterendeDays',
            'InvasivVentilation', 'IsActivSmoker', 'IsChronicLungDiseasePatient',
            'IsChronicNeurologicNeuromuscularPatient', 'IsEcmoTreatmentAdministered',
            'IsHeartDiseaseIncludingHypertensionPatient', 'IsImpairedImmuneSystemIncludingHivPatient',
            'IsKidneyDiseaseIncludingFailurePatient', 'IsLiverDiseaseIncludingFailurePatient',
            'IsObesePatient', 'Isolation', 'IsolationDaysTotal', 'IsRiskFactor', 'KidneyReplacingTreatment',
            'Kontinuerlig', 'KontinuerligDays', 'Kreft', 'Leukocytes', 'MechanicalRespirator',
            'MechanicalRespiratorEnd', 'MechanicalRespiratorStart', 'Municipal','MunicipalNumber',
            'MvOrCpap', 'Nems', 'NonInvasivVentilation',
            'PatientTransferredFromHospital', 'PatientTransferredFromHospitalName',
            'PatientTransferredToHospital', 'PatientTransferredToHospitalName', 'Potassium',
            'PrimaryReasonAdmitted', 'Respirator', 'Saps2Score', 'Saps2ScoreNumber',
            'SerumUreaOrBun', 'ShType', 'SkjemaGUID', 'Sodium', 'SystolicBloodPressure',
            'Temperature', 'Trakeostomi', 'TypeOfAdmission', 'UrineOutput',
            'PatientInRegistryGuid') #'Helseenhet', 'HelseenhetID','ShNavn', 'ReshId',
beregnVar <- c('Birthdate', 'FormDate', 'FormStatus', 'HF', 'HelseenhetKortnavn')
BeredIntRaa <- BeredIntRaa1[ ,c(varMed, varFellesInt, beregnVar)] #c()]

if (dim(BeredIntRaa)[1]>0) {
  BeredIntPas <- NIRPreprosessBeredsk(RegData = BeredIntRaa, kobletInt = 1)
}


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

             #------------Oversiktsside-----------------------------
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

                                   selectInput(inputId = "valgtRHF", label="Velg RHF",
                                               choices = rhfNavn
                                   ),
                                   selectInput(inputId = "bekr", label="Bekreftet/Mistenkt",
                                               choices = c("Alle"=9, "Bekreftet"=1, "Mistenkt"=0)
                                   ),
                                   selectInput(inputId = "skjemastatus", label="Skjemastatus",
                                               choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
                                   ),
                                   selectInput(inputId = "resp", label="Respiratorbehandlet",
                                               choices = c("Alle"=9, "Ja"=1, "Nei"=2)
                                   ),
                                   selectInput(inputId = "dodInt", label="Tilstand ut fra intensiv",
                                               choices = c("Alle"=9, "Død"=1, "Levende"=0)
                                   ),
                                   selectInput(inputId = "erMann", label="Kjønn",
                                               choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
                                   ),
                                   dateRangeInput(inputId = 'datovalgStart', start = startDato, end = '2020-05-10',
                                                  label = "Tidsperiode", separator="t.o.m.", language="nb"
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
                                         br(),
                                         #downloadButton("LastNedFigAldKj", "Last ned figur"),
                                         br(),
                                         br(),
                                         downloadButton("lastNedAldKj", "Last ned tabell")

                                  ))
                      ) #main
             ), #tab Tabeller

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
                          choices = c('Alder' = 'alder',
                                       'Bukleie' = 'bukleie',
                                      # 'Hemodynamisk overvåkn.' = 'ExtendedHemodynamicMonitoring',
                                       'Frailty index' = 'frailtyIndex',
                                      # 'Inklusjonskriterier' = 'inklKrit',
                                       'Isolasjon, type' = 'isolering',
                                      # 'Isolasjon, varighet' = 'isoleringDogn',
                                       'Liggetid' = 'liggetid',
                                      # 'Nas-skår (sykepleierakt.)' = 'Nas24',
                                       'NEMS-skår per døgn' = 'NEMS24',
                                      # 'Nyreerstattende beh., type' = 'nyreBeh',
                                      # 'Nyreerstattende beh., varighet' = 'nyreBehTid',
                                      # 'Potensielle donorer, årsak ikke påvist opph. sirkulasjon' = 'CerebralCirculationAbolishedReasonForNo',
                                       'Primærårsak' = 'PrimaryReasonAdmitted',
                                       'Respiratortid, intensiv' = 'RespiratortidInt',
                                       'Respiratortid, ikke-invasiv' = 'respiratortidNonInv',
                                       'Respiratortid, invasiv' = 'respiratortidInv',
                                       'SAPSII-skår (alvorlighet av sykd.)' = 'Saps2ScoreNumber'
                                      # 'Spesielle tiltak' = 'spesTiltak',
                                      )
                          ),
                        dateRangeInput(inputId = 'datovalg', start = startDato, end = '2020-05-10',
                            label = "Tidsperiode", separator="t.o.m.", language="nb"
                            ),
                        selectInput(inputId = "erMann", label="Kjønn",
                                    choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
                        )
                        ),
                      mainPanel(
                      #h2('Fordelingsfigurer for variabler registrert under intensivoppholdene'),
                      h3('Data er aggregerte til pasientnivå'),
                        plotOutput('fordelinger'),
                      br()
                      )
                      ), #Tab, fordelinger

             #---------Datakvalitet-----------------
             tabPanel(#p('Tilhørende intensivskjema som mangler ferdigstillelse'),
               title = 'Datakvalitet',
               value = 'Datakvalitet',
               sidebarLayout(
                 sidebarPanel(width = 2
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

             #-----------Artikkelarbeid------------
             tabPanel(p("Artikkelarbeid",
                        title='Data til artikkel'),
                      value = 'Artikkelarbeid',
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     h3('Covidpasienter med innleggelsesdato t.o.m. 10.mai 2020'),
                                     h4('Koblet RÅdatatsett: Covid-opphold og tilhørende intensivskjema'),
                                     downloadButton(outputId = 'lastNed_dataBeredNIRraa', label='Last ned rådata'),
                                     br(),
                                     h4('Koblet datatsett: Covid-pasienter'),
                                     downloadButton(outputId = 'lastNed_dataBeredNIR', label='Last ned data'),
                                     br(),
                                     br(),
                                     h3('Last ned oppsummeringsdata'),
                                     h4('P.t. ikke så pen tabell...'),
                                     downloadButton(outputId = 'lastNed_BeredIntOppsumTab', label = 'Last ned oppsummeringstall')
                        ),
                        mainPanel(

                        )
                      )
             ) #tab artikkelarb


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
    if ((rolle != 'SC') | !(brukernavn %in% c('lenaro', 'Reidar', 'eabu'))) { #
      hideTab(inputId = "hovedark", target = "Artikkelarbeid")
      hideTab(inputId = "hovedark", target = "Fordelingsfigurer")
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
             sum(UtData$RegData$DischargedIntensivStatus==1))
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
    RisikoTab <- RisikofaktorerTab(RegData=CoroData, tidsenhet='Totalt',
                                   valgtRHF= input$valgtRHF,
                                   skjemastatus=as.numeric(input$skjemastatus),
                                   bekr=as.numeric(input$bekr),
                                   resp=as.numeric(input$resp),
                                   dodInt=as.numeric(input$dodInt),
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
  ManglerIntSkjemaTab <- ManglerIntSkjema(reshID = ifelse(rolle == 'SC', 0, reshID))
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
    owner <- rapbase::getUserName(session)
    interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
    intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
    organization <- rapbase::getUserReshId(session)
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = interval
    )
    email <- rapbase::getUserEmail(session)

    # if (input$subscriptionRep == "Koronarapport") {
    #   synopsis <- "Rapporteket-Pandemi: Koronarapport"
    #   rnwFil <- "KoronaRapport.Rnw" #Navn på fila
    # }
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
    # print(input$valgtRHFabb)
    # print(test)

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
      paste0('AldKjFig', Sys.time(), '.', input$bildeformat)
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
    NIRberedskFigAndeler(RegData=BeredIntPas, preprosess = 0, valgtVar=input$valgtVar,
                  # reshID=reshID,
                  # enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                  datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                  # minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                  erMann=as.numeric(input$erMann), session = session
                  )
  }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
  )

  # observe({
  #
  #   UtDataFord <- NIRberedskFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
  #                               reshID=reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
  #                               velgAvd = input$velgResh,
  #                               datoFra=input$datovalg[1], datoTil=input$datovalg[2],
  #                               #minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
  #                               erMann=as.numeric(input$erMann), lagFig = 0, session = session)
  #   #RegData <- NIRRegDataSQL(datoFra = '2018-01-01')
  #   #UtDataFord <- NIRberedskFigAndeler(RegData=RegData, valgtVar='bukleie', reshID=109773, enhetsUtvalg=0 )
  #   tab <- lagTabavFig(UtDataFraFig = UtDataFord)
  #
  #   output$tittelFord <- renderUI({
  #     tagList(
  #       h3(HTML(paste(UtDataFord$tittel, sep='<br />'))),
  #       h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
  #     )}) #, align='center'
  #   output$fordelingTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
  #
  #     #       kable_styling("hover", full_width = F)
  #     antKol <- ncol(tab)
  #     kableExtra::kable(tab, format = 'html'
  #                       , full_width=F
  #                       , digits = c(0,1,0,1)[1:antKol]
  #     ) %>%
  #       add_header_above(c(" "=1, 'Valgt gruppe' = 2, 'Resten' = 2)[1:(antKol/2+1)]) %>%
  #       column_spec(column = 1, width_min = '7em') %>%
  #       column_spec(column = 2:(ncol(tab)+1), width = '7em') %>%
  #       row_spec(0, bold = T)
  #   }
  #
  #   output$lastNed_tabFord <- downloadHandler(
  #     filename = function(){
  #       paste0(input$valgtVar, '_fordeling.csv')
  #     },
  #     content = function(file, filename){
  #       write.csv2(tab, file, row.names = F, na = '')
  #     })
  # }) #observe


  #-----------Artikkelarbeid------------
  #CoroDataRaa <- NIRberedskDataSQL()

  BeredIntRaaArt <- BeredIntRaa[which(as.Date(BeredIntRaa$DateAdmittedIntensive) < '2020-05-11'), ]
  BeredIntPasArt <- BeredIntPas[which(BeredIntPas$InnDato < '2020-05-11'), ]

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
      write.csv2(BeredIntPasArt, file, row.names = F, na = '')
    })

  var <- c("Alder","DischargedIntensivStatus","Graviditet", "Astma", "Diabetes" , "IsActivSmoker",
           "IsChronicLungDiseasePatient", "IsChronicNeurologicNeuromuscularPatient",
           "IsHeartDiseaseIncludingHypertensionPatient", "IsImpairedImmuneSystemIncludingHivPatient",
           "IsKidneyDiseaseIncludingFailurePatient", "IsLiverDiseaseIncludingFailurePatient",
           "IsObesePatient", "IsRiskFactor", "Kreft", "Bekreftet", "ReinnKval", "Reinn",
           "ReinnResp", "MechanicalRespirator", "MechanicalRespiratorEnd", "RespTid", "Liggetid",
           "ExtendedHemodynamicMonitoring", "Bilirubin", "BrainDamage", "Bukleie", "ChronicDiseases",
           "Diagnosis", "FrailtyIndex", "Glasgow", "Hco3", "HeartRate", "Impella", "Leukocytes",
           "MvOrCpap", "NEMS", "NonInvasivVentilation", "Potassium", "Saps2Score", "Saps2ScoreNumber",
           "SerumUreaOrBun", "Sodium", "SystolicBloodPressure", "Temperature", "Trakeostomi", "UrineOutput",
           "VasoactiveInfusion", "erMann", "ECMOTid", "Dod30")

  OppsumTab <- t(summary(BeredIntPasArt[,var]))

  output$lastNed_BeredIntOppsumTab <- downloadHandler(
    filename = function(){
      paste0('OppsumTab', Sys.Date(), '.csv')
    },
    content = function(file, filename){
      write.csv2(OppsumTab, file, row.names = T, na='')
    })




}
# Run the application
shinyApp(ui = ui, server = server)
