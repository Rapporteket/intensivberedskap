
koronafigurer_UI <- function(id, rhfNavn){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(id = ns('brukervalgStartside'),
                 width = 3,
                 h3('Gjør filtreringer/utvalg:'),
                 selectInput(inputId = ns("valgtRHF"), label="Velg RHF",
                             choices = rhfNavn
                 ),
                 selectInput(inputId = ns('valgtVar'), label='Velg variabel',
                             choices = c('Antall innleggelser'='antreg',
                                         # 'Antall døde'='antdod',
                                         'Antall utskrivinger'= 'antut',
                                         'Antall inneliggende'='antinn')
                 ),
                 selectInput(inputId = ns("velgTidsenhet"), label="Velg tidsenhet",
                             choices = c("Dag"="dag", "Uke"="uke", "Måned"="maaned")),
                 selectInput(inputId = ns("velgAntVisning"), label="Velg antall dager",
                             choices = c(10, 20, 30, 50, 100, 200), selected = 30),
                 selectInput(inputId = ns("bekr"), label="Bekreftet/Mistenkt",
                             choices = c("Alle"=9, "Bekreftet"=1, "Mistenkt"=0)
                 ),
                 selectInput(inputId = ns("skjemastatus"), label="Skjemastatus",
                             choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
                 ),
                 selectInput(inputId = ns("resp"), label="Respiratorbehandlet",
                             choices = c("Alle"=9, "Ja"=1, "Nei"=2)
                 ),
                 selectInput(inputId = ns("dodInt"), label="Tilstand ut fra intensiv",
                             choices = c("Alle"=9, "Død"=1, "Levende"=0)
                 ),
                 conditionalPanel(condition = paste0("input['", ns("figurer"), "'] == 'id_antpas'"),
                                  selectInput(inputId = ns("erMann"), label="Kjønn",
                                              choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
                                  )
                 ),
                 selectInput(inputId = ns("bildeformat"), label = "Velg format for nedlastet figur",
                             choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                 br(),
                 actionButton(ns("tilbakestillValg"), label="Tilbakestill valg")
    ),
    mainPanel(tabsetPanel(id=ns("figurer"),
                          tabPanel("Antall pasienter", value = "id_antpas",
                                   plotOutput(ns("FigurTidEnhet"), height="auto"),
                                   downloadButton(ns("LastNedFig"), label = 'Last ned figur'),
                                   br(),
                                   br(),
                                   # DT::DTOutput(ns("tabTidEnhet_DT")),
                                   tableOutput(ns("tabTidEnhet_plain")),
                                   downloadButton(ns("lastNed"), "Last ned tabell")
                          )
    )
    )
  )

}


koronafigurer <- function(input, output, session, rolle, CoroData, egetRHF, reshID){

  observeEvent(input$tilbakestillValg, {
    shinyjs::reset("brukervalgStartside")
  })

  if (rolle != 'SC') {
    updateSelectInput(session, "valgtRHF",
                      choices = unique(c('Alle', ifelse(egetRHF=='Ukjent', 'Alle',
                                                        egetRHF))))
  }

  observe(
    switch (input$velgTidsenhet,
            "dag" = updateSelectInput(session, "velgAntVisning", label="Velg antall dager",
                                      choices = c(10, 20, 30, 50, 100, 200), selected = 30),
            "uke" = updateSelectInput(session, "velgAntVisning", label="Velg antall uker",
                                      choices = c(4, 8, 12, 20, 40, 100), selected = 8),
            "maaned" = updateSelectInput(session, "velgAntVisning", label="Velg antall måneder",
                                         choices = c(2, 4, 8, 12, 20), selected = 4)
    )
  )

  datoFra <- reactive(datoFra <- switch (input$velgTidsenhet,
                                         "dag" = Sys.Date() - days(as.numeric(input$velgAntVisning)-1),
                                         "uke" = floor_date(Sys.Date() - weeks(as.numeric(input$velgAntVisning)-1), unit = 'week', week_start = 1),
                                         "maaned" = floor_date(Sys.Date() - months(as.numeric(input$velgAntVisning)-1), unit = 'month')
  )
  )

  AntTab <- function() {
    valgtRHF <- ifelse(rolle == 'SC', as.character(input$valgtRHF), egetRHF)
    # AntTab <- TabTidEnhet(RegData=CoroData, tidsenhet='dag',
    #                       valgtRHF= valgtRHF,
    #                       skjemastatus=as.numeric(input$skjemastatus),
    #                       resp=as.numeric(input$resp),
    #                       dodInt = as.numeric(input$dodInt),
    #                       bekr=as.numeric(input$bekr),
    #                       erMann=as.numeric(input$erMann)
    # )
    AntTab <- switch(input$valgtVar,
                     'antreg'= TabTidEnhet(RegData=CoroData,
                                           tidsenhet=input$velgTidsenhet,
                                           datoFra=datoFra(),
                                           valgtRHF= valgtRHF,
                                           skjemastatus=as.numeric(input$skjemastatus),
                                           resp=as.numeric(input$resp),
                                           dodInt = as.numeric(input$dodInt),
                                           bekr=as.numeric(input$bekr),
                                           erMann=as.numeric(input$erMann)),
                     'antut'= antallTidUtskrevneNIRberedskap(RegData=CoroData,
                                                           tidsenhet=input$velgTidsenhet,
                                                           datoFra=datoFra(),
                                                           valgtRHF= valgtRHF,
                                                           skjemastatus=as.numeric(input$skjemastatus),
                                                           resp=as.numeric(input$resp),
                                                           dodInt = as.numeric(input$dodInt),
                                                           bekr=as.numeric(input$bekr),
                                                           erMann=as.numeric(input$erMann)),
                     # 'antut'=antallTidUtskrevne(RegData=KoroData, tilgangsNivaa=rolle,
                     #                            valgtEnhet= egenEnhet, #nivå avgjort av rolle
                     #                            tidsenhet=input$velgTidsenhet,
                     #                            datoFra=datoFra(),
                     #                            aarsakInn = as.numeric(input$aarsakInnRes),
                     #                            skjemastatusInn=as.numeric(input$skjemastatusInnRes),
                     #                            erMann=as.numeric(input$erMannRes)),
                     'antinn'= antallTidInneliggendeBeredskap(RegData=CoroData,
                                                              tidsenhet=input$velgTidsenhet,
                                                              datoFra=datoFra(),
                                                              valgtRHF= valgtRHF,
                                                              skjemastatus=as.numeric(input$skjemastatus),
                                                              resp=as.numeric(input$resp),
                                                              dodInt = as.numeric(input$dodInt),
                                                              bekr=as.numeric(input$bekr),
                                                              erMann=as.numeric(input$erMann))
    )


    ant_skjema <- AntTab$Tab_tidy
    ant_skjema[-dim(ant_skjema)[1], ] <- ant_skjema[rev(1:(dim(ant_skjema)[1]-1)), ]
    sketch <- htmltools::withTags(table(
      DT::tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      DT::tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    AntTab$ant_skjema <- ant_skjema
    AntTab$sketch <- sketch
    AntTab
  }

  output$tabTidEnhet_DT = DT::renderDT(
    DT::datatable(AntTab()$ant_skjema[-dim(AntTab()$ant_skjema)[1], ],
                  container = AntTab()$sketch,
                  rownames = F,
                  options = list(pageLength = 40)
    )
  )

  output$tabTidEnhet_plain = renderTable(
    AntTab()$ant_skjema,
    digits = 0
  )

  output$FigurTidEnhet <- renderPlot({
    AntTab <- AntTab()
    if (rolle != 'SC' | input$valgtRHF != 'Alle') {
      AntTab$Tab_tidy <- AntTab$Tab_tidy[, -(dim(AntTab$Tab_tidy)[2]-1)]
    }
    intensivberedskap::FigTidEnhet(AntTab)
  }, width = 700, height = 700)


  output$LastNedFig <- downloadHandler(
    filename = function(){
      paste0('KoronaFigur', Sys.time(), '.', input$bildeformat)
    },

    content = function(file){
      AntTab <- AntTab()
      if (rolle != 'SC' | input$valgtRHF != 'Alle') {
        AntTab$Tab_tidy <- AntTab$Tab_tidy[, -(dim(AntTab$Tab_tidy)[2]-1)]
      }
      intensivberedskap::FigTidEnhet(AntTab, outfile = file)
    }
  )

  output$lastNed <- downloadHandler(
    filename = function(){
      paste0('KoronaTabell', Sys.time(), '.csv')
    },

    content = function(file){
      Tabell1 <- AntTab()$Tab_tidy
      write.csv2(Tabell1, file, row.names = F, fileEncoding = 'latin1')
    }
  )

}



