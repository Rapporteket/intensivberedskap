
koronafigurer_UI <- function(id, rhfNavn){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(id = ns('brukervalgStartside'),
                 width = 3,
                 h3('Gjør filtreringer/utvalg:'),
                 selectInput(inputId = ns("valgtRHF"), label="Velg RHF",
                             choices = rhfNavn
                 ),
                 selectInput(inputId = ns("bekr"), label="Bekreftet/Mistenkt",
                             choices = c("Alle"=9, "Bekreftet"=1, "Mistenkt"=0)
                 ),
                 selectInput(inputId = ns("skjemastatus"), label="Skjemastatus",
                             choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
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
                                   DT::DTOutput(ns("tabTidEnhet_DT")),
                                   downloadButton(ns("lastNed"), "Last ned tabell")
                          ),
                          tabPanel("Aldersfordeling, kjønnsdelt",
                                   plotOutput(ns("FigurAldersfordeling"), height="auto"),
                                   br(),
                                   br(),
                                   tableOutput(ns("tabAlder"))
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


  AntTab <- function() {
    valgtRHF <- ifelse(rolle == 'SC', as.character(input$valgtRHF), egetRHF)
    AntTab <- TabTidEnhet(RegData=CoroData, tidsenhet='dag',
                          valgtRHF= valgtRHF,
                          skjemastatus=as.numeric(input$skjemastatus),
                          bekr=as.numeric(input$bekr),
                          erMann=as.numeric(input$erMann)
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

  output$FigurTidEnhet <- renderPlot({
    AntTab <- AntTab()
    if (rolle != 'SC' | input$valgtRHF != 'Alle') {
      AntTab$Tab_tidy <- AntTab$Tab_tidy[, -(dim(AntTab$Tab_tidy)[2]-1)]
    }
    intensivberedskap::FigTidEnhet(AntTab)
  }, width = 700, height = 700)


  output$LastNedFig <- downloadHandler(
    filename = function(){
      # paste0('KoronaFigur', Sys.time(), '.', input$bildeformat)
      paste0('KoronaFigur.', input$bildeformat)
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


  output$FigurAldersfordeling <- renderPlot({
    valgtRHF <- ifelse(rolle == 'SC', as.character(input$valgtRHF), egetRHF)
    intensivberedskap::FigFordelingKjonnsdelt(RegData = CoroData, valgtVar = 'Alder',
                                              valgtRHF= valgtRHF,
                                              skjemastatus=as.numeric(input$skjemastatus),
                                              bekr=as.numeric(input$bekr))
  }, width = 700, height = 700)




  # output$tabAlder<- renderTable({xtable::xtable()}, rownames = F, digits=0, spacing="xs")

  output$tabAlder <- function() {
    valgtRHF <- ifelse(rolle == 'SC', as.character(input$valgtRHF), egetRHF)
    Tabell <- intensivberedskap::FigFordelingKjonnsdelt(RegData = CoroData, valgtVar = 'Alder',
                                                        valgtRHF= valgtRHF,
                                                        skjemastatus=as.numeric(input$skjemastatus),
                                                        bekr=as.numeric(input$bekr))

    # names(Tabell) <- c('Kategori', 'Antall i kategori', 'Antall totalt', 'Andel (%)', 'Antall i kategori', 'Antall totalt', 'Andel (%)')
    Tabell %>% knitr::kable("html", digits = 0) %>%
      kable_styling("hover", full_width = F) %>%
      add_header_above(c("Kategori", "Antall" = (dim(Tabell)[2]-3), "Totalt" = 2))
  }




}



