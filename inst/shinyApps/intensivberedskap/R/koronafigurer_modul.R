
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
                 selectInput(inputId = ns("erMann"), label="Kjønn",
                             choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
                 ),
                 # h4('Kun for risikofaktorer:'),
                 # sliderInput(inputId=ns("alder"), label = "Alder",
                 #             min = 0, max = 110,
                 #             value = c(0, 110),
                 #             step = 10
                 # ),
                 br(),
                 actionButton(ns("tilbakestillValg"), label="Tilbakestill valg")
    ),
    mainPanel(
      # h3('Her kommer figur'),
      # tableOutput(ns('tabTidEnhet')),
      plotOutput(ns("FigurTidEnhet"), height="auto"),
      br(),
      DT::DTOutput(ns("tabTidEnhet_DT"))
              ) #main
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
    # list(ant_skjema=ant_skjema, sketch=sketch)
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


}



