
koronafigurer_UI <- function(id, rhfNavn){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(id = ns('brukervalgStartside'),
                 width = 3,
                 # tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
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
                 h4('Kun for risikofaktorer:'),
                 sliderInput(inputId=ns("alder"), label = "Alder",
                             min = 0, max = 110,
                             value = c(0, 110),
                             step = 10
                 ),
                 br(),
                 actionButton(ns("tilbakestillValg"), label="Tilbakestill valg")
    ),
    mainPanel(
      h3('Her kommer figur')
              ) #main
  )

}


koronafigurer <- function(input, output, session){

  observeEvent(input$tilbakestillValg, {
    shinyjs::reset("id_shus_panel")
  })



}



