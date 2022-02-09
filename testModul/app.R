#Henter data:
library(intensivberedskap)
CoroData <- NIRPreprosessBeredsk(RegData = NIRberedskDataSQL(kobleInt = 0))


## client user interface function
ui <- shiny::fluidPage(
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      autoReportOrgInput("beredUts"),
      autoReportInput("beredUts")
    ),
    shiny::mainPanel(
      autoReportUI("beredUts")
    )
  )
)

## server function
server <- function(input, output, session) {

  ## make a list of organization names and numbers
  orgs <- c('Alle', as.character(sort(unique(CoroData$RHF))))
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
    #abonnementBeredsk(rnwFil, brukernavn='beredskap', reshID=0, valgtRHF = 'Alle'),
    InfluensaRapp = list(
      synopsis = "Influensarapport",
      fun = "abonnementBeredsk",
      paramNames = c('rnwFil', "valgtRHF"),
      paramValues = c('Koronarapport', 'Alle')
    )
  )

  org <- autoReportOrgServer("beredUts", orgs)

  # set reactive parameters overriding those in the reports list
  paramNames <- shiny::reactive("valgtRHF")
  paramValues <- shiny::reactive(org$value())

  #shiny::reactive(print(org))

  autoReportServer(
    id = "beredUts", registryName = "intensivberedskap", type = "dispatchment",
    org = org$value, paramNames = paramNames, paramValues = paramValues,
    reports = reports, orgs = orgs, eligible = TRUE
  )
}

# Run the application
shinyApp(ui = ui, server = server)
