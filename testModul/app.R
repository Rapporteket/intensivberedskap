#Henter data:
library(intensivberedskap)
CoroData <- NIRPreprosessBeredsk(RegData = NIRberedskDataSQL(kobleInt = 0))

## make a list of organization names and numbers
orgs <- c('Alle', as.character(sort(unique(CoroData$RHF))))

# orgs <- list( #
#   OrgOne = 111111,
#   OrgTwo = 222222
# )


## make a list for report metadata
# reports <- list(
#   FirstReport = list(
#     synopsis = "First example report",
#     fun = "fun1", #Må fortsatt skrives. Ta mal fra abonnement. Skrives slik at den selv tar høyde for ekstra brukervalg som RHF. Må genereres ulike moduler
#     #eller innebygges i andre parametre, eks. valg av rapport. Modulen kan bare bruke parametre som er håndtert i modulen
#     paramNames = c("organization", "topic", "outputFormat"), #Kan ha så mange parametre man ønsker, men de er faste. Kun org og format er reaktive
#     paramValues = c(111111, "work", "html")
#   ),
#   SecondReport = list(
#     synopsis = "Second example report",
#     fun = "fun2",
#     paramNames = c("organization", "topic", "outputFormat"),
#     paramValues = c(111111, "leisure", "pdf")
#   )
# )
reports <- list(
  CovidRapp = list(
    synopsis = "Resultater, Covid-19",
    fun = "abonnementBeredsk", #DENNE MÅ SKRIVES SOM FØR
    paramNames <- c('rnwFil', "valgtRHF"),
    paramValues <- c('Koronarapport', 'Alle') #valgtRHF) #
  ),
  #abonnementBeredsk(rnwFil, brukernavn='beredskap', reshID=0, valgtRHF = 'Alle'),
  InfluensaRapp = list(
    synopsis = "Influensarapport",
    fun = "abonnementBeredsk",
    paramNames <- c('rnwFil', "valgtRHF"),
    paramValues <- c('Koronarapport', 'Alle') #valgtRHF) #
  ),
  SecondReport = list(
    synopsis = "Influensarapport",
    fun = "fun2",
    paramNames = c("organization", "topic", "outputFormat"),
    paramValues = c(700720, "leisure", "pdf")
  )
)

## client user interface function
ui <- shiny::fluidPage(
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      autoReportFormatInput("test"),
      selectInput(inputId = "valgtRHFsub", label="Velg RHF",
                  choices = orgs
      ),
      #autoReportOrgInput("test"), #Kan definere det slik at RHF benyttes
      autoReportInput("test")
    ),
    shiny::mainPanel(
      autoReportUI("test")
    )
  )
)

## server function
server <- function(input, output, session) {
#  org <- autoReportOrgServer("test", orgs) #Bytt ut test med aktuelt navn for utsending, beredsk.
  org <- shiny::reactive(as.character(input$valgtRHFsub))
  format <- autoReportFormatServer("test")

  # set reactive parameters overriding those in the reports list
  paramNames <- shiny::reactive("organization") #Hvorfor reaktiv når verdien statisk..
  #paramValues <- shiny::reactive(c(org$value(), format()))
  #paramValues <- shiny::reactive(c(input$valgtRHFsub()))
  paramValues <- shiny::reactive(as.character(input$valgtRHFsub))

  autoReportServer(
    id = "test", registryName = "rapbase", type = "dispatchment",
    org = org, paramNames = paramNames, paramValues = paramValues,
    #org = org$value, paramNames = paramNames, paramValues = paramValues,
    reports = reports, orgs = orgs, eligible = TRUE
  )
}

# Run the application
shinyApp(ui = ui, server = server)
