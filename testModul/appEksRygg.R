
## client user interface function
ui <- shiny::fluidPage(
  shiny::sidebarLayout(
     shiny::sidebarPanel(
       h4('Lage abonnementslister for utsendinger'),
       uiOutput("reportUts"),
       uiOutput("freqUts"),
       uiOutput("ReshUts"),
       h5('E-postmottagere legges inn en og en. Trykk legg til e-postmottager for hver gang.
                           Når du har lagt til alle, trykker du på lag utsending. '),
       textInput("email", "Epostmottakere:"),
       uiOutput("editEmail"),
       htmlOutput("recipients"),
       tags$hr(),
     ),


    shiny::mainPanel(
   #   autoReportUI("test")
      h3('Oversikt over aktive utsendinger'),
      uiOutput("dispatchmentContent")
    ))
)


## server function
server <- function(input, output, session) {
  #----- Utsending -----------------
  ## reaktive verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  dispatchment <- reactiveValues(
    tab = rapbase::makeAutoReportTab(session = session, type = "dispatchment"),
    report = "RyggMndRapp",
    freq = "Månedlig-month",
    email = vector()
  )
  ## observér og foreta endringer mens applikasjonen kjører
  observeEvent(input$addEmail, {
    dispatchment$email <- c(dispatchment$email, input$email)
  })
  observeEvent(input$delEmail, {
    dispatchment$email <-
      dispatchment$email[!dispatchment$email == input$email]
  })
  observeEvent (input$dispatch, {
    package <- "rygg"
    type <- "dispatchment"
    owner <- rapbase::getUserName(session)
    ownerName <- rapbase::getUserFullName(session)
    interval <- strsplit(input$dispatchmentFreq, "-")[[1]][2]
    intervalName <- strsplit(input$dispatchmentFreq, "-")[[1]][1]
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = interval)

    email <- dispatchment$email

    if (input$dispatchmentRep == "Kvartalsrapport") {
      synopsis <- "Kvartalsrapport, Rygg"
      fun <- "abonnementRygg"
      rnwFil <- "RyggMndRapp.Rnw" #Navn på fila
      reshIDuts <- input$dispatchmentResh
      organization <- reshIDuts #rapbase::getUserReshId(session)
      #print(reshIDuts)
      indReshUts <- match(reshIDuts, RegData$ReshId) #Velger sykehusresh
      paramNames <- c('rnwFil', 'brukernavn', "reshID")
      paramValues <- c(rnwFil, brukernavn, reshIDuts)
    }

    rapbase::createAutoReport(synopsis = synopsis, package = package,
                              type = type, fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              ownerName = ownerName,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear,
                              interval = interval, intervalName = intervalName)
    dispatchment$tab <- rapbase::makeAutoReportTab(session, type = "dispatchment")
    test <- dimnames(dispatchment$tab)
    # print(test[[]])
    # print(attributes(dispatchment$tab))
    #Author DataFlair

    alleAutorapporter <- rapbase::readAutoReportData()
    egneUts <-  rapbase::filterAutoRep(
      rapbase::filterAutoRep(alleAutorapporter, by = 'package', pass = 'rygg'),
      by = 'type', pass = 'dispatchment')

    dispatchment$email <- vector()
  })


  ## ui: velg rapport
  output$reportUts <- renderUI({
    selectInput("dispatchmentRep", "Rapport:",
                c("Kvartalsrapport"),
                selected = dispatchment$report)
  })
  ## ui: velg enhet
  output$ReshUts <- renderUI({
    selectInput("dispatchmentResh", "Avdelingstilhørighet:",
                sykehusValg[-1],
                selected = dispatchment$Resh)
  })

  ## ui: velg frekvens
  output$freqUts <- renderUI({
    selectInput("dispatchmentFreq", "Frekvens:",
                list(Årlig = "Årlig-year",
                      Kvartalsvis = "Kvartalsvis-quarter",
                      Månedlig = "Månedlig-month",
                      Ukentlig = "Ukentlig-week",
                      Daglig = "Daglig-DSTday"),
                selected = dispatchment$freq)
  })

  ## ui: legg til gyldig- og slett epost
  output$editEmail <- renderUI({
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
               input$email)) {
      tags$p("Angi mottaker over")
    } else {
      if (input$email %in% dispatchment$email) {
        actionButton("delEmail", "Slett epostmottaker",
                     icon = shiny::icon("trash"))
      } else {
        actionButton("addEmail", "Legg til epostmottaker",
                     icon = shiny::icon("pencil"))
      }
    }
  })

  ## ui: vis valgte mottakere
  output$recipients <- renderText(paste(dispatchment$email, sep = "<br>"))

  ## ui: lag ny utsending
  output$makeDispatchment <- renderUI({
    if (length(dispatchment$email) == 0) {
      NULL
    } else {
      actionButton("dispatch", "Lag utsending",
                   icon = shiny::icon("save"))
    }
  })

  ## lag tabell over gjeldende status for utsending
  output$activeDispatchments <- DT::renderDataTable(
    dispatchment$tab, server = FALSE, escape = FALSE, selection = 'none',
    options = list(dom = 'tp', ordning = FALSE, columnDefs = list(list(visible = FALSE, targets = 9))),
    rownames = FALSE
  )

  ## ui: lag side som viser status for utsending, også når det ikke finnes noen
  output$dispatchmentContent <- renderUI({
    if (length(dispatchment$tab) == 0) {
      p("Det finnes ingen utsendinger")
    } else {
      tagList(
        h4("Aktive utsendinger:"),
        h5("Når du trykker på knappen for å gjøre endringer i ei utsending,
           slettes utsendinga fra lista og alle valg UNNTATT sykehustilhørighet/resh legger seg inn i skjemaet til venstre
           slik at du f.eks. kan legge til/slette e-postmottagere og endre frekvens."),
        DT::dataTableOutput("activeDispatchments")
      )
    }
  })

  # Rediger eksisterende auto rapport (alle typer)
  observeEvent(input$edit_button, {
    repId <- strsplit(input$edit_button, "_")[[1]][2]
    rep <- rapbase::readAutoReportData()[[repId]]

    dispatchment$freq <- paste0(rep$intervalName, "-", rep$interval)
      dispatchment$email <- rep$email
      rapbase::deleteAutoReport(repId)
      dispatchment$tab <-
        rapbase::makeAutoReportTab(session, type = "dispatchment")
      dispatchment$report <- rep$synopsis
  })


  # Slett eksisterende auto rapport (alle typer)
  observeEvent(input$del_button, {
    repId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(repId)
    subscription$tab <-
      rapbase::makeAutoReportTab(session, type = "subscription")
    dispatchment$tab <-
      rapbase::makeAutoReportTab(session, type = "dispatchment")
  })


}

# Run the application
shinyApp(ui = ui, server = server)
