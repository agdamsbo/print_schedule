# This app is run via PrintSchedule::run_app()
# It relies on functions from the PrintSchedule package namespace

library(shiny)
library(dplyr)

Sys.setenv(TZ = "Europe/Copenhagen")

############ APP

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::HTML(
      '<script defer src="https://stats.freesearchr.org/script.js" data-website-id="eb207a4c-6ab3-495e-8c91-ea2eabd5da3b"></script>'
    )
  ),
  shiny::titlePanel("Arbejdsoversigt fra delt .ics-fil"),
  shiny::p(
    "Udskriv oversigt over arbejdstider for de næste dage ud fra det delte kalender link fra MyPlan"
  ),
  shiny::p(
      "Husk at når du printer kalenderen får du et øjebliksbillede. Det kan være rart, men du skal være opmærksom på ændringer der kommer til."
  ),
  shiny::p(
    paste(
      "Værktøjet her er lavet af mig, Andreas. Jeg er læge og forsker og bliver let frustreret over dårlig software. Læs mere om programmet og find mine kontaktoplysninger her: "
    ),
    shiny::a("https://github.com/agdamsbo/print_schedule/", href = "https://github.com/agdamsbo/print_schedule/")
  ),
  shiny::textInput(
    inputId = "link",
    label = "Indsæt kalenderlink:",
    placeholder = "https://minplan.rm.dk/SharedCalendar/11248--yg6u0EBIE5.ics"
  ),
  shiny::conditionalPanel(
    condition = "input.link.trim() !== ''",
    colorSelectInput(
      inputId = "color_palette",
      label = "Vælg farvekort",
      choices = c(
        "Regnbue"                      = "Spectral",
        "Kraftig og tydelig"           = "Dark2",
        "Klar og levende"              = "Set1",
        "Blød og dæmpet"               = "Set2",
        "Pastel"                       = "Pastel1",
        "Farveblind-venlig"            = "Okabe-Ito",
        "Mange kategorier (varieret)"  = "Alphabet",
        "Rød, gul, grøn"               = "RdYlGn"
      ),
      previews = 6
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::numericInput(
          inputId = "length",
          label = "Dage til udskrift:",
          min = 7,
          max = 150,
          value = 30
        )
      ),
      shiny::column(width = 4, shiny::uiOutput("event_types")),
      shiny::column(
        width = 4,
        shiny::radioButtons(
          inputId = "week_month",
          label = "Uge- eller månedsplan",
          choices = c(Uge = "week", Måned = "month"),
          inline = TRUE
        )
      )
    ),
    shiny::h3("Kalender (når klar):"),
    shiny::plotOutput("calendar", height = "60vh"),
    shiny::downloadButton(outputId = "download", label = "Hent A4"),
    shiny::br(),
    shiny::br(),
    shiny::p(
      "Jeg har naturligvis gjort mig umage, men jeg tager intet ansvar for programmets indhold eller funktion, og ved at bruge det accepterer du naturligvis at det er dit eget ansvar at møde til tiden."
    ),
  )

)

server <- function(input, output) {
  rv <- shiny::reactiveValues(data = NULL,
                              data_filter = NULL,
                              p = NULL)

  shiny::observeEvent(list(input$file1, input$link), {
    if (!is.null(input$file1)) {
      rv$data <- calendar::ic_read(input$file1$datapath)
    } else if (!is.null(input$link) && input$link != "") {
      if (isTRUE(validate_ics_link(input$link))) {
        rv$data <- calendar::ic_read(input$link)
      } else {
        shiny::showNotification("Tjek lige at du har indsat et korrekt link.", type = "error")
        rv$data <- NULL
      }
    }
  })

  shiny::observeEvent(rv$data, {
    req(rv$data)
    options <- unique(rv$data[["SUMMARY"]])
    defaults <- c("Arbejde", "Kursus", "Administration", "Forskning")

    output$event_types <- shiny::renderUI(
      shiny::selectInput(
        inputId = "event_types",
        label = "Vælg typer, der skal på kalenderen:",
        choices = options,
        selected = defaults[defaults %in% options],
        multiple = TRUE
      )
    )
  })

  shiny::observeEvent(input$event_types, {
    req(input$event_types)

    rv$data_filter <- rv$data |>
      dplyr::filter(SUMMARY %in% input$event_types) |>
      categorise_events()

  })

  output$calendar <- shiny::renderPlot({
    req(rv$data_filter)

    rv$p <- rv$data_filter |> print_events(
      length = input$length,
      palette = input$color_palette,
      type = input$week_month
    )
    rv$p
  })

  output$download <- shiny::downloadHandler(
    filename = function() {
      "schedule.pdf"
    },
    content = function(file) {
      p <- rv$p
      papersize <-  "A4"
      orientation <- "l"

      switch (
        papersize,
        A6 = {
          a <- 148
          b <- 105

        },
        A5 = {
          a <- 210
          b <- 148

        },
        A4 = {
          a <- 297
          b <- 210
        },
        A3 = {
          a <- 420
          b <- 297
        },
        A2 = {
          a <- 594
          b <- 420
        },
        A1 = {
          a <- 841
          b <- 594
        },
        A0 = {
          a <- 1189
          b <- 841
        },
      )

      if (orientation == "l") {
        ggplot2::ggsave(
          plot = p,
          filename = file,
          height = b,
          width = a,
          units = "mm"
        )
      } else {
        ggplot2::ggsave(
          plot = p,
          filename = file,
          width = b,
          height = a,
          units = "mm"
        )
      }
    }
  )

}

# Run the application
shiny::shinyApp(ui = ui, server = server)
