library(shiny)
library(shinyjs)

Sys.setenv(TZ = "Europe/Copenhagen")

ui <- shiny::fluidPage(
  title = "Udskriv arbejdsplan",
  shiny::tags$head(
    shiny::tags$link(
      href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;600&display=swap",
      rel = "stylesheet"
    ),
    shiny::tags$style(HTML("
      body { font-family: 'Open Sans', sans-serif; padding: 1em; line-height: 1.6; }
      h2   { color: #2c3e50; }
      label { font-weight: 600; }
      .well { background: #f8f9fa; border-radius: 8px; padding: 1em;
              box-shadow: 0 1px 3px rgba(0,0,0,.05); margin-bottom: 1em; }
      #calendar { margin-top: 1em; }
    ")),
    # In ui — add to tags$head:
    shinyjs::useShinyjs(),
    shiny::tags$style(HTML("
  body { font-family: 'Open Sans', sans-serif; padding: 1em; line-height: 1.6; }
  h2   { color: #2c3e50; }
  label { font-weight: 600; }
  .well { background: #f8f9fa; border-radius: 8px; padding: 1em;
          box-shadow: 0 1px 3px rgba(0,0,0,.05); margin-bottom: 1em; }
  #calendar { margin-top: 1em; transition: opacity 0.2s; }

  /* Dimming + spinner overlay when recalculating */
  #calendar.recalculating {
    opacity: 0.3;
    pointer-events: none;
  }
  .plot-spinner {
    display: none;
    text-align: center;
    color: #2c3e50;
    font-style: italic;
    margin: 0.5em 0;
  }
  .plot-spinner.visible { display: block; }
"))
  ),

  shiny::h2("Arbejdsoversigt fra delt .ics-fil"),

  shiny::wellPanel(
    shiny::p(
      tags$b("Tip:"), "Statisk visning — husk at tjekke for opdateringer i din kalender. ",
      "Lavet af Andreas — se mere på ",
      shiny::a("GitHub", href = "https://github.com/agdamsbo/PrintSchedule",
               target = "_blank", rel = "noopener noreferrer"), ".",
      tags$br(),
      tags$small("Brug på eget ansvar. Programmet leveres uden garanti.")
    )
  ),

  shiny::textInput(
    inputId     = "link",
    label       = "Kalenderlink:",
    placeholder = "https://minplan.rm.dk/SharedCalendar/11248--yg6u0EBIE5.ics",
    width       = "100%"
  ),

  shiny::conditionalPanel(
    condition = "input.link.trim() !== ''",

    colorSelectInput(
      inputId  = "color_palette",
      label    = "Farvetema:",
      choices  = list(
        "Regnbue"                     = "Spectral",
        "Kraftig og tydelig"          = "Dark2",
        "Klar og levende"             = "Set1",
        "Blød og dæmpet"              = "Set2",
        "Pastel"                      = "Pastel1",
        "Farveblind-venlig"           = "Okabe-Ito",
        "Mange kategorier (varieret)" = "Alphabet",
        "Rød, gul, grøn"              = "RdYlGn"
      ),
      previews = 6
    ),

    shiny::fluidRow(
      shiny::column(4,
                    shiny::numericInput("length", "Antal dage:", min = 7, max = 365, value = 30)
      ),
      shiny::column(4,
                    shiny::uiOutput("event_types")
      ),
      shiny::column(4,
                    shiny::radioButtons(
                      "week_month", "Vis som:",
                      choices = c("Uger" = "week", "Måneder" = "month"),
                      inline  = TRUE
                    )
      )
    ),

    shiny::textOutput("event_text"),
    shiny::div(id = "plot_spinner", class = "plot-spinner",
               "⏳ Opretter arbejdsplan…"),
    shiny::plotOutput("calendar", height = "60vh"),
    shiny::downloadButton("download", "Hent som PDF (A4)")
  )
)


server <- function(input, output, session) {

  # Debounce raw link input — waits 600 ms after user stops typing
  link_d <- shiny::debounce(shiny::reactive(input$link), 1200)

  length_d <- shiny::debounce(shiny::reactive(input$length), 1200)

  # Single reactive: parse ICS once; NULL on bad/empty link
  ics_data <- shiny::reactive({
    lnk <- link_d()
    shiny::req(nzchar(trimws(lnk)))
    if (!isTRUE(validate_ics_link(lnk))) {
      shiny::showNotification("Tjek at du har indsat et korrekt link.", type = "error")
      return(NULL)
    }
    read_ics(lnk)
  })

  # Populate event-type selector whenever data changes
  output$event_types <- shiny::renderUI({
    dat <- ics_data()
    shiny::req(dat)
    opts     <- unique(dat[["SUMMARY"]])
    defaults <- c("Arbejde", "Kursus", "Administration", "Forskning")
    shiny::selectInput(
      "event_types", "Begivenhedstyper:",
      choices  = opts,
      selected = defaults[defaults %in% opts],
      multiple = TRUE
    )
  })

  # Summary text — derived from ics_data, no separate observeEvent needed
  output$event_text <- shiny::renderText({
    dat <- ics_data()
    shiny::req(dat)
    today          <- Sys.Date()
    first_day <- as.Date(min(dat[[find_col(dat, "DTSTART")]], na.rm = TRUE), tz = "Europe/Copenhagen")
    last_day  <- as.Date(max(dat[[find_col(dat, "DTEND")]],   na.rm = TRUE), tz = "Europe/Copenhagen")
    days_remaining <- as.numeric(as.Date(last_day) - as.Date(today))
    glue::glue(
      "Kalenderen indeholder {nrow(dat)} begivenheder fra ",
      "{format(first_day, '%d. %b')} til {format(last_day, '%d. %b %Y')} ",
      "({days_remaining} dage frem)."
    )
  })

  # Filtered + categorised data — only recomputes when selection changes
  filtered_data <- shiny::reactive({
    dat  <- ics_data()
    shiny::req(dat, input$event_types)
    dat |>
      dplyr::filter(SUMMARY %in% input$event_types) |>
      categorise_events()
  })

  # Plot — only rerenders when filtered data or display options change
  plot_obj <- shiny::reactive({
    shiny::req(filtered_data(), input$event_types, length_d())
    filtered_data() |>
      print_events(
        length  = length_d(),
        palette = input$color_palette,
        type    = input$week_month
      )
  })

  # Observe all plot-triggering inputs and show spinner immediately
  shiny::observe({
    # Take a dependency on all inputs that affect the plot
    input$event_types
    input$color_palette
    input$week_month
    length_d()
    link_d()

    # Only show spinner if we actually have data
    if (!is.null(ics_data())) {
      shinyjs::addClass("calendar", "recalculating")
      shinyjs::addClass("plot_spinner", "visible")
    }
  })

  output$calendar <- shiny::renderPlot({
    p <- plot_obj()
    # Remove spinner — this runs after the plot is built
    shinyjs::removeClass("calendar", "recalculating")
    shinyjs::removeClass("plot_spinner", "visible")
    print(p)
  })

  output$download <- shiny::downloadHandler(
    filename = function() "arbejdsplan.pdf",
    content  = function(file) {
      ggplot2::ggsave(plot_obj(), filename = file,
                      height = 210, width = 297, units = "mm")
    }
  )
}

shiny::shinyApp(ui, server)
