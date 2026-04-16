library(shiny)
library(calendar)
library(lubridate)
library(dplyr)

# source(here::here("R/calendr_refactored.R"))
Sys.setenv(TZ = "Europe/Copenhagen")


###### Functions
validate_ics_link <- function(link) {
  # Check if the link ends with .ics (case-insensitive)
  grepl("\\.ics$", link, ignore.case = TRUE)
}

categorise_events <- function(df, tz = "Europe/Copenhagen") {
  # Detect all-day events: start at 00:00:00 and end at 00:00:00 the next day
  # (iCal all-day convention: DTSTART = date 00:00, DTEND = next date 00:00)
  is_allday <- format(df$DTSTART, "%H:%M:%S") == "00:00:00" &
    format(df$DTEND, "%H:%M:%S") == "00:00:00" &
    as.Date(df$DTEND) > as.Date(df$DTSTART)

  if (isTRUE(any(is_allday))) {
    allday <- df[is_allday, ]

    expanded <- do.call(rbind, lapply(seq_len(nrow(allday)), function(i) {
      row     <- allday[i, ]
      start_d <- as.Date(format(row$DTSTART, "%Y-%m-%d", tz = tz))
      end_d   <- as.Date(format(row$DTEND, "%Y-%m-%d", tz = tz)) - 1
      days    <- seq(start_d, end_d, by = "day")
      n       <- length(days)
      rep_row <- row[rep(1, n), ]
      rep_row$DTSTART <- as.POSIXct(paste(days, "00:00:01"), tz = tz)
      rep_row$DTEND   <- as.POSIXct(paste(days, "23:59:59"), tz = tz)
      rep_row
    }))

    df <- rbind(df[!is_allday, ], expanded)
  }

  df <- df |>
    dplyr::mutate(
      StartTime = format(DTSTART, "%H:%M"),
      EndTime   = format(DTEND, "%H:%M"),
      StretchesToNextDay = as.Date(DTEND) > as.Date(DTSTART),
      EventType = ifelse(
        StretchesToNextDay,
        paste0(StartTime, "-", EndTime, " (Nattevagt)"),
        paste0(StartTime, "-", EndTime)
      ),
      # This ensures compability with other categories than work
      EventType = ifelse(
        SUMMARY != "Arbejde",
        glue::glue("{EventType} ({SUMMARY})"),
        EventType
      ),
      EventType = ifelse(StartTime == "00:00" &
                           EndTime == "23:59", SUMMARY, EventType)
    )

  return(df)
}

print_events <- function(data,
                         start = Sys.Date(),
                         length = NULL,
                         start_col = "start_date",
                         cat_col = "EventType",
                         palette = "Spectral") {
  df_cat_min <- data |>
    dplyr::mutate(start_date = as.Date(format(DTSTART, "%Y-%m-%d"))) |>
    dplyr::select(
      tidyselect::all_of(c(start_col, cat_col))
      # start_date,
      # # DTSTART,
      # # DTEND,
      # # StretchesToNextDay,
      # EventType
    )

  date_start <- start
  if (is.null(length)) {
    date_end <- max(df_cat_min$start_date)
  } else if (is.numeric(length)) {
    if (length > 150)
      length <- 150
    date_end <- date_start + lubridate::days(length)
  } else {
    date_end <- date_start + lubridate::days(30)
  }

  # browser()


  # df_print <- df_cat_min |>
  #   dplyr::rename(date = !!start_col) |>
  #   dplyr::filter(date >= date_start &
  #                   date <= date_end)


  df_print <- dplyr::full_join(data.frame(date = as.Date(seq(
    date_start, date_end, by = 1
  ))),
  df_cat_min,
  by = dplyr::join_by(date == !!start_col)) |>
    dplyr::filter(date >= date_start &
                    date <= date_end)

  # browser()
  calendR(
      from = date_start,
      to = date_end,
      view = "month",
      title = glue::glue(
          "Schedule ({as.character(date_start)} to {as.character(date_end)})"
      ),
      subtitle = paste("Printed on", date_start),
      week.number = TRUE,
      start = "M",
      legend.pos = "bottom",
      events = dplyr::rename(df_print,label=!!cat_col) |> dplyr::mutate(category=label),
      palette = setNames(generate_colors(n = length(unique(
        na.omit(df_print[[cat_col]])
      )), palette = palette),unique(
        na.omit(df_print[[cat_col]])
      ))
  )

  # calendR::calendR(
  #   from = date_start,
  #   to = date_end,
  #   # view = "month",
  #   title = glue::glue(
  #     "Schedule ({as.character(date_start)} to {as.character(date_end)})"
  #   ),
  #   subtitle = paste("Printed on", date_start),
  #   # text = labels,
  #   # text.pos = 1:30,
  #   week.number = TRUE,
  #   # year = lubridate::year(start),
  #   # month = lubridate::month(start),
  #   start = "M",
  #   legend.pos = "bottom",
  #   text.size = 12,
  #   # events = dplyr::rename(df_print,label=!!cat_col) |> dplyr::mutate(category=label),
  #   special.days = df_print[[cat_col]],
  #   # palette = setNames(generate_colors(n = length(unique(
  #   #   na.omit(df_print[[cat_col]])
  #   # )), palette = palette),unique(
  #   #   na.omit(df_print[[cat_col]])
  #   # ))
  #   special.col = generate_colors(n = length(unique(
  #     na.omit(df_print[[cat_col]])
  #   )), palette = palette)
  #   # RColorBrewer::brewer.pal(, "Spectral")
  # )
}

ggplot2pdf <- function(p,
                       doc_name = "",
                       papersize = "A4",
                       orientation = c("l", "h")) {
  orientation <- match.arg(orientation)

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


  if (doc_name == "") {
    doc_name <- "schedule.pdf"
  } else {
    doc_name <- paste0(doc_name, ".pdf")
  }

  if (orientation == "l") {
    ggplot2::ggsave(
      plot = p,
      filename = doc_name,
      height = b,
      width = a,
      units = "mm"
    )
  } else {
    ggplot2::ggsave(
      plot = p,
      filename = doc_name,
      width = b,
      height = a,
      units = "mm"
    )
  }
}

## Colors

generate_colors <- function(n, palette = "viridis", ...) {
  # --- Input validation -------------------------------------------------------
  if (!is.numeric(n) || length(n) != 1 || n < 1 || n %% 1 != 0) {
    stop("`n` must be a single positive integer.")
  }
  if (!is.function(palette) &&
      (!is.character(palette) || length(palette) != 1)) {
    stop("`palette` must be a single character string or a function.")
  }

  # --- Function passthrough ---------------------------------------------------
  if (is.function(palette)) {
    return(palette(n, ...))
  }

  # --- Named palette dispatch -------------------------------------------------
  palette_lower <- tolower(palette)

  viridis_palettes <- c("viridis",
                        "magma",
                        "plasma",
                        "inferno",
                        "cividis",
                        "mako",
                        "rocket",
                        "turbo")

  if (palette_lower %in% viridis_palettes) {
    viridisLite::viridis(n = n, option = palette_lower, ...)

  } else if (palette_lower == "hcl") {
    grDevices::hcl.colors(n = n, ...)

  } else if (palette_lower == "rainbow") {
    grDevices::rainbow(n = n, ...)

  } else if (palette_lower == "heat") {
    grDevices::heat.colors(n = n, ...)

  } else if (palette_lower == "terrain") {
    grDevices::terrain.colors(n = n, ...)

  } else if (palette_lower == "topo") {
    grDevices::topo.colors(n = n, ...)

  } else {
    # Case-insensitive RColorBrewer lookup
    brewer_names <- rownames(RColorBrewer::brewer.pal.info)
    brewer_match <- brewer_names[match(palette_lower, tolower(brewer_names))]

    if (!is.na(brewer_match)) {
      max_n       <- RColorBrewer::brewer.pal.info[brewer_match, "maxcolors"]
      fetch_n     <- max(min(n, max_n), 3L)
      base_colors <- RColorBrewer::brewer.pal(n = fetch_n, name = brewer_match)
      grDevices::colorRampPalette(base_colors)(n)

    } else {
      # Case-insensitive grDevices palette.pals() lookup
      pal_names <- grDevices::palette.pals()
      pal_match <- pal_names[match(palette_lower, tolower(pal_names))]

      if (!is.na(pal_match)) {
        grDevices::colorRampPalette(grDevices::palette.colors(palette = pal_match))(n)

      } else if (palette %in% grDevices::hcl.pals()) {
        # Named HCL palettes (e.g. "Rocket", "Plasma") — distinct from viridisLite
        grDevices::hcl.colors(n = n, palette = palette, ...)

      } else {
        warning(
          "Unknown palette: '",
          palette,
          "'. Falling back to viridis.\n",
          "Available options:\n",
          "  viridisLite  : viridis, magma, plasma, inferno, cividis, mako, rocket, turbo\n",
          "  grDevices    : hcl, rainbow, heat, terrain, topo\n",
          "  grDevices HCL: use grDevices::hcl.pals() to see all options\n",
          "  grDevices    : use grDevices::palette.pals() to see all options\n",
          "  RColorBrewer : use RColorBrewer::brewer.pal.info to see all options"
        )
        viridisLite::viridis(n = n, option = "viridis")
      }
    }
  }
}


colorSelectInput <- function(inputId,
                             label,
                             choices,
                             selected = NULL,
                             previews = 4,
                             ...,
                             placeholder = "") {
  vals <- if (shiny::is.reactive(choices)) {
    choices()
  } else{
    choices
  }

  swatch_html <- function(palette_name) {
    colors <- tryCatch(
      suppressMessages(generate_colors(previews, palette_name)),
      error = function(e)
        rep("#cccccc", 3)
    )
    # Strip alpha channel to ensure valid 6-digit CSS hex
    colors <- substr(colors, 1, 7)
    paste0(
      sprintf(
        "<span style='display:inline-block;width:12px;height:12px;background:%s;border-radius:2px;margin-right:1px;'></span>",
        colors
      ),
      collapse = ""
    )
  }



  labels <- sprintf(
    '{"name": "%s", "label": "%s", "swatch": "%s"}',
    vals,
    names(vals) %||% "",
    vapply(vals, swatch_html, character(1))
  )

  choices_new <- stats::setNames(vals, labels)

  if (is.null(selected) || selected == "") {
    selected <- vals[[1]]
  }

  shiny::selectizeInput(
    inputId  = inputId,
    label    = label,
    choices  = choices_new,
    selected = selected,
    ...,
    options = list(
      render = I(
        "{
    option: function(item, escape) {
      item.data = JSON.parse(item.label);
      return '<div style=\"padding:3px 12px\">' +
               '<div><strong>' + escape(item.data.name) + '</strong></div>' +
               (item.data.label != '' ? '<div><small>' + escape(item.data.label) + '</small></div>' : '') +
               '<div style=\"margin-top:4px\">' + item.data.swatch + '</div>' +
             '</div>';
    },
    item: function(item, escape) {
      item.data = JSON.parse(item.label);
      return '<div style=\"display:flex;align-items:center;gap:6px\">' +
               '<span>' + escape(item.data.name) + '</span>' +
               item.data.swatch +
             '</div>';
    }
  }"
      ),
      onInitialize = I(
        "function() {
    var self = this;
    self.$control_input.prop('readonly', true);
    self.$control_input.css('cursor', 'default');
    self.$control.css('cursor', 'pointer');
  }"
      )
    )
  )
}



############ APP

ui <- fluidPage(
  shiny::tags$head(shiny::HTML('<script defer src="https://stats.freesearchr.org/script.js" data-website-id="eb207a4c-6ab3-495e-8c91-ea2eabd5da3b"></script>')),
  titlePanel("Arbejdsoversigt fra delt .ics-fil"),
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
    a("https://github.com/agdamsbo/print_schedule/", href = "https://github.com/agdamsbo/print_schedule/")
  ),
  textInput(
    inputId = "link",
    label = "Indsæt kalenderlink:",
    placeholder = "https://minplan.rm.dk/SharedCalendar/11248--yg6u0EBIE5.ics"
  ),
  conditionalPanel(
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
        # "Parrede farver"               = "Paired",
        "Farveblind-venlig"            = "Okabe-Ito",
        "Mange kategorier (varieret)"  = "Alphabet",
        # "Mange kategorier (rig)"       = "Polychrome 36",
        "Rød, gul, grøn"               = "RdYlGn"
      ),
      previews = 6
    ),
    # verbatimTextOutput("file_info"),
    fluidRow(
      column(
        width = 6,
        shiny::numericInput(
          inputId = "length",
          label = "Dage til udskrift:",
          min = 7,
          max = 150,
          value = 30
        )
      ),
      column(width = 6, shiny::uiOutput("event_types"))
    ),
    shiny::h3("Kalender (når klar):"),
    shiny::plotOutput("calendar", height = "60vh"),
    shiny::downloadButton(outputId = "download", label = "Hent A4")
  )

)

server <- function(input, output) {
  rv <- shiny::reactiveValues(data = NULL,
                              data_filter = NULL,
                              p = NULL)

  # output$file_info <- renderPrint({
  #     if (!is.null(input$file1)) {
  #         cat("Valgt fil:", input$file1$name)
  #     } else if (!isTRUE(validate_ics_link(input$link))) {
  #         cat("Indsæt korrekt link.")
  #     } else if (!is.null(input$link) && input$link != "") {
  #         cat("Indsat link:", input$link)
  #     } else {
  #         cat("Intet link indsat.")
  #     }
  # })

  shiny::observeEvent(list(input$file1, input$link), {
    if (!is.null(input$file1)) {
      rv$data <- calendar::ic_read(input$file1$datapath)
    } else if (!is.null(input$link) && input$link != "") {
      if (isTRUE(validate_ics_link(input$link))) {
        rv$data <- calendar::ic_read(input$link)
      } else {
        # Show an error message or warning
        showNotification("Tjek lige at du har indsat et korrekt link.", type = "error")
        rv$data <- NULL  # Clear data if invalid
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

  observeEvent(input$event_types, {
    req(input$event_types)

    rv$data_filter <- rv$data |>
      dplyr::filter(SUMMARY %in% input$event_types) |>
      categorise_events()

  })

  output$calendar <- renderPlot({
    req(rv$data_filter)

    rv$p <- rv$data_filter |> print_events(length = input$length,
                                           palette = input$color_palette)
    rv$p
  })

  output$download <- downloadHandler(
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
shinyApp(ui = ui, server = server)

### - test med andre skemaer (SL, spl vagter)
### - week-wise plotting
### - round nearest whole month/week
### - create docker container
