library(shiny)
library(calendar)
library(lubridate)
library(dplyr)

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
                         cat_col = "EventType") {
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



    df_print <- dplyr::full_join(data.frame(date = as.Date(seq(
        date_start, date_end, by = 1
    ))),
    df_cat_min,
    by = dplyr::join_by(date == !!start_col)) |>
        dplyr::filter(date >= date_start &
                          date <= date_end) #|>
    # mutate(dow = as.numeric(format(date, "%w"))) %>%
    # mutate(month = format(date, "%B")) %>%
    # mutate(woy = as.numeric(format(date, "%U"))) %>%
    # mutate(year = as.numeric(format(date, "%Y"))) %>%
    # # mutate(month = toupper(factor(month, levels = months, ordered = TRUE))) %>%
    # # arrange(year, month) %>%
    # mutate(monlabel = month)

    # labels <- as.character(df_print[[cat_col]])

    calendR::calendR(
        from = date_start,
        to = date_end,
        title = glue::glue(
            "Schedule ({as.character(date_start)} to {as.character(date_end)})"
        ),
        subtitle = paste("Printed on", date_start),
        # text = labels,
        # text.pos = 1:30,
        week.number = TRUE,
        # year = lubridate::year(start),
        # month = lubridate::month(start),
        start = "M",
        legend.pos = "bottom",
        special.days = df_print[[cat_col]],
        special.col = RColorBrewer::brewer.pal(length(unique(
            na.omit(df_print[[cat_col]])
        )), "Spectral")
    )
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



############ APP

ui <- fluidPage(
    titlePanel("Arbejdsoversigt fra delt .ics-fil"),
    shiny::p(
        "Udskriv oversigt over arbejdstider for de næste dage ud fra det delte kalender link fra MyPlan"
    ),
    shiny::p(
        "Husk at når du printer kalenderen får du et øjebliksbillede. Det kan være rart, men du skal være opmærksom på ændringer der kommer til."
    ),
    textInput(
        inputId = "link",
        label = "Indsæt kalenderlink:",
        placeholder = "https://minplan.rm.dk/SharedCalendar/11248--yg6u0EBIE5.ics"
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
                showNotification("Tjek lige at du har indsat et korrekt link.",
                                 type = "error")
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

        rv$p <- rv$data_filter |> print_events(length = input$length)
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
