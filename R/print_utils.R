#' Print Events to Calendar
#'
#' Prepares event data and generates a calendar plot based on the calendR package.
#' This is a convenience wrapper that formats the data and calls the
#' main calendar plotting function.
#'
#' @param data A data frame containing calendar events, typically the output
#'   of [categorise_events()]. Must have `DTSTART`, `DTEND`, and `EventType` columns.
#' @param start Start date for the calendar. Default is today's date.
#' @param length Number of days to display. If NULL, uses all events in data.
#'   Maximum is 150 days.
#' @param start_col Column name containing dates. Default is "start_date".
#' @param cat_col Column name containing event categories. Default is "EventType".
#' @param palette Color palette name for event categories. Default is "Spectral".
#' @param type Calendar view type: "month" or "week". Default is "month".
#'
#' @return A ggplot2 object (calendar plot).
#'
#' @examples
#' \dontrun{
#' events <- data.frame(
#'   DTSTART = as.POSIXct("2024-06-15 08:00:00"),
#'   DTEND = as.POSIXct("2024-06-15 16:00:00"),
#'   SUMMARY = "Arbejde"
#' )
#' events <- categorise_events(events)
#' print_events(events, length = 30)
#' }
#'
#' @seealso [plot_calendar()] for more calendar options.
#'
#' @import dplyr
#' @importFrom lubridate days
#' @importFrom glue glue
#' @importFrom tidyselect all_of
#' @export
print_events <- function(data,
                         start = Sys.Date(),
                         length = NULL,
                         start_col = "start_date",
                         cat_col = "EventType",
                         palette = "Spectral",
                         type = "month") {
  df_cat_min <- data |>
    dplyr::mutate(start_date = as.Date(format(DTSTART, "%Y-%m-%d"))) |>
    dplyr::select(
      tidyselect::all_of(c(start_col, cat_col))
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

  df_print <- dplyr::full_join(data.frame(date = as.Date(seq(
    date_start, date_end, by = 1
  ))),
  df_cat_min,
  by = dplyr::join_by(date == !!start_col)) |>
    dplyr::filter(date >= date_start &
                    date <= date_end)

  plot_calendar(
    from = date_start,
    to = date_end,
    view = type,
    day.size = 5,
    title = "Arbejdsplan",
    # title = glue::glue(
    #   "Arbejdsplan ({as.character(date_start)} to {as.character(date_end)})"
    # ),
    subtitle = paste("Printed on", date_start),
    week.number = TRUE,
    start = "M",
    legend.pos = "bottom",
    events = dplyr::rename(df_print, label = !!cat_col) |> dplyr::mutate(category =
                                                                           label),
    palette = stats::setNames(
      generate_colors(n = length(unique(
        stats::na.omit(df_print[[cat_col]])
      )), palette = palette),
      unique(stats::na.omit(df_print[[cat_col]]))
    ),
    source_caption = "Printet fra 'agdamsbo.github.io/PrintSchedule'"
  )
}

#' Save ggplot to PDF
#'
#' Saves a ggplot object to PDF with standard paper sizes and optional
#' orientation. Defaults to A4 landscape.
#'
#' @param p A ggplot object to save.
#' @param doc_name Output filename (without extension). If empty, uses "schedule.pdf".
#' @param papersize Paper size: "A6", "A5", "A4", "A3", "A2", "A1", or "A0".
#'   Default is "A4".
#' @param orientation Page orientation: "l" (landscape) or "h" (portrait).
#'   Default is "l" (landscape).
#'
#' @return Invisible. Called for side effect (saves PDF file).
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
#'   ggplot2::geom_point()
#' ggplot2pdf(p, doc_name = "my_plot", papersize = "A3", orientation = "h")
#' }
#'
#' @importFrom ggplot2 ggsave
#' @export
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
