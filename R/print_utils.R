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
#' @export
print_events <- function(data,
                         start     = Sys.Date(),
                         length    = NULL,
                         start_col = "start_date",
                         cat_col   = "EventType",
                         palette   = "Spectral",
                         type      = "month") {

  date_start        <- as.Date(start)
  data$start_date   <- as.Date(format(data$DTSTART, "%Y-%m-%d"))
  df_cat_min        <- data[, c("start_date", cat_col)]

  date_end <- if (is.null(length)) {
    max(df_cat_min$start_date)
  } else if (is.numeric(length)) {
    date_start + as.integer(min(length, 90L))
  } else {
    date_start + 30L
  }

  # Filter before join
  df_cat_min <- df_cat_min[
    df_cat_min$start_date >= date_start & df_cat_min$start_date <= date_end,
  ]

  # Full join via merge — all.x keeps every day in the sequence
  date_seq <- data.frame(date = seq(date_start, date_end, by = 1L))
  df_print <- merge(date_seq, df_cat_min,
                    by.x = "date", by.y = "start_date",
                    all.x = TRUE)

  cats <- unique(df_print[[cat_col]][!is.na(df_print[[cat_col]])])
  if (length(cats) == 0L) return(NULL)

  # Rename cat_col -> label and add category column
  names(df_print)[names(df_print) == cat_col] <- "label"
  df_print$category <- df_print$label

  print(plot_calendar(
    from     = date_start,
    to       = date_end,
    view     = type,
    day.size = 5,
    title    = "Arbejdsplan",
    subtitle = paste("Printet den", format(date_start, "%d. %b %Y")),
    week.number    = TRUE,
    start          = "M",
    legend.pos     = "bottom",
    events         = df_print,
    palette        = stats::setNames(
      generate_colors(n = length(cats), palette = palette),
      cats
    ),
    source_caption = "Printet fra 'agdamsbo.github.io/PrintSchedule'"
  ))
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
