#' Validate ICS Link
#'
#' Checks if a URL ends with .ics (case-insensitive), indicating it's a valid
#' iCalendar file link.
#'
#' @param link A character string containing the URL to validate.
#'
#' @return Logical. TRUE if the link ends with .ics, FALSE otherwise.
#'
#' @examples
#' validate_ics_link("https://example.com/calendar.ics")
#' validate_ics_link("https://example.com/calendar.ICS")
#' validate_ics_link("https://example.com/calendar.pdf")
#'
#' @export
validate_ics_link <- function(link) {
  # Check if the link ends with .ics (case-insensitive)
  grepl("\\.ics$", link, ignore.case = TRUE)
}

#' Categorise Calendar Events
#'
#' Processes calendar event data from an ICS file. Expands all-day events
#' across multiple days and categorises events by type (work, course, etc.)
#' with formatted time ranges. Danish healthcare terminology is used for
#' event categorisation.
#'
#' @param df A data frame containing calendar events with columns
#'   `DTSTART`, `DTEND`, and `SUMMARY`.
#' @param tz Timezone string (default: "Europe/Copenhagen").
#'
#' @return A data frame with added columns: `StartTime`, `EndTime`,
#'   `StretchesToNextDay`, and `EventType`.
#'
#' @details
#' All-day events (where DTSTART and DTEND are at midnight and span multiple days)
#' are expanded into individual day events. Event types are formatted as
#' time ranges with optional "Nattevagt" (night shift) suffix for events
#' spanning midnight.
#'
#' @examples
#' \dontrun{
#' events <- data.frame(
#'   DTSTART = as.POSIXct("2024-06-15 08:00:00"),
#'   DTEND = as.POSIXct("2024-06-15 16:00:00"),
#'   SUMMARY = "Arbejde"
#' )
#' categorise_events(events)
#' }
#'
#' @import dplyr
#' @importFrom glue glue
#' @export
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
