
#' wrapper to read ics file
#'
#' @param data ics file
#'
#' @returns data.frame
#' @export
#'
#' @importFrom calendar ic_read
read_ics <- function(data){
  calendar::ic_read(data)
}

find_col <- function(dat, prefix) {
  nm <- names(dat)[startsWith(names(dat), prefix)]
  if (length(nm) == 0) stop("No column matching prefix: ", prefix)
  nm[1]
}


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
categorise_events <- function(df, tz = "Europe/Copenhagen", night_label = "NV") {
  is_allday <- format(df$DTSTART, "%H:%M:%S") == "00:00:00" &
    format(df$DTEND,   "%H:%M:%S") == "00:00:00" &
    as.Date(df$DTEND) > as.Date(df$DTSTART)

  if (any(is_allday, na.rm = TRUE)) {
    allday <- df[is_allday, ]
    expanded <- do.call(rbind, lapply(seq_len(nrow(allday)), function(i) {
      row     <- allday[i, ]
      start_d <- as.Date(format(row$DTSTART, "%Y-%m-%d", tz = tz))
      end_d   <- as.Date(format(row$DTEND,   "%Y-%m-%d", tz = tz)) - 1
      days    <- seq(start_d, end_d, by = "day")
      n       <- length(days)
      rep_row <- row[rep(1, n), ]
      rep_row$DTSTART <- as.POSIXct(paste(days, "00:00:01"), tz = tz)
      rep_row$DTEND   <- as.POSIXct(paste(days, "23:59:59"), tz = tz)
      rep_row
    }))
    df <- rbind(df[!is_allday, ], expanded)
  }

  df$StartTime          <- format(df$DTSTART, "%H:%M")
  df$EndTime            <- format(df$DTEND,   "%H:%M")
  df$StretchesToNextDay <- as.Date(df$DTEND) > as.Date(df$DTSTART)

  df$EventType <- ifelse(
    df$StretchesToNextDay,
    paste0(df$StartTime, " - ", df$EndTime, " (", night_label, ")"),
    paste0(df$StartTime, " - ", df$EndTime)
  )
  df$EventType <- ifelse(
    df$SUMMARY != "Arbejde",
    paste0(df$EventType, " (", df$SUMMARY, ")"),
    df$EventType
  )
  df$EventType <- ifelse(
    df$StartTime == "00:00" & df$EndTime == "23:59",
    df$SUMMARY,
    df$EventType
  )
  df
}
