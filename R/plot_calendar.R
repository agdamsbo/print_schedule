#' @title Monthly, yearly, and week-block calendars
#'
#' @param year  Calendar year (default: current year).
#' @param month Numeric month 1–12, or NULL for a full-year calendar.
#' @param from  Custom start date (overrides year/month).
#' @param to    Custom end date (required when from is set).
#' @param view  `"month"` (default) or `"week"` for a rolling week-block view.
#' @param start `"S"` (Sunday, default) or `"M"` (Monday).
#' @param orientation `"landscape"` (default) or `"portrait"`.
#' @param title  Main title. Auto-generated when missing.
#' @param title.size,title.col  Title font size and colour.
#' @param subtitle,subtitle.size,subtitle.col  Optional italic subtitle.
#' @param text,text.pos,text.size,text.col  Character annotations on specific
#'   day positions (monthly view only).
#' @param events  A data.frame with columns `date` (Date or "YYYY-MM-DD"),
#'   `label` (character), and optionally `category` (character).
#' @param palette  Named character vector mapping category names to colours.
#' @param special.days  Integer vector of day indices to highlight, or `"weekend"`.
#' @param special.col  Highlight colour (or upper gradient colour).
#' @param gradient  Logical. Treat `special.days` as a continuous gradient.
#' @param low.col  Lower gradient / background tile colour (default `"white"`).
#' @param col,lwd,lty  Grid line colour, width, linetype.
#' @param font.family,font.style  Font family and style for all text.
#' @param day.size,days.col  Day-number font size and colour.
#' @param weeknames,weeknames.col,weeknames.size  Day-of-week header labels,
#'   colour, and size.
#' @param week.number,week.number.col,week.number.size  Show ISO week numbers.
#' @param monthnames,months.size,months.col,months.pos,mbg.col  Month label
#'   customisation (yearly view).
#' @param legend.pos,legend.title  Legend position and title.
#' @param bg.col  Background colour.
#' @param margin  Numeric margin multiplier.
#' @param ncol  Number of facet columns (yearly / week-block view).
#' @param source_caption Optional caption string.
#'
#' @return A \pkg{ggplot2} object.
#'
#' @import ggplot2
#' @export
plot_calendar <- function(year        = as.integer(format(Sys.Date(), "%Y")),
                          month       = NULL,
                          from        = NULL,
                          to          = NULL,
                          view        = c("month", "week"),
                          start       = c("S", "M"),
                          orientation = c("landscape", "portrait"),

                          title,
                          title.size  = 20,
                          title.col   = "gray30",

                          subtitle       = "",
                          subtitle.size  = 10,
                          subtitle.col   = "gray30",

                          text       = NULL,
                          text.pos   = NULL,
                          text.size  = 8,
                          text.col   = "gray30",

                          events  = NULL,
                          palette = NULL,

                          special.days = NULL,
                          special.col  = "gray90",
                          gradient     = FALSE,
                          low.col      = "white",

                          col = "gray30",
                          lwd = 0.5,
                          lty = 1,

                          font.family = "sans",
                          font.style  = "plain",

                          day.size  = 3,
                          days.col  = "gray30",

                          weeknames,
                          weeknames.col  = "gray30",
                          weeknames.size = 4.5,

                          week.number      = FALSE,
                          week.number.col  = "gray30",
                          week.number.size = 8,

                          monthnames,
                          months.size = 10,
                          months.col  = "gray30",
                          months.pos  = 0.5,
                          mbg.col     = "white",

                          legend.pos   = "bottom",
                          legend.title = "",

                          bg.col = "white",
                          margin = 1,
                          ncol,

                          source_caption = NULL) {

  # ── Argument validation ────────────────────────────────────────────────────
  view        <- match.arg(view)
  start       <- match.arg(start)
  orientation <- match.arg(orientation)

  if (year < 0)
    stop("'year' must be a positive integer.")
  if (!is.null(from) && is.null(to))
    stop("Provide an end date with the 'to' argument.")
  if (is.null(from) && !is.null(to))
    stop("Provide a start date with the 'from' argument.")
  if (!is.null(month) && (!is.numeric(month) || month < 1 || month > 12))
    stop("'month' must be a number between 1 and 12.")

  # ── Default ncol ──────────────────────────────────────────────────────────
  if (missing(ncol))
    ncol <- if (orientation == "landscape") 4L else 3L

  # ── Week-day names ────────────────────────────────────────────────────────
  if (missing(weeknames)) {
    cap1 <- function(x) { substr(x, 1, 1) <- toupper(substr(x, 1, 1)); x }
    ref  <- seq(as.Date("2020-08-23"), by = 1, len = 7)
    wd   <- cap1(weekdays(ref))
    weeknames <- c(wd[2:7], wd[1])
  }

  # ── Date range ────────────────────────────────────────────────────────────
  if (!is.null(from)) {
    mindate <- as.Date(from)
    maxdate <- as.Date(to)
    if (maxdate <= mindate)
      stop("'to' must be after 'from'.")
    if (as.numeric(maxdate - mindate) > 366)
      stop("'from' and 'to' cannot be more than 1 year apart.")
  } else if (!is.null(month)) {
    mindate <- as.Date(sprintf("%d-%02d-01", year, month))
    maxdate <- seq(mindate, length = 2, by = "month")[2] - 1
  } else {
    mindate <- as.Date(sprintf("%d-01-01", year))
    maxdate <- as.Date(sprintf("%d-12-31", year))
  }

  dates  <- seq(mindate, maxdate, by = "1 day")
  n_days <- length(dates)

  # ── Month-name factor levels ───────────────────────────────────────────────
  month_levels <- format(
    seq(as.Date("2016-01-01"), as.Date("2016-12-01"), by = "month"), "%B"
  )

  # ── Day-of-week helpers ───────────────────────────────────────────────────
  dow_offset <- if (start == "M") {
    function(d) ifelse(as.integer(format(d, "%w")) == 0L,
                       6L, as.integer(format(d, "%w")) - 1L)
  } else {
    function(d) as.integer(format(d, "%w"))
  }
  woy_fmt <- if (start == "M") "%W" else "%U"

  # ── Base data.frame ───────────────────────────────────────────────────────
  t1            <- data.frame(date = dates, stringsAsFactors = FALSE)
  t1$dow        <- dow_offset(dates)
  t1$woy        <- as.integer(format(dates, woy_fmt))
  t1$yr         <- as.integer(format(dates, "%Y"))
  t1$mon_nm     <- format(dates, "%B")
  t1$month_fac  <- toupper(as.character(
    factor(t1$mon_nm, levels = month_levels, ordered = TRUE)
  ))
  t1$monlabel   <- if (!is.null(month)) {
    paste(t1$month_fac, t1$yr)
  } else {
    as.character(t1$month_fac)
  }
  t1$monlabel   <- factor(t1$monlabel, levels = unique(t1$monlabel))
  t1$is_weekend <- if (start == "M") {
    t1$dow %in% c(5L, 6L)
  } else {
    t1$dow %in% c(0L, 6L)
  }

  # ── Fill vector ───────────────────────────────────────────────────────────
  if (!is.null(special.days)) {
    if (identical(special.days, "weekend")) {
      fills <- as.numeric(t1$is_weekend)
    } else if (is.numeric(special.days) && !gradient) {
      fills <- rep(0, n_days)
      idx   <- as.integer(special.days)
      idx   <- idx[idx >= 1L & idx <= n_days]
      fills[idx] <- 1
    } else if (is.numeric(special.days) && gradient) {
      if (length(special.days) != n_days)
        stop("When gradient = TRUE, 'special.days' length must equal the number of days.")
      fills <- special.days
    } else {
      stop("'special.days' must be 'weekend', an integer index vector, or a numeric gradient vector.")
    }
  } else {
    fills       <- rep(0, n_days)
    special.col <- low.col
  }
  t1$fill <- fills

  # ── Day text annotations ──────────────────────────────────────────────────
  ann_texts <- character(n_days)
  if (!is.null(text) && !is.null(text.pos)) {
    ann_texts[text.pos] <- text
  } else {
    if (!is.null(text)     && is.null(text.pos)) warning("Specify day positions with 'text.pos'.")
    if ( is.null(text)     && !is.null(text.pos)) warning("Provide text with the 'text' argument.")
  }
  t1$ann_text <- ann_texts

  # ── Events ────────────────────────────────────────────────────────────────
  has_events <- !is.null(events) && nrow(events) > 0

  if (has_events) {
    events$date <- as.Date(events$date)
    if (!"category" %in% names(events)) events$category <- "Event"
    if (!"label"    %in% names(events)) events$label    <- ""

    events <- events[events$date >= mindate & events$date <= maxdate, ]

    # Join category onto t1 (first occurrence per date only)
    ev_fill      <- events[!duplicated(events$date), c("date", "category")]
    t1           <- merge(t1, ev_fill, by = "date", all.x = TRUE)
    t1           <- t1[order(t1$date), ]
    t1$fill      <- ifelse(!is.na(t1$category),
                           t1$category, as.character(t1$fill))
    t1$category  <- NULL

    if (is.null(palette)) {
      cats    <- unique(events$category)
      colours <- grDevices::hcl.colors(max(length(cats), 1L), palette = "Set 2")
      palette <- stats::setNames(colours, cats)
    }
  }

  # ── Dispatch ──────────────────────────────────────────────────────────────
  if (view == "week") {
    p <- .build_week_view(
      t1               = t1,
      dates            = dates,
      mindate          = mindate,
      maxdate          = maxdate,
      start            = start,
      ncol             = ncol,
      title            = if (missing(title)) NULL else title,
      title.size       = title.size,
      title.col        = title.col,
      subtitle         = subtitle,
      subtitle.size    = subtitle.size,
      subtitle.col     = subtitle.col,
      weeknames        = weeknames,
      weeknames.col    = weeknames.col,
      weeknames.size   = weeknames.size,
      day.size         = day.size,
      days.col         = days.col,
      col              = col,
      lwd              = lwd,
      lty              = lty,
      low.col          = low.col,
      special.col      = special.col,
      week.number.col  = if (week.number) week.number.col else "transparent",
      week.number.size = week.number.size,
      font.family      = font.family,
      font.style       = font.style,
      legend.pos       = legend.pos,
      legend.title     = legend.title,
      bg.col           = bg.col,
      margin           = margin,
      events           = events,
      palette          = palette,
      has_events       = has_events
    )
  } else {
    if (missing(title)) {
      title <- if (!is.null(from)) {
        paste0(format(mindate, "%m/%Y"), " - ", format(maxdate, "%m/%Y"))
      } else if (is.null(month)) {
        year
      } else {
        as.character(unique(t1$monlabel))
      }
    }

    p <- .build_month_view(
      t1               = t1,
      dates            = dates,
      month            = month,
      ncol             = ncol,
      start            = start,
      weeknames        = weeknames,
      weeknames.col    = weeknames.col,
      weeknames.size   = weeknames.size,
      day.size         = day.size,
      days.col         = days.col,
      text.col         = text.col,
      col              = col,
      lwd              = lwd,
      lty              = lty,
      low.col          = low.col,
      special.col      = special.col,
      gradient         = gradient,
      title            = title,
      title.size       = title.size,
      title.col        = title.col,
      subtitle         = subtitle,
      subtitle.size    = subtitle.size,
      subtitle.col     = subtitle.col,
      monthnames       = if (missing(monthnames)) NULL else monthnames,
      months.size      = months.size,
      months.col       = months.col,
      months.pos       = months.pos,
      mbg.col          = mbg.col,
      week.number      = week.number,
      week.number.col  = week.number.col,
      week.number.size = week.number.size,
      font.family      = font.family,
      font.style       = font.style,
      legend.pos       = legend.pos,
      legend.title     = legend.title,
      bg.col           = bg.col,
      margin           = margin,
      events           = events,
      palette          = palette,
      has_events       = has_events
    )
  }

  if (!is.null(source_caption))
    p <- p + ggplot2::labs(caption = source_caption)

  invisible(p)
}


# ══════════════════════════════════════════════════════════════════════════════
#  INTERNAL: month / year view
# ══════════════════════════════════════════════════════════════════════════════

.build_month_view <- function(t1, dates, month, ncol, start,
                              weeknames, weeknames.col, weeknames.size,
                              day.size, days.col, text.col,
                              col, lwd, lty, low.col, special.col, gradient,
                              title, title.size, title.col,
                              subtitle, subtitle.size, subtitle.col,
                              monthnames, months.size, months.col,
                              months.pos, mbg.col,
                              week.number, week.number.col, week.number.size,
                              font.family, font.style,
                              legend.pos, legend.title, bg.col, margin,
                              events, palette, has_events) {

  # Row position within each month ──────────────────────────────────────────
  t2           <- t1
  mon_min_woy  <- ave(t2$woy, t2$monlabel, FUN = min)
  mon_max_lwoy <- ave(t2$woy - mon_min_woy, t2$monlabel, FUN = max)
  t2$local_woy <- t2$woy - mon_min_woy
  t2$y         <- mon_max_lwoy - t2$local_woy + 1L

  # Optional custom month names ─────────────────────────────────────────────
  if (!is.null(monthnames)) {
    lvls <- levels(t2$monlabel)
    if (length(monthnames) != length(lvls))
      stop("Length of 'monthnames' must equal the number of months displayed.")
    levels(t2$monlabel) <- monthnames
  }

  # Fill scale ───────────────────────────────────────────────────────────────
  fill_is_char <- is.character(t2$fill)

  fill_scale <- if (has_events) {
    ggplot2::scale_fill_manual(values = palette, name = legend.title,
                               na.value = low.col, na.translate = FALSE)
  } else if (fill_is_char) {
    ggplot2::scale_fill_manual(values = special.col, name = legend.title,
                               na.value = low.col, na.translate = FALSE)
  } else {
    ggplot2::scale_fill_gradient(low = low.col, high = special.col,
                                 name = legend.title, na.value = low.col)
  }

  # Single-month layout ──────────────────────────────────────────────────────
  if (!is.null(month)) {
    wd_ordered <- if (start == "S") c(weeknames[7], weeknames[1:6]) else weeknames
    df_header  <- data.frame(
      week  = wd_ordered,
      pos.x = 0:6,
      pos.y = max(t2$local_woy) + 1.75,
      stringsAsFactors = FALSE
    )

    p <- ggplot2::ggplot(t2, ggplot2::aes(dow, y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = fill),
                         color = col, linewidth = lwd, linetype = lty) +
      fill_scale +
      ggplot2::ggtitle(title) +
      ggplot2::labs(subtitle = subtitle, fill = legend.title) +
      ggplot2::geom_text(data = df_header,
                         ggplot2::aes(label = week, x = pos.x, y = pos.y),
                         size = weeknames.size, family = font.family,
                         color = weeknames.col, fontface = font.style) +
      ggplot2::geom_text(ggplot2::aes(label = ann_text),
                         color = text.col, size = 3, family = font.family) +
      ggplot2::scale_y_continuous(
        expand = c(0.05, 0.05),
        labels = rev(unique(t2$woy)),
        breaks = seq_len(length(unique(t2$woy)))
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = as.integer(format(date, "%d")), x = dow, y = y),
        size = day.size, family = font.family, color = days.col,
        fontface = font.style, hjust = 0.5
      ) +
      .calendar_theme_single(bg.col, weeknames.col, weeknames.size,
                             week.number.col, week.number.size,
                             title.size, title.col, subtitle.size, subtitle.col,
                             legend.pos, margin, font.family, font.style)

  } else {
    # Yearly / multi-month facet layout ──────────────────────────────────────
    wd_ordered <- if (start == "S") c(weeknames[7], weeknames[1:6]) else weeknames
    wd_short   <- substring(wd_ordered, 1, 3)

    p <- ggplot2::ggplot(t2, ggplot2::aes(dow, woy + 1)) +
      ggplot2::geom_tile(ggplot2::aes(fill = fill),
                         color = col, linewidth = lwd, linetype = lty) +
      fill_scale +
      ggplot2::facet_wrap(~monlabel, ncol = ncol, scales = "free") +
      ggplot2::ggtitle(title) +
      ggplot2::labs(subtitle = subtitle, fill = legend.title) +
      ggplot2::scale_x_continuous(expand = c(0.01, 0.01), position = "top",
                                  breaks = 0:6, labels = wd_short) +
      ggplot2::scale_y_continuous(expand = c(0.01, 0.01), trans = "reverse",
                                  breaks = sort(unique(t2$woy + 1))) +
      ggplot2::geom_text(ggplot2::aes(label = as.integer(format(date, "%d"))),
                         size = day.size, family = font.family,
                         color = days.col, fontface = font.style) +
      .calendar_theme_yearly(bg.col, mbg.col, months.col, months.size,
                             months.pos, weeknames.col, weeknames.size,
                             week.number.col, week.number.size,
                             title.size, title.col, subtitle.size, subtitle.col,
                             legend.pos, margin, font.family, font.style)
  }

  p
}


# ══════════════════════════════════════════════════════════════════════════════
#  INTERNAL: week-block view
# ══════════════════════════════════════════════════════════════════════════════

.build_week_view <- function(t1, dates, mindate, maxdate, start, ncol,
                             title, title.size, title.col,
                             subtitle, subtitle.size, subtitle.col,
                             weeknames, weeknames.col, weeknames.size,
                             day.size, days.col, col, lwd, lty,
                             low.col, special.col,
                             week.number.col, week.number.size,
                             font.family, font.style,
                             legend.pos, legend.title, bg.col, margin,
                             events, palette, has_events,
                             weeks_per_block = 5L) {

  # Assign week-blocks ───────────────────────────────────────────────────────
  all_weeks  <- sort(unique(t1$woy))
  n_blocks   <- ceiling(length(all_weeks) / weeks_per_block)
  week_block <- stats::setNames(
    rep(seq_len(n_blocks), each = weeks_per_block)[seq_along(all_weeks)],
    all_weeks
  )

  t2              <- t1
  t2$block        <- week_block[as.character(t2$woy)]
  woy_rank        <- match(t2$woy, all_weeks)
  t2$row_in_block <- woy_rank - (t2$block - 1L) * weeks_per_block

  # Block date-range labels ──────────────────────────────────────────────────
  block_lo <- tapply(t2$date, t2$block, min)
  block_hi <- tapply(t2$date, t2$block, max)
  block_ranges <- data.frame(
    block  = as.integer(names(block_lo)),
    lo     = as.Date(block_lo, origin = "1970-01-01"),
    hi     = as.Date(block_hi, origin = "1970-01-01"),
    stringsAsFactors = FALSE
  )
  block_ranges$blabel <- sprintf(
    "%s - %s",
    format(block_ranges$lo, "%d %b"),
    format(block_ranges$hi, "%d %b %Y")
  )

  t2 <- merge(t2, block_ranges[, c("block", "blabel")], by = "block", all.x = TRUE)
  t2 <- t2[order(t2$date), ]
  t2$blabel <- factor(t2$blabel, levels = unique(t2$blabel))

  # Event join ───────────────────────────────────────────────────────────────
  if (has_events) {
    ev_join              <- events[, c("date", "category", "label")]
    names(ev_join)[2:3]  <- c("event_cat", "event_label")
    t2                   <- merge(t2, ev_join, by = "date", all.x = TRUE)
    t2                   <- t2[order(t2$date), ]
  } else {
    t2$event_cat   <- NA_character_
    t2$event_label <- ""
  }

  # Tile fill ────────────────────────────────────────────────────────────────
  t2$tile_fill <- ifelse(
    !is.na(t2$event_cat),
    t2$event_cat,
    ifelse(
      is.numeric(t2$fill) & t2$fill != 0,
      "highlight",
      ifelse(
        is.character(t2$fill) & !is.na(t2$fill),
        t2$fill,
        NA_character_
      )
    )
  )

  fill_values <- c(highlight = special.col)
  if (has_events) fill_values <- c(fill_values, palette)

  # Title ────────────────────────────────────────────────────────────────────
  if (is.null(title))
    title <- sprintf("%s \u2013 %s",
                     format(mindate, "%b %Y"),
                     format(maxdate, "%b %Y"))

  wd_labels <- if (start == "S") c(weeknames[7], weeknames[1:6]) else weeknames
  wd_short  <- substring(wd_labels, 1, 3)

  # First-of-month subset ────────────────────────────────────────────────────
  first_of_month <- t2[as.integer(format(t2$date, "%d")) == 1L, ]

  # Build plot ───────────────────────────────────────────────────────────────
  p <- ggplot2::ggplot(t2, ggplot2::aes(x = dow, y = woy + 1)) +
    ggplot2::geom_tile(ggplot2::aes(fill = tile_fill),
                       color = col, linewidth = lwd, linetype = lty) +
    ggplot2::scale_fill_manual(values = fill_values, na.value = low.col,
                               name = legend.title, na.translate = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = as.integer(format(date, "%d"))),
                       size = day.size, color = days.col,
                       family = font.family, fontface = font.style, hjust = 0.5) +
    ggplot2::geom_text(data = first_of_month,
                       ggplot2::aes(label = toupper(format(date, "%b")), y = woy + .7),
                       size = day.size * 0.7, color = weeknames.col,
                       family = font.family, fontface = "bold") +
    ggplot2::facet_wrap(~blabel, ncol = ncol, scales = "free_y") +
    ggplot2::scale_y_continuous(
      trans  = "reverse",
      breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)
    ) +
    ggplot2::scale_x_continuous(breaks = 0:6, labels = wd_short,
                                position = "top", expand = c(0.01, 0.01)) +
    ggplot2::ggtitle(title) +
    ggplot2::labs(subtitle = subtitle) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = NA,     color = NA),
      strip.background = ggplot2::element_rect(fill = bg.col, color = bg.col),
      plot.background  = ggplot2::element_rect(fill = bg.col),
      panel.grid       = ggplot2::element_line(colour = bg.col),
      strip.text.x     = ggplot2::element_text(hjust = 0, face = "bold",
                                               color = "gray30",
                                               size  = weeknames.size * 1.5),
      axis.ticks       = ggplot2::element_blank(),
      axis.title       = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(colour = weeknames.col,
                                               size   = weeknames.size * 2.25),
      axis.text.y      = ggplot2::element_text(colour = week.number.col,
                                               size   = week.number.size),
      plot.title       = ggplot2::element_text(hjust = 0.5, size = title.size,
                                               colour = title.col),
      plot.subtitle    = ggplot2::element_text(hjust = 0.5, face = "italic",
                                               colour = subtitle.col,
                                               size   = subtitle.size),
      legend.position  = legend.pos,
      plot.margin      = ggplot2::unit(c(margin, 0.5 * margin, margin, 0.5 * margin), "cm"),
      text             = ggplot2::element_text(family = font.family, face = font.style)
    )

  p
}


# ══════════════════════════════════════════════════════════════════════════════
#  INTERNAL: theme helpers
# ══════════════════════════════════════════════════════════════════════════════

.calendar_theme_single <- function(bg.col, weeknames.col, weeknames.size,
                                   week.number.col, week.number.size,
                                   title.size, title.col,
                                   subtitle.size, subtitle.col,
                                   legend.pos, margin, font.family, font.style) {
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = NA, color = NA),
    strip.background = ggplot2::element_rect(fill = NA, color = NA),
    plot.background  = ggplot2::element_rect(fill = bg.col),
    panel.grid       = ggplot2::element_line(colour = bg.col),
    strip.text.x     = ggplot2::element_text(hjust = 0, face = "bold"),
    axis.ticks       = ggplot2::element_blank(),
    axis.title       = ggplot2::element_blank(),
    axis.text.x      = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_text(colour = week.number.col,
                                             size   = week.number.size),
    plot.title       = ggplot2::element_text(hjust = 0.5, size = title.size,
                                             colour = title.col),
    plot.subtitle    = ggplot2::element_text(hjust = 0.5, face = "italic",
                                             colour = subtitle.col,
                                             size   = subtitle.size),
    legend.position  = legend.pos,
    plot.margin      = ggplot2::unit(c(margin, 0.5 * margin, margin, 0.5 * margin), "cm"),
    text             = ggplot2::element_text(family = font.family, face = font.style)
  )
}

.calendar_theme_yearly <- function(bg.col, mbg.col, months.col, months.size,
                                   months.pos, weeknames.col, weeknames.size,
                                   week.number.col, week.number.size,
                                   title.size, title.col,
                                   subtitle.size, subtitle.col,
                                   legend.pos, margin, font.family, font.style) {
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = NA,     color = NA),
    strip.background = ggplot2::element_rect(fill = mbg.col, color = mbg.col),
    plot.background  = ggplot2::element_rect(fill = bg.col),
    panel.grid       = ggplot2::element_line(colour = bg.col),
    strip.text.x     = ggplot2::element_text(hjust = months.pos, color = months.col,
                                             size  = months.size),
    axis.ticks       = ggplot2::element_blank(),
    axis.title       = ggplot2::element_blank(),
    axis.text.x      = ggplot2::element_text(colour = weeknames.col,
                                             size   = weeknames.size * 2.25),
    axis.text.y      = ggplot2::element_text(colour = week.number.col,
                                             size   = week.number.size),
    plot.title       = ggplot2::element_text(hjust = 0.5, size = title.size,
                                             colour = title.col),
    plot.subtitle    = ggplot2::element_text(hjust = 0.5, face = "italic",
                                             colour = subtitle.col,
                                             size   = subtitle.size),
    legend.position  = legend.pos,
    plot.margin      = ggplot2::unit(c(margin, 0.5 * margin, margin, 0.5 * margin), "cm"),
    text             = ggplot2::element_text(family = font.family, face = font.style),
    strip.placement  = "outside"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
#  UTILITY
# ══════════════════════════════════════════════════════════════════════════════

# Null-coalescing operator (avoids importing rlang just for this)
`%||%` <- function(a, b) {
  if (!is.null(a)) {
    a
  } else{
    b
  }
}

#' Generate a vector of colours from a named palette
#'
#' @param n       Number of colours to generate (positive integer).
#' @param palette Name of the colour palette to use. Can be a viridisLite palette
#'   (`"viridis"`, `"magma"`, `"plasma"`, `"inferno"`, `"cividis"`, `"mako"`,
#'   `"rocket"`, `"turbo"`), a grDevices palette (`"hcl"`, `"rainbow"`,
#'   `"heat"`, `"terrain"`, `"topo"`), an RColorBrewer palette (e.g., `"Set1"`,
#'   `"Spectral"`, `"Dark2"`), or a function that generates colours.
#' @param ...     Additional arguments passed to the palette function.
#'
#' @return A character vector of colour hex codes.
#'
#' @examples
#' generate_colors(5, "viridis")
#' generate_colors(3, "Spectral")
#' generate_colors(4, "Dark2")
#'
#' @importFrom viridisLite viridis
#' @importFrom grDevices hcl.colors rainbow heat.colors terrain.colors topo.colors
#'   colorRampPalette palette.colors palette.pals col2rgb
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @export
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
        # Named HCL palettes (e.g. "Rocket", "Plasma") distinct from viridisLite
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


#' Pick a contrasting text colour for a given background
#'
#' @param background  A colour string accepted by \code{\link[grDevices]{col2rgb}}.
#' @param light_text  Colour to return on dark backgrounds (default `"white"`).
#' @param dark_text   Colour to return on light backgrounds (default `"black"`).
#' @param threshold   Luminance cut-point in `[0, 1]` (default 0.5).
#' @param method      One of `"relative"`, `"perceived"`, `"perceived_2"` (default).
#' @return A character vector of colour names the same length as `background`.
#'
#' @examples
#' contrast_text("#000000")  # returns "white"
#' contrast_text("#FFFFFF")  # returns "black"
#'
#' @importFrom grDevices col2rgb
#' @export
contrast_text <- function(background,
                          light_text = "white",
                          dark_text  = "black",
                          threshold  = 0.5,
                          method     = c("perceived_2", "perceived", "relative")) {
  method <- match.arg(method)

  rgb_mat <- grDevices::col2rgb(background) / 255   # 3 × n matrix

  luminance <- switch(
    method,
    relative    = drop(c(0.2126, 0.7152, 0.0722) %*% rgb_mat),
    perceived   = drop(c(0.299, 0.587, 0.114)  %*% rgb_mat),
    perceived_2 = sqrt(colSums((
      c(0.299, 0.587, 0.114) * rgb_mat
    )^2))
  )

  ifelse(luminance < threshold, light_text, dark_text)
}
