# calendR_refactored.R
# Refactored version of calendR with:
#   - Reduced dependencies (forcats, ggimage removed; suncalc/gggibbous made optional)
#   - New `view` argument: "month" (default) or "week"
#   - New `events` argument: data.frame(date, label, category) for annotating days
#   - New `palette` argument: named character vector mapping category -> color
#
# Hard dependencies: ggplot2, dplyr
# Soft dependencies (loaded only when used):
#   - suncalc + gggibbous  (only when lunar = TRUE)

#' @title Monthly, yearly, and week-block calendars
#'
#' @param year  Calendar year (default: current year).
#' @param month Numeric month, or NULL for a full-year calendar.
#' @param from  Custom start date (overrides year/month).
#' @param to    Custom end date (required when from is set).
#' @param view  `"month"` (default) for the classic month/year layout, or
#'   `"week"` for a rolling week-block view that shows up to 5 weeks at a time.
#'   When `view = "week"` and `month` is NULL, the calendar is split into
#'   groups of 5 weeks and printed as separate facets (one per page-block).
#' @param start `"S"` (Sunday, default) or `"M"` (Monday).
#' @param orientation `"landscape"` (default) or `"portrait"`.
#' @param title  Main title. Auto-generated when missing.
#' @param title.size,title.col  Title font size and colour.
#' @param subtitle,subtitle.size,subtitle.col  Optional italic subtitle.
#' @param text,text.pos,text.size,text.col  Character annotations on specific
#'   day positions (monthly view only).
#' @param events  A data.frame with at least columns `date` (Date or
#'   character "YYYY-MM-DD") and `label` (character). An optional `category`
#'   column is used together with `palette` for colour coding.
#' @param palette  Named character vector mapping category names to fill
#'   colours, e.g. `c(Holiday = "#4A90D9", Meeting = "#E86C3A")`.
#'   If `events` has no `category` column, all events share `special.col`.
#' @param special.days  Numeric vector of day indices to highlight, or
#'   `"weekend"`. Ignored when `events` is supplied.
#' @param special.col  Highlight colour (or upper gradient colour).
#' @param gradient  Logical. Create a gradient fill across all days.
#' @param low.col  Lower gradient / background colour (default `"white"`).
#' @param col,lwd,lty  Grid line colour, width, type.
#' @param font.family,font.style  Font family and style for all text.
#' @param day.size,days.col  Day-number font size and colour.
#' @param weeknames,weeknames.col,weeknames.size  Day-of-week header labels,
#'   colour and size.
#' @param week.number,week.number.col,week.number.size  Show ISO week numbers
#'   on the y-axis.
#' @param monthnames,months.size,months.col,months.pos,mbg.col  Month label
#'   customisation (yearly view).
#' @param legend.pos,legend.title  Legend position and title.
#' @param bg.col  Background colour.
#' @param margin  Numeric margin multiplier.
#' @param ncol  Number of facet columns (yearly/week-block view).
#'
#' @return A \pkg{ggplot2} object.
#' @import ggplot2 dplyr
#' @export
calendR <- function(
    year        = as.integer(format(Sys.Date(), "%Y")),
    month       = NULL,
    from        = NULL,
    to          = NULL,

    # --- NEW: view mode ---
    view        = c("month", "week"),

    start       = c("S", "M"),
    orientation = c("landscape", "portrait"),

    title,
    title.size  = 20,
    title.col   = "gray30",

    subtitle       = "",
    subtitle.size  = 10,
    subtitle.col   = "gray30",

    # Day text annotations (monthly view)
    text       = NULL,
    text.pos   = NULL,
    text.size  = 8,
    text.col   = "gray30",

    # --- NEW: event data.frame and colour palette ---
    events  = NULL,   # data.frame(date, label, [category])
    palette = NULL,   # c(Category = "#hex", ...)

    # Legacy highlight arguments
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
    ncol
) {

  # ── Argument validation ─────────────────────────────────────────────────────
  view        <- match.arg(view)
  start       <- match.arg(start)
  orientation <- match.arg(orientation)


  if (year < 0)
    stop("Year must be a positive integer.")
  if (!is.null(from) && is.null(to))
    stop("Provide an end date with the 'to' argument.")
  if (is.null(from) && !is.null(to))
    stop("Provide a start date with the 'from' argument.")
  if (!is.null(month)) {
    if (!is.numeric(month) || month < 1 || month > 12)
      stop("'month' must be a number between 1 and 12.")
  }

  # ── Default ncol ────────────────────────────────────────────────────────────
  if (missing(ncol)) {
    ncol <- ifelse(orientation %in% c("landscape", "l"), 4, 3)
  }

  # ── Week names (locale-aware, no forcats needed) ──────────────────────────
  if (missing(weeknames)) {
    cap1 <- function(x) { substr(x,1,1) <- toupper(substr(x,1,1)); x }
    ref  <- seq(as.Date("2020-08-23"), by = 1, len = 7)   # Sun..Sat
    wd   <- cap1(weekdays(ref))
    weeknames <- c(wd[2:7], wd[1])   # Mon..Sun
  }

  # ── Date range ───────────────────────────────────────────────────────────────
  if (!is.null(from)) {
    mindate <- as.Date(from)
    maxdate <- as.Date(to)
    if (maxdate <= mindate) stop("'to' must be after 'from'.")
    if (as.numeric(maxdate - mindate) > 366)
      stop("'from' and 'to' cannot be more than 1 year apart.")
  } else if (!is.null(month)) {
    mindate <- as.Date(sprintf("%d-%02d-01", year, month))
    maxdate <- seq(mindate, length = 2, by = "month")[2] - 1
  } else {
    mindate <- as.Date(sprintf("%d-01-01", year))
    maxdate <- as.Date(sprintf("%d-12-31", year))
  }

  dates <- seq(mindate, maxdate, by = "1 day")

  # ── Month name lookup (base R, no forcats) ───────────────────────────────
  month_levels <- format(seq(as.Date("2016-01-01"),
                             as.Date("2016-12-01"), by = "month"), "%B")

  # ── Build base tibble ────────────────────────────────────────────────────
  dow_offset <- if (start == "M") {
    function(d) ifelse(as.integer(format(d,"%w")) == 0L, 6L,
                       as.integer(format(d,"%w")) - 1L)
  } else {
    function(d) as.integer(format(d, "%w"))
  }
  woy_fmt <- if (start == "M") "%W" else "%U"

  t1 <- dplyr::tibble(date = dates) |>
    dplyr::mutate(
      dow    = dow_offset(date),
      woy    = as.integer(format(date, woy_fmt)),
      yr     = as.integer(format(date, "%Y")),
      mon_nm = format(date, "%B"),
      month  = toupper(factor(mon_nm, levels = month_levels, ordered = TRUE))
    )

  # monlabel: for multi-month views, include year
  t1 <- dplyr::mutate(t1,
                      monlabel = if (!is.null(month)) paste(month, yr) else as.character(month)
  )

  # Preserve facet order without forcats: use a factor with levels in
  # first-appearance order.
  label_order <- unique(t1$monlabel)
  t1 <- dplyr::mutate(t1,
                      monlabel = factor(monlabel, levels = label_order)
  )

  # ── Weekend flag ────────────────────────────────────────────────────────────
  t1 <- dplyr::mutate(t1,
                      is_weekend = if (start == "M") dow %in% c(5L, 6L) else dow %in% c(0L, 6L)
  )

  # ── Build fill vector (legacy special.days path) ─────────────────────────
  fills <- rep(0, length(dates))

  # browser()

  # if (!is.null(special.days) && !(length(special.days) == 1 && special.days == 0)) {
  #   if (is.character(special.days)) {
  #     if (any(special.days == "weekend")) {
  #       fills <- as.integer(t1$is_weekend)
  #     } else if (length(special.days) == length(dates)) {
        fills <- special.days
  #     } else {
  #       stop("'special.days' character vector must equal the number of days or be 'weekend'.")
  #     }
  #   } else {
  #     if (gradient) {
  #       if (length(special.days) != length(dates))
  #         stop("When gradient = TRUE, 'special.days' length must equal number of days.")
  #       fills <- special.days
  #     } else {
        # fills[special.days] <- 1
  #     }
  #   }
  # } else {
    special.col <- low.col   # no highlights → use background colour
  # }

  t1 <- dplyr::mutate(t1, fill = fills)


  # browser()

  # ── Text annotations ────────────────────────────────────────────────────────
  texts <- character(length(dates))
  if (!is.null(text) && !is.null(text.pos)) {
    texts[text.pos] <- text
  } else {
    if (!is.null(text)     && is.null(text.pos)) warning("Specify day positions with 'text.pos'.")
    if (is.null(text)      && !is.null(text.pos)) warning("Provide text with the 'text' argument.")
  }
  t1 <- dplyr::mutate(t1, ann_text = texts)

  # ── Events (new) ────────────────────────────────────────────────────────────
  # browser()
  has_events <- !is.null(events) && nrow(events) > 0
  if (has_events) {
    events <- dplyr::mutate(events, date = as.Date(date))
    if (!"category" %in% names(events)) events$category <- "Event"
    if (!"label"    %in% names(events)) events$label    <- ""

    t1 <- dplyr::mutate(t1, fill = events$category)
    # Default palette if none given
    if (is.null(palette)) {
      cats    <- unique(events$category)
      colours <- grDevices::hcl.colors(max(length(cats), 1), palette = "Set 1")
      palette <- stats::setNames(colours, cats)
    }
    events <- dplyr::filter(events, date >= mindate, date <= maxdate)
  }

  # browser()

  # ─────────────────────────────────────────────────────────────────────────────
  #  DISPATCH: week view vs month/year view
  # ─────────────────────────────────────────────────────────────────────────────

  if (view == "week") {
    p <- .build_week_view(
      t1, dates, mindate, maxdate,
      start, ncol,
      title, title.size, title.col,
      subtitle, subtitle.size, subtitle.col,
      weeknames, weeknames.col, weeknames.size,
      day.size, days.col,
      col, lwd, lty,
      low.col, special.col, gradient,
      week.number, week.number.col, week.number.size,
      font.family, font.style,
      legend.pos, legend.title,
      bg.col, margin,
      events, palette, has_events
    )

  } else {
    # ── month / year view ─────────────────────────────────────────────────────
    is_single_month <- (!is.null(month) && is.null(from)) ||
      (!is.null(from)  && !is.null(to)  && is.null(month))
    is_single_month <- !is.null(month)  # keep original logic

    if (missing(title)) {
      title <- if (!is.null(from)) {
        paste0(format(mindate,"%m/%Y"), " - ", format(maxdate,"%m/%Y"))
      } else if (is.null(month)) {
        year
      } else {
        as.character(unique(t1$monlabel))
      }
    }

    if (!week.number) week.number.col <- "transparent"

    p <- .build_month_view(
      t1, dates, mindate, maxdate, month,
      ncol, start,
      weeknames, weeknames.col, weeknames.size,
      day.size, days.col,
      col, lwd, lty,
      low.col, special.col, gradient,
      title, title.size, title.col,
      subtitle, subtitle.size, subtitle.col,
      monthnames, months.size, months.col, months.pos, mbg.col,
      week.number, week.number.col, week.number.size,
      font.family, font.style,
      legend.pos, legend.title,
      bg.col, margin,
      events, palette, has_events,
      lunar, lunar.col, lunar.size
    )
  }

  invisible(p)
}


# ═══════════════════════════════════════════════════════════════════════════════
#  INTERNAL: month / year view  (.build_month_view)
# ═══════════════════════════════════════════════════════════════════════════════

.build_month_view <- function(
    t1, dates, mindate, maxdate, month,
    ncol, start,
    weeknames, weeknames.col, weeknames.size,
    day.size, days.col, col, lwd, lty,
    low.col, special.col, gradient,
    title, title.size, title.col,
    subtitle, subtitle.size, subtitle.col,
    monthnames, months.size, months.col, months.pos, mbg.col,
    week.number, week.number.col, week.number.size,
    font.family, font.style, legend.pos, legend.title,
    bg.col, margin,
    events, palette, has_events,
    lunar, lunar.col, lunar.size
) {

  # y-position within month (row from top)
  t2 <- t1 |>
    dplyr::group_by(monlabel) |>
    dplyr::mutate(
      local_woy  = woy - min(woy),
      y          = max(local_woy) - local_woy + 1
    ) |>
    dplyr::ungroup()

  # Replace custom month names if provided
  if (!missing(monthnames)) {
    lvls <- levels(t2$monlabel)
    if (length(monthnames) != length(lvls))
      stop("Length of 'monthnames' must equal the number of months displayed.")
    levels(t2$monlabel) <- monthnames
  }

  # ── Base tile layer ────────────────────────────────────────────────────────
  # For single-month view, use y (descending row); for yearly, use woy.
  if (!is.null(month)) {
    # Single-month: classic layout using y
    df_header <- dplyr::tibble(
      week  = if (start == "S") c(weeknames[7], weeknames[1:6]) else weeknames,
      pos.x = 0:6,
      pos.y = rep(max(t2$local_woy) + 1.75, 7)
    )

    p <- ggplot2::ggplot(t2, ggplot2::aes(dow, y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = fill),
                         color = col, linewidth = lwd, linetype = lty)

    if (has_events) {
      p <- .add_event_tiles_single(p, t2, events)
    }

    p <- p +
      (if (is.character(t2$fill[1]))
        ggplot2::scale_fill_manual(values = special.col, na.value = "white", na.translate = FALSE)
       else
         ggplot2::scale_fill_gradient(low = low.col, high = special.col, na.value = "white")) +
      ggplot2::ggtitle(title) +
      ggplot2::labs(subtitle = subtitle, fill = legend.title) +
      ggplot2::geom_text(
        data  = df_header,
        ggplot2::aes(label = week, x = pos.x, y = pos.y),
        size = weeknames.size, family = font.family,
        color = weeknames.col, fontface = font.style
      ) +
      ggplot2::geom_text(ggplot2::aes(label = ann_text),
                         color = text.col %||% "gray30",
                         size = 3, family = font.family) +
      ggplot2::scale_y_continuous(
        expand = c(0.05, 0.05),
        labels = rev(unique(t2$woy)),
        breaks = seq_len(length(unique(t2$woy)))
      ) +
      ggplot2::geom_text(
        data = t2,
        ggplot2::aes(label = as.integer(format(date, "%d")),
                     x = dow - 0.4, y = y + 0.35),
        size = day.size, family = font.family,
        color = days.col, fontface = font.style
      )

    p <- p + .calendar_theme_single(
      bg.col, mbg.col, weeknames.col, weeknames.size,
      week.number.col, week.number.size,
      title.size, title.col, subtitle.size, subtitle.col,
      legend.pos, margin, font.family, font.style
    )

  } else {
    # ── Yearly / multi-month facet view ───────────────────────────────────
    weekdays_ordered <- if (start == "S") c(weeknames[7], weeknames[1:6]) else weeknames
    weekdays_short   <- substring(weekdays_ordered, 1, 3)

    p <- ggplot2::ggplot(t2, ggplot2::aes(dow, woy + 1)) +
      ggplot2::geom_tile(ggplot2::aes(fill = fill),
                         color = col, linewidth = lwd, linetype = lty)

    p <- p +
      (if (is.character(t2$fill[1]))
        ggplot2::scale_fill_manual(values = special.col, na.value = "white", na.translate = FALSE)
       else
         ggplot2::scale_fill_gradient(low = low.col, high = special.col, na.value = "white"))

    # Event overlays (yearly view)
    if (has_events) {
      p <- .add_event_tiles_yearly(p, t2, events, palette, woy_axis = TRUE)
    }

    # Week-number labels on y-axis
    weeklabels <- if (!week.number) NULL else {
      wks <- sort(unique(t2$woy + 1))
      setNames(as.character(seq_along(wks)), wks)
    }

    p <- p +
      ggplot2::facet_wrap(~ monlabel, ncol = ncol, scales = "free") +
      ggplot2::ggtitle(title) +
      ggplot2::labs(subtitle = subtitle, fill = legend.title) +
      ggplot2::scale_x_continuous(
        expand = c(0.01, 0.01), position = "top",
        breaks = 0:6, labels = weekdays_short
      ) +
      ggplot2::scale_y_continuous(
        expand = c(0.01, 0.01), trans = "reverse",
        breaks = sort(unique(t2$woy + 1))
      ) +
      ggplot2::geom_text(
        data = t2,
        ggplot2::aes(label = as.integer(format(date, "%d"))),
        size = day.size, family = font.family,
        color = days.col, fontface = font.style
      ) +
      .calendar_theme_yearly(
        bg.col, mbg.col, months.col, months.size, months.pos,
        weeknames.col, weeknames.size,
        week.number.col, week.number.size,
        title.size, title.col, subtitle.size, subtitle.col,
        legend.pos, margin, font.family, font.style
      )
  }

  # Event legend
  if (has_events && legend.pos != "none") {
    p <- p + ggplot2::scale_fill_manual(
      values = palette, name = legend.title, na.value = low.col
    )
  }

  p
}


# ═══════════════════════════════════════════════════════════════════════════════
#  INTERNAL: week-block view  (.build_week_view)
# ═══════════════════════════════════════════════════════════════════════════════
#
#  Layout: each "block" shows exactly `weeks_per_block` consecutive weeks
#  (default 5) laid out as a 7-column grid.  Blocks become facets.
#  This gives a planning/Gantt feel where the horizontal axis is the
#  day of the week and vertical is the week row within the block.

.build_week_view <- function(
    t1, dates, mindate, maxdate,
    start, ncol,
    title, title.size, title.col,
    subtitle, subtitle.size, subtitle.col,
    weeknames, weeknames.col, weeknames.size,
    day.size, days.col,
    col, lwd, lty,
    low.col, special.col, gradient,
    week.number, week.number.col, week.number.size,
    font.family, font.style,
    legend.pos, legend.title,
    bg.col, margin,
    events, palette, has_events,
    weeks_per_block = 5L
) {
# browser()
  # Assign a block number to each week
  all_weeks  <- sort(unique(t1$woy))
  n_blocks   <- ceiling(length(all_weeks) / weeks_per_block)
  week_block <- setNames(
    rep(seq_len(n_blocks), each = weeks_per_block)[seq_along(all_weeks)],
    all_weeks
  )

  t2 <- t1 |>
    dplyr::mutate(
      block     = week_block[as.character(woy)],
      row_in_block = match(woy, all_weeks) - (block - 1L) * weeks_per_block
    )

  # Block labels: date range of each block
  block_ranges <- t2 |>
    dplyr::group_by(block) |>
    dplyr::summarise(
      lo = min(date), hi = max(date), .groups = "drop"
    ) |>
    dplyr::mutate(
      blabel = sprintf("%s – %s",
                       format(lo, "%d %b"), format(hi, "%d %b %Y"))
    )

  t2 <- dplyr::left_join(t2, block_ranges[, c("block","blabel")],
                         by = "block") |>
    dplyr::mutate(blabel = factor(blabel, levels = unique(blabel)))

  # Weekday header order
  wd_labels <- if (start == "S") c(weeknames[7], weeknames[1:6]) else weeknames
  wd_short  <- substring(wd_labels, 1, 3)

  # ── Event join ───────────────────────────────────────────────────────────
  t2 <- dplyr::mutate(t2, event_cat = NA_character_, event_label = "")

  if (has_events) {
    ev_join <- dplyr::select(events, date, category, label) |>
      dplyr::rename(event_cat = category, event_label = label)
    t2 <- dplyr::left_join(t2, ev_join, by = "date")
    # resolve potential column clash from dplyr join
    if ("event_cat.y" %in% names(t2)) {
      t2 <- dplyr::rename(t2,
                          event_cat   = event_cat.y,
                          event_label = event_label.y) |>
        dplyr::select(-event_cat.x, -event_label.x)
    }
  }

  # Tile fill: event category overrides special.days fills
  t2 <- dplyr::mutate(t2,
                      tile_fill = dplyr::if_else(!is.na(event_cat), event_cat,
                                                 dplyr::if_else(fill != 0, "highlight", NA_character_))
  )

  # Build colour scale
  fill_values <- c(highlight = special.col)
  if (has_events) fill_values <- c(fill_values, palette)

  # ── Plot ─────────────────────────────────────────────────────────────────
  if (missing(title)) {
    title <- sprintf("%s – %s", format(mindate,"%b %Y"), format(maxdate,"%b %Y"))
  }

  p <- ggplot2::ggplot(t2,
                       ggplot2::aes(x = dow, y = row_in_block)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = tile_fill),
      color = col, linewidth = lwd, linetype = lty
    ) +
    ggplot2::scale_fill_manual(
      values    = fill_values,
      na.value  = low.col,
      name      = legend.title,
      na.translate = FALSE
    ) +
    # Day numbers (top-left of cell)
    ggplot2::geom_text(
      ggplot2::aes(
        label = as.integer(format(date, "%d")),
        x = dow - 0.4, y = row_in_block + 0.35
      ),
      size = day.size, color = days.col,
      family = font.family, fontface = font.style, hjust = 0
    ) +
    # Month label when it changes mid-block
    ggplot2::geom_text(
      data = t2 |>
        dplyr::filter(as.integer(format(date, "%d")) == 1L),
      ggplot2::aes(
        label = toupper(format(date, "%b")),
        x = dow + 0.42, y = row_in_block + 0.38
      ),
      size = day.size * 0.7, color = weeknames.col,
      family = font.family, fontface = "bold", hjust = 1
    ) +
    # Event label inside tile
    ggplot2::geom_text(
      ggplot2::aes(label = event_label),
      size = day.size * 0.75, color = "white",
      family = font.family, vjust = 0, nudge_y = -0.15,
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(~ blabel, ncol = ncol, scales = "free_y") +
    ggplot2::scale_x_continuous(
      breaks = 0:6, labels = wd_short,
      position = "top", expand = c(0.01, 0.01)
    ) +
    ggplot2::scale_y_continuous(
      trans = "reverse",
      breaks = seq_len(weeks_per_block),
      labels = if (week.number) {
        # show ISO week numbers
        t2 |>
          dplyr::distinct(block, row_in_block, woy) |>
          dplyr::arrange(block, row_in_block) |>
          dplyr::pull(woy) |>
          unique() |>
          (\(x) x[seq_len(weeks_per_block)])()
      } else {
        rep("", weeks_per_block)
      },
      expand = c(0.05, 0.05)
    ) +
    ggplot2::ggtitle(title) +
    ggplot2::labs(subtitle = subtitle) +
    ggplot2::theme(
      panel.background  = ggplot2::element_rect(fill = NA, color = NA),
      strip.background  = ggplot2::element_rect(fill = bg.col, color = bg.col),
      plot.background   = ggplot2::element_rect(fill = bg.col),
      panel.grid        = ggplot2::element_line(colour = bg.col),
      strip.text.x      = ggplot2::element_text(
        hjust = 0, face = "bold",
        color = "gray30", size = weeknames.size * 1.5
      ),
      axis.ticks        = ggplot2::element_blank(),
      axis.title        = ggplot2::element_blank(),
      axis.text.x       = ggplot2::element_text(
        colour = weeknames.col, size = weeknames.size * 2.25
      ),
      axis.text.y       = ggplot2::element_text(
        colour = week.number.col, size = week.number.size
      ),
      plot.title        = ggplot2::element_text(
        hjust = 0.5, size = title.size, colour = title.col
      ),
      plot.subtitle     = ggplot2::element_text(
        hjust = 0.5, face = "italic",
        colour = subtitle.col, size = subtitle.size
      ),
      legend.position   = legend.pos,
      plot.margin       = ggplot2::unit(
        c(margin, 0.5*margin, margin, 0.5*margin), "cm"
      ),
      text = ggplot2::element_text(
        family = font.family, face = font.style
      )
    )

  p
}


# ═══════════════════════════════════════════════════════════════════════════════
#  INTERNAL: event helpers
# ═══════════════════════════════════════════════════════════════════════════════

# Overlay coloured tiles for events on a single-month plot.
.add_event_tiles_single <- function(p, t2, events) {
  ev <- dplyr::inner_join(
    t2, events, by = "date"
  )
  if (nrow(ev) == 0) return(p)
  p + ggplot2::geom_tile(
    data = ev,
    ggplot2::aes(fill = category),
    color = NA, alpha = 0.7
  ) +
    ggplot2::geom_text(
      data = ev,
      ggplot2::aes(label = label),
      size = 2.5, color = "white", vjust = 0.5
    )
}

# Overlay coloured tiles for events on a yearly/multi-month facet plot.
.add_event_tiles_yearly <- function(p, t2, events, palette, woy_axis = TRUE) {
  ev <- dplyr::inner_join(t2, events, by = "date")
  if (nrow(ev) == 0) return(p)
  y_var <- if (woy_axis) ggplot2::aes(dow, woy + 1, fill = category) else
    ggplot2::aes(dow, y,        fill = category)
  p + ggplot2::geom_tile(
    data = ev, mapping = y_var,
    color = NA, alpha = 0.75
  ) +
    ggplot2::scale_fill_manual(values = palette, na.value = "white") +
    ggplot2::geom_text(
      data = ev,
      ggplot2::aes(label = label),
      size = 2, color = "white"
    )
}


# ═══════════════════════════════════════════════════════════════════════════════
#  INTERNAL: theme helpers
# ═══════════════════════════════════════════════════════════════════════════════

.calendar_theme_single <- function(
    bg.col, mbg.col, weeknames.col, weeknames.size,
    week.number.col, week.number.size,
    title.size, title.col, subtitle.size, subtitle.col,
    legend.pos, margin, font.family, font.style
) {
  ggplot2::theme(
    panel.background  = ggplot2::element_rect(fill = NA, color = NA),
    strip.background  = ggplot2::element_rect(fill = NA, color = NA),
    plot.background   = ggplot2::element_rect(fill = bg.col),
    panel.grid        = ggplot2::element_line(colour = bg.col),
    strip.text.x      = ggplot2::element_text(hjust = 0, face = "bold"),
    axis.ticks        = ggplot2::element_blank(),
    axis.title        = ggplot2::element_blank(),
    axis.text.y       = ggplot2::element_text(colour = week.number.col,
                                              size   = week.number.size),
    axis.text.x       = ggplot2::element_blank(),
    plot.title        = ggplot2::element_text(hjust = 0.5, size  = title.size,
                                              colour = title.col),
    plot.subtitle     = ggplot2::element_text(hjust = 0.5, face  = "italic",
                                              colour = subtitle.col,
                                              size   = subtitle.size),
    legend.position   = legend.pos,
    plot.margin       = ggplot2::unit(c(margin, 0.5*margin,
                                        margin, 0.5*margin), "cm"),
    text = ggplot2::element_text(family = font.family, face = font.style)
  )
}

.calendar_theme_yearly <- function(
    bg.col, mbg.col, months.col, months.size, months.pos,
    weeknames.col, weeknames.size,
    week.number.col, week.number.size,
    title.size, title.col, subtitle.size, subtitle.col,
    legend.pos, margin, font.family, font.style
) {
  ggplot2::theme(
    panel.background  = ggplot2::element_rect(fill = NA, color = NA),
    strip.background  = ggplot2::element_rect(fill = mbg.col, color = mbg.col),
    plot.background   = ggplot2::element_rect(fill = bg.col),
    panel.grid        = ggplot2::element_line(colour = bg.col),
    strip.text.x      = ggplot2::element_text(hjust  = months.pos,
                                              color  = months.col,
                                              size   = months.size),
    axis.ticks        = ggplot2::element_blank(),
    axis.title        = ggplot2::element_blank(),
    axis.text.y       = ggplot2::element_text(colour = week.number.col,
                                              size   = week.number.size),
    axis.text.x       = ggplot2::element_text(colour = weeknames.col,
                                              size   = weeknames.size * 2.25),
    plot.title        = ggplot2::element_text(hjust = 0.5, size  = title.size,
                                              colour = title.col),
    plot.subtitle     = ggplot2::element_text(hjust = 0.5, face  = "italic",
                                              colour = subtitle.col,
                                              size   = subtitle.size),
    legend.position   = legend.pos,
    plot.margin       = ggplot2::unit(c(margin, 0.5*margin,
                                        margin, 0.5*margin), "cm"),
    text = ggplot2::element_text(family = font.family, face = font.style),
    strip.placement   = "outside"
  )
}

# Null-coalescing operator (avoids importing rlang just for this)
`%||%` <- function(a, b) if (!is.null(a)) a else b
