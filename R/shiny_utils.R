#' Color Select Input
#'
#' A custom Shiny select input that displays color palette previews as
#' swatches alongside palette names. Useful for letting users choose
#' color schemes in a visual way.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param choices List of values to select from. Can be a named vector
#'   where names are display labels.
#' @param selected The initially selected value (if not specified then
#'   defaults to the first value).
#' @param previews Number of color swatches to display (default: 4).
#' @param ... Additional arguments passed to [shiny::selectizeInput()].
#' @param placeholder A string to show as placeholder (not currently used).
#'
#' @return A selectize input control with custom rendering for color previews.
#'
#' @details
#' The function generates color previews using [generate_colors()] and
#' displays them as small colored boxes next to palette names in the
#' dropdown. When a palette is selected, the swatches are shown inline
#' with the selected value.
#'
#' @examples
#' \dontrun{
#' colorSelectInput(
#'   inputId = "palette",
#'   label = "Choose a color palette:",
#'   choices = c("Viridis" = "viridis", "Spectral" = "Spectral")
#' )
#' }
#'
#' @import shiny
#' @importFrom stats setNames
#' @export
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
