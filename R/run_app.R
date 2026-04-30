#' Run the Schedule Shiny Application
#'
#' Launches the Shiny app for printing work schedules from shared .ics calendar files.
#'
#' @return This function normally does not return; it invokes [shiny::runApp()]
#'   to start the app. It will return the value returned by the app's server
#'   function if the app is programmatically exited with a value.
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
#'
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "PrintSchedule")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `PrintSchedule`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
