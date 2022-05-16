#' Launch Shiny App
#'
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
app <- function(...) {
  appDir <- system.file(paste0('apps/BlockmodelingGUI.R'), package = 'BlockmodelingGUI')
  if (appDir == '') stop('Could not find the app Try re-installing BlockmodelingGUI!', call. = FALSE)
  shiny::runApp(appDir, ...)
}
